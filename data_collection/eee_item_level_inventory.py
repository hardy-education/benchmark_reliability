#!/usr/bin/env python3
"""
Search the Every Eval Ever (EEE) datastore for evaluations with item-level results.

This script uses ONLY the Python standard library. It does not require
huggingface_hub, pandas, requests, or any other package, so it also avoids the
"termios" import issue that can occur in some environments.

What it does
------------
1. Reads EEE's authoritative flat/latest_manifest.json.
2. Follows the manifest to the current entries.jsonl snapshot.
3. Keeps only records with instance_level_available == true.
4. Streams every corresponding *_samples.jsonl file from Hugging Face.
5. Groups the item-level rows by:
       EEE benchmark/collection + evaluation_name
6. Counts:
       - distinct model_id values
       - distinct sample_id values ("items")
7. Writes:
       eee_item_level_evaluations.csv   <-- main requested output
       eee_item_level_benchmarks.csv    <-- benchmark-level convenience summary
       eee_item_level_failures.csv      <-- only if any files could not be read

Important
---------
"n_items" is the number of distinct sample_id values observed across all
item-level files for that benchmark/evaluation. "n_models" is the number of
distinct model_id values with at least one item-level row.

Because this scans the source-of-truth item-level JSONL files, it can download
a substantial amount of data. The script prints the total declared size before
starting.

Usage
-----
    python eee_item_level_inventory.py

Optional:
    python eee_item_level_inventory.py --workers 8
    python eee_item_level_inventory.py --output-dir ./eee_output
    python eee_item_level_inventory.py --limit 20   # useful for a quick test
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import os
import sys
import time
import csv
from collections import defaultdict
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path
from typing import Any
from urllib.error import HTTPError, URLError
from urllib.parse import quote
from urllib.request import Request, urlopen


REPO_BASE = "https://huggingface.co/datasets/evaleval/EEE_datastore/resolve/main/"
MANIFEST_PATH = "flat/latest_manifest.json"
USER_AGENT = "EEE-item-level-inventory/1.0"


def load_existing_benchmark_summary(path: Path) -> list[dict]:
    """Load an existing eee_item_level_benchmarks.csv file."""

    rows = []

    with path.open("r", encoding="utf-8", newline="") as f:
        reader = csv.DictReader(f)

        required = {"benchmark", "n_items", "n_models"}
        missing = required - set(reader.fieldnames or [])

        if missing:
            raise ValueError(
                f"{path} is missing required columns: {sorted(missing)}"
            )

        for row in reader:
            rows.append(
                {
                    "benchmark": row["benchmark"],
                    "n_items": int(row["n_items"]),
                    "n_models": int(row["n_models"]),
                }
            )

    return rows

def hf_url(path: str) -> str:
    """Build a direct Hugging Face download URL for a repository-relative path."""
    return REPO_BASE + quote(path.lstrip("/"), safe="/")


def open_url(url: str, *, timeout: int = 120, retries: int = 5):
    """Open a URL with retries for transient HTTP/network failures."""
    last_error = None

    for attempt in range(retries):
        try:
            req = Request(url, headers={"User-Agent": USER_AGENT})
            return urlopen(req, timeout=timeout)
        except HTTPError as exc:
            last_error = exc
            # Retry rate limits and transient server errors.
            if exc.code == 429 or 500 <= exc.code < 600:
                wait = min(2 ** attempt, 16)
                time.sleep(wait)
                continue
            raise
        except (URLError, TimeoutError, ConnectionError) as exc:
            last_error = exc
            wait = min(2 ** attempt, 16)
            time.sleep(wait)

    raise RuntimeError(f"Failed after {retries} attempts: {url}\n{last_error}")


def read_json_url(url: str) -> dict[str, Any]:
    with open_url(url) as response:
        return json.load(response)


def normalize_id(value: Any) -> str:
    """
    Convert sample/model/evaluation IDs into stable strings.

    EEE normally uses strings or integers, but this also handles unusual JSON
    values defensively.
    """
    if isinstance(value, (str, int, float, bool)) or value is None:
        return str(value)
    return json.dumps(value, sort_keys=True, ensure_ascii=False, separators=(",", ":"))


def verify_and_load_entries(manifest: dict[str, Any]) -> list[dict[str, Any]]:
    """
    Download the current entries.jsonl, verify its byte size and SHA-256 when
    those fields are present in the manifest, and parse it.
    """
    entries_path = manifest["entries_path"]
    entries_url = hf_url(entries_path)

    print(f"Downloading EEE snapshot index:\n  {entries_path}")

    with open_url(entries_url) as response:
        raw = response.read()

    expected_size = manifest.get("entries_size_bytes")
    if expected_size is not None and len(raw) != int(expected_size):
        raise RuntimeError(
            f"entries.jsonl size mismatch: got {len(raw):,} bytes, "
            f"expected {int(expected_size):,}."
        )

    expected_sha = manifest.get("entries_sha256")
    if expected_sha:
        actual_sha = hashlib.sha256(raw).hexdigest()
        if actual_sha.lower() != str(expected_sha).lower():
            raise RuntimeError(
                "entries.jsonl SHA-256 mismatch.\n"
                f"  got:      {actual_sha}\n"
                f"  expected: {expected_sha}"
            )

    entries = []
    for line_number, line in enumerate(raw.splitlines(), start=1):
        if not line.strip():
            continue
        try:
            entries.append(json.loads(line))
        except json.JSONDecodeError as exc:
            raise RuntimeError(
                f"Could not parse entries.jsonl line {line_number}: {exc}"
            ) from exc

    return entries


def scan_sample_file(entry: dict[str, Any]) -> dict[str, Any]:
    """
    Stream one EEE *_samples.jsonl file.

    Returns local sets that can be merged into the global inventory.
    """
    benchmark = normalize_id(entry.get("benchmark", ""))
    path = entry.get("instance_level_path")

    if not path:
        raise ValueError("instance_level_available=true but instance_level_path is missing")

    url = hf_url(path)

    # evaluation_name -> set(model_id)
    models_by_eval: dict[str, set[str]] = defaultdict(set)

    # evaluation_name -> set(sample_id)
    items_by_eval: dict[str, set[str]] = defaultdict(set)

    rows = 0
    missing_sample_id = 0
    missing_model_id = 0
    missing_eval_name = 0

    with open_url(url) as response:
        for line_number, raw_line in enumerate(response, start=1):
            if not raw_line.strip():
                continue

            rows += 1
            try:
                row = json.loads(raw_line)
            except json.JSONDecodeError as exc:
                raise RuntimeError(
                    f"Invalid JSON in {path} at line {line_number}: {exc}"
                ) from exc

            evaluation_name = row.get("evaluation_name")
            model_id = row.get("model_id")
            sample_id = row.get("sample_id")

            if evaluation_name is None:
                missing_eval_name += 1
                # Without evaluation_name we cannot group this row correctly.
                continue

            evaluation_name = normalize_id(evaluation_name)

            if model_id is None:
                missing_model_id += 1
            else:
                models_by_eval[evaluation_name].add(normalize_id(model_id))

            if sample_id is None:
                missing_sample_id += 1
            else:
                items_by_eval[evaluation_name].add(normalize_id(sample_id))

    return {
        "benchmark": benchmark,
        "path": path,
        "rows": rows,
        "models_by_eval": models_by_eval,
        "items_by_eval": items_by_eval,
        "missing_sample_id": missing_sample_id,
        "missing_model_id": missing_model_id,
        "missing_eval_name": missing_eval_name,
    }


def human_bytes(n: int) -> str:
    value = float(n)
    for unit in ["B", "KiB", "MiB", "GiB", "TiB"]:
        if value < 1024 or unit == "TiB":
            return f"{value:,.2f} {unit}"
        value /= 1024
    return f"{n:,} B"


def write_csv(path: Path, rows: list[dict[str, Any]], fieldnames: list[str]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def print_table(rows: list[dict[str, Any]], max_rows: int | None = None) -> None:
    """Print the main four-column result in a readable fixed-width table."""
    display_rows = rows if max_rows is None else rows[:max_rows]

    headers = ["benchmark", "evaluation_name", "n_items", "n_models"]
    widths = {h: len(h) for h in headers}

    for row in display_rows:
        for h in headers:
            widths[h] = max(widths[h], len(str(row[h])))

    # Avoid absurdly wide terminal output.
    widths["benchmark"] = min(widths["benchmark"], 35)
    widths["evaluation_name"] = min(widths["evaluation_name"], 55)

    def clip(value: Any, width: int) -> str:
        text = str(value)
        if len(text) <= width:
            return text
        if width <= 3:
            return text[:width]
        return text[: width - 3] + "..."

    header_line = " | ".join(clip(h, widths[h]).ljust(widths[h]) for h in headers)
    separator = "-+-".join("-" * widths[h] for h in headers)

    print("\n" + header_line)
    print(separator)
    for row in display_rows:
        print(
            " | ".join(
                clip(row[h], widths[h]).ljust(widths[h])
                if h in ("benchmark", "evaluation_name")
                else str(row[h]).rjust(widths[h])
                for h in headers
            )
        )


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Inventory EEE evaluations with item-level results."
    )

    parser.add_argument(
        "--workers",
        type=int,
        default=8,
        help="Number of parallel sample-file downloads (default: 8).",
    )

    parser.add_argument(
        "--output-dir",
        type=Path,
        default=Path("."),
        help="Directory for input/output CSV files (default: current directory).",
    )

    parser.add_argument(
        "--limit",
        type=int,
        default=None,
        help=(
            "Scan only the first N item-level files. Useful for testing. "
            "Omit for the complete datastore."
        ),
    )

    parser.add_argument(
        "--min-models",
        type=int,
        default=5,
        help="Minimum number of models required to keep a benchmark (default: 5).",
    )

    parser.add_argument(
        "--min-items",
        type=int,
        default=100,
        help="Minimum number of items required to keep a benchmark (default: 100).",
    )

    parser.add_argument(
        "--force-rescan",
        action="store_true",
        help=(
            "Ignore an existing eee_item_level_benchmarks.csv "
            "and rescan the EEE datastore."
        ),
    )

    args = parser.parse_args()

    if args.workers < 1:
        parser.error("--workers must be >= 1")

    if args.limit is not None and args.limit < 1:
        parser.error("--limit must be >= 1")

    if args.min_models < 1:
        parser.error("--min-models must be >= 1")

    if args.min_items < 1:
        parser.error("--min-items must be >= 1")

    # Make sure output directory exists
    args.output_dir.mkdir(parents=True, exist_ok=True)

    # ================================================================
    # FIRST: check whether we already have a benchmark summary
    # ================================================================

    existing_summary = (
        args.output_dir / "eee_item_level_benchmarks.csv"
    )

    filtered_output = (
        args.output_dir / "eee_item_level_benchmarks_filtered.csv"
    )

    if existing_summary.exists() and not args.force_rescan:

        print(
            f"Found existing benchmark summary:\n"
            f"  {existing_summary.resolve()}"
        )

        print(
            "\nUsing this file instead of rescanning the EEE datastore."
        )

        # ------------------------------------------------------------
        # Load existing CSV
        # ------------------------------------------------------------

        benchmark_rows = []

        with existing_summary.open(
            "r",
            encoding="utf-8",
            newline=""
        ) as f:

            reader = csv.DictReader(f)

            required_columns = {
                "benchmark",
                "n_items",
                "n_models",
            }

            available_columns = set(reader.fieldnames or [])

            missing_columns = (
                required_columns - available_columns
            )

            if missing_columns:
                raise ValueError(
                    f"{existing_summary} is missing required columns: "
                    f"{sorted(missing_columns)}"
                )

            for row in reader:

                benchmark_rows.append(
                    {
                        "benchmark": row["benchmark"],
                        "n_items": int(row["n_items"]),
                        "n_models": int(row["n_models"]),
                    }
                )

        # ------------------------------------------------------------
        # Filter
        # ------------------------------------------------------------

        filtered_rows = [
            row
            for row in benchmark_rows
            if (
                row["n_models"] >= args.min_models
                and row["n_items"] >= args.min_items
            )
        ]

        # Sort most useful benchmarks first
        filtered_rows.sort(
            key=lambda r: (
                -r["n_models"],
                -r["n_items"],
                r["benchmark"].lower(),
            )
        )

        # ------------------------------------------------------------
        # Save filtered CSV
        # ------------------------------------------------------------

        write_csv(
            filtered_output,
            filtered_rows,
            [
                "benchmark",
                "n_items",
                "n_models",
            ],
        )

        # ------------------------------------------------------------
        # Print results
        # ------------------------------------------------------------

        print(
            f"\nTotal benchmarks in existing file: "
            f"{len(benchmark_rows):,}"
        )

        print(
            f"Benchmarks with >= {args.min_models} models "
            f"and >= {args.min_items} items: "
            f"{len(filtered_rows):,}"
        )

        print(
            f"\nSaved filtered results to:\n"
            f"  {filtered_output.resolve()}"
        )

        print(
            "\n"
            f"{'benchmark':50} "
            f"{'n_items':>10} "
            f"{'n_models':>10}"
        )

        print("-" * 74)

        for row in filtered_rows:
            print(
                f"{row['benchmark'][:50]:50} "
                f"{row['n_items']:>10,} "
                f"{row['n_models']:>10,}"
            )

        return 0

    # ================================================================
    # NO EXISTING SUMMARY:
    # run the full EEE scan
    # ================================================================

    if args.force_rescan:
        print(
            "Force-rescan requested. "
            "Ignoring any existing benchmark summary.\n"
        )
    else:
        print(
            "No existing eee_item_level_benchmarks.csv found."
        )

        print(
            "Scanning the EEE datastore to create it.\n"
        )

    # ----------------------------------------------------------------
    # 1. Read the authoritative EEE manifest + entries snapshot
    # ----------------------------------------------------------------

    manifest_url = hf_url(MANIFEST_PATH)

    print(
        f"Reading EEE manifest:\n"
        f"  {manifest_url}"
    )

    manifest = read_json_url(manifest_url)

    created_at = manifest.get(
        "created_at",
        "unknown",
    )

    print(
        f"EEE snapshot created_at: {created_at}"
    )

    entries = verify_and_load_entries(manifest)

    # ----------------------------------------------------------------
    # 2. Keep only records with item-level companions
    # ----------------------------------------------------------------

    item_entries = [
        e
        for e in entries
        if (
            e.get("instance_level_available") is True
            and e.get("instance_level_path")
        )
    ]

    # Sort for deterministic output
    item_entries.sort(
        key=lambda e: (
            str(e.get("benchmark", "")),
            str(
                e.get(
                    "instance_level_path",
                    "",
                )
            ),
        )
    )

    if args.limit is not None:
        item_entries = item_entries[: args.limit]

    declared_bytes = sum(
        int(
            e.get(
                "instance_level_size_bytes"
            )
            or 0
        )
        for e in item_entries
    )

    print(
        f"Aggregate records in snapshot: "
        f"{len(entries):,}"
    )

    print(
        f"Item-level companion files to scan: "
        f"{len(item_entries):,}"
    )

    if declared_bytes:
        print(
            f"Declared item-level download size: "
            f"{human_bytes(declared_bytes)}"
        )

    print(
        "\nScanning item-level files. "
        "This may take a while."
    )

    # ----------------------------------------------------------------
    # 3. Scan all item-level files
    # ----------------------------------------------------------------

    eval_models = defaultdict(set)
    eval_items = defaultdict(set)

    benchmark_models = defaultdict(set)
    benchmark_items = defaultdict(set)

    failures = []

    total_rows = 0
    total_missing_sample = 0
    total_missing_model = 0
    total_missing_eval = 0

    completed = 0
    start = time.time()

    with ThreadPoolExecutor(
        max_workers=args.workers
    ) as executor:

        futures = {
            executor.submit(
                scan_sample_file,
                entry,
            ): entry
            for entry in item_entries
        }

        for future in as_completed(futures):

            entry = futures[future]

            completed += 1

            try:
                result = future.result()

            except Exception as exc:

                failures.append(
                    {
                        "benchmark": str(
                            entry.get(
                                "benchmark",
                                "",
                            )
                        ),
                        "instance_level_path": str(
                            entry.get(
                                "instance_level_path",
                                "",
                            )
                        ),
                        "error": (
                            f"{type(exc).__name__}: "
                            f"{exc}"
                        ),
                    }
                )

            else:

                benchmark = result["benchmark"]

                total_rows += result["rows"]

                total_missing_sample += (
                    result["missing_sample_id"]
                )

                total_missing_model += (
                    result["missing_model_id"]
                )

                total_missing_eval += (
                    result["missing_eval_name"]
                )

                evaluation_names = (
                    set(
                        result[
                            "models_by_eval"
                        ]
                    )
                    |
                    set(
                        result[
                            "items_by_eval"
                        ]
                    )
                )

                for evaluation_name in evaluation_names:

                    key = (
                        benchmark,
                        evaluation_name,
                    )

                    models = (
                        result[
                            "models_by_eval"
                        ].get(
                            evaluation_name,
                            set(),
                        )
                    )

                    items = (
                        result[
                            "items_by_eval"
                        ].get(
                            evaluation_name,
                            set(),
                        )
                    )

                    eval_models[key].update(
                        models
                    )

                    eval_items[key].update(
                        items
                    )

                    benchmark_models[
                        benchmark
                    ].update(
                        models
                    )

                    benchmark_items[
                        benchmark
                    ].update(
                        (
                            evaluation_name,
                            sample_id,
                        )
                        for sample_id in items
                    )

            # Progress report
            if (
                completed == 1
                or completed % 25 == 0
                or completed == len(item_entries)
            ):

                elapsed = (
                    time.time() - start
                )

                rate = (
                    completed / elapsed
                    if elapsed > 0
                    else 0
                )

                print(
                    f"  {completed:,}/"
                    f"{len(item_entries):,} files "
                    f"({rate:.2f} files/s), "
                    f"failures="
                    f"{len(failures):,}",
                    flush=True,
                )

    # ----------------------------------------------------------------
    # 4. Build evaluation-level table
    # ----------------------------------------------------------------

    evaluation_rows = []

    all_eval_keys = (
        set(eval_models)
        |
        set(eval_items)
    )

    for (
        benchmark,
        evaluation_name,
    ) in all_eval_keys:

        evaluation_rows.append(
            {
                "benchmark": benchmark,
                "evaluation_name": (
                    evaluation_name
                ),
                "n_items": len(
                    eval_items[
                        (
                            benchmark,
                            evaluation_name,
                        )
                    ]
                ),
                "n_models": len(
                    eval_models[
                        (
                            benchmark,
                            evaluation_name,
                        )
                    ]
                ),
            }
        )

    evaluation_rows.sort(
        key=lambda r: (
            -r["n_models"],
            -r["n_items"],
            r["benchmark"].lower(),
            r["evaluation_name"].lower(),
        )
    )

    out_eval = (
        args.output_dir
        /
        "eee_item_level_evaluations.csv"
    )

    write_csv(
        out_eval,
        evaluation_rows,
        [
            "benchmark",
            "evaluation_name",
            "n_items",
            "n_models",
        ],
    )

    # ----------------------------------------------------------------
    # 5. Build benchmark-level table
    # ----------------------------------------------------------------

    benchmark_rows = []

    all_benchmarks = (
        set(benchmark_models)
        |
        set(benchmark_items)
    )

    for benchmark in all_benchmarks:

        benchmark_rows.append(
            {
                "benchmark": benchmark,
                "n_items": len(
                    benchmark_items[
                        benchmark
                    ]
                ),
                "n_models": len(
                    benchmark_models[
                        benchmark
                    ]
                ),
            }
        )

    benchmark_rows.sort(
        key=lambda r: (
            -r["n_models"],
            -r["n_items"],
            r["benchmark"].lower(),
        )
    )

    out_bench = (
        args.output_dir
        /
        "eee_item_level_benchmarks.csv"
    )

    write_csv(
        out_bench,
        benchmark_rows,
        [
            "benchmark",
            "n_items",
            "n_models",
        ],
    )

    # ----------------------------------------------------------------
    # 6. Create filtered benchmark table
    # ----------------------------------------------------------------

    filtered_rows = [
        row
        for row in benchmark_rows
        if (
            row["n_models"]
            >= args.min_models
            and
            row["n_items"]
            >= args.min_items
        )
    ]

    filtered_rows.sort(
        key=lambda r: (
            -r["n_models"],
            -r["n_items"],
            r["benchmark"].lower(),
        )
    )

    write_csv(
        filtered_output,
        filtered_rows,
        [
            "benchmark",
            "n_items",
            "n_models",
        ],
    )

    # ----------------------------------------------------------------
    # 7. Save failures
    # ----------------------------------------------------------------

    out_fail = (
        args.output_dir
        /
        "eee_item_level_failures.csv"
    )

    if failures:

        write_csv(
            out_fail,
            failures,
            [
                "benchmark",
                "instance_level_path",
                "error",
            ],
        )

    elif out_fail.exists():

        out_fail.unlink()

    # ----------------------------------------------------------------
    # 8. Print summary
    # ----------------------------------------------------------------

    print("\nDone.")

    print(
        f"Item-level rows scanned: "
        f"{total_rows:,}"
    )

    print(
        f"Benchmark/evaluation combinations found: "
        f"{len(evaluation_rows):,}"
    )

    print(
        f"EEE benchmark collections found: "
        f"{len(benchmark_rows):,}"
    )

    print(
        f"Files that failed to scan: "
        f"{len(failures):,}"
    )

    if (
        total_missing_eval
        or total_missing_model
        or total_missing_sample
    ):

        print(
            "Rows with missing required identifiers: "
            f"evaluation_name="
            f"{total_missing_eval:,}, "
            f"model_id="
            f"{total_missing_model:,}, "
            f"sample_id="
            f"{total_missing_sample:,}"
        )

    print(
        f"\nFull benchmark summary:\n"
        f"  {out_bench.resolve()}"
    )

    print(
        f"\nFiltered benchmark summary "
        f"(>= {args.min_models} models, "
        f">= {args.min_items} items):\n"
        f"  {filtered_output.resolve()}"
    )

    print(
        f"\nKept "
        f"{len(filtered_rows):,} of "
        f"{len(benchmark_rows):,} benchmarks."
    )

    if failures:
        print(
            f"\nFailures:\n"
            f"  {out_fail.resolve()}"
        )

    print(
        "\n"
        f"{'benchmark':50} "
        f"{'n_items':>10} "
        f"{'n_models':>10}"
    )

    print("-" * 74)

    for row in filtered_rows:

        print(
            f"{row['benchmark'][:50]:50} "
            f"{row['n_items']:>10,} "
            f"{row['n_models']:>10,}"
        )

    return 0 if not failures else 2


if __name__ == "__main__":
    try:
        raise SystemExit(main())

    except KeyboardInterrupt:
        print(
            "\nInterrupted by user.",
            file=sys.stderr,
        )

        raise SystemExit(130)
