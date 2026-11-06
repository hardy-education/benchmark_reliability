#!/usr/bin/env python3
"""
Build a combined item-level EEE dataset for benchmarks listed in
`eee_item_level_benchmarks_filtered.csv`.

Uses only the Python standard library.

Outputs
-------
1. eee_item_level_filtered.csv
   Analysis-friendly flat table with one row per EEE item-level result.

2. eee_item_level_filtered.jsonl
   Full-fidelity source rows, augmented with `_eee_benchmark` and
   `_eee_source_path`.

3. eee_item_level_dataset_summary.csv
   Observed items/models/rows/source files per benchmark, compared with the
   counts in the filtered benchmark summary.

4. eee_item_level_download_failures.csv
   Only created if any source files fail to download or parse.

The script does NOT silently deduplicate rows. If EEE contains more than one
row for the same benchmark/evaluation/model/sample key, all source rows are
retained and the source path is included.

Usage
-----
    python eee_build_filtered_item_dataset.py

Optional
--------
    python eee_build_filtered_item_dataset.py --input my_filtered.csv
    python eee_build_filtered_item_dataset.py --output-dir ./eee_data
    python eee_build_filtered_item_dataset.py --workers 8
    python eee_build_filtered_item_dataset.py --csv-only
    python eee_build_filtered_item_dataset.py --jsonl-only
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import sys
import time
from collections import defaultdict
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path
from typing import Any
from urllib.error import HTTPError, URLError
from urllib.parse import quote
from urllib.request import Request, urlopen


REPO_BASE = "https://huggingface.co/datasets/evaleval/EEE_datastore/resolve/main/"
MANIFEST_PATH = "flat/latest_manifest.json"
USER_AGENT = "EEE-filtered-item-level-builder/1.0"


# -----------------------------------------------------------------------------
# Network helpers
# -----------------------------------------------------------------------------

def hf_url(path: str) -> str:
    return REPO_BASE + quote(path.lstrip("/"), safe="/")


def open_url(url: str, timeout: int = 120, retries: int = 5):
    last_error = None
    for attempt in range(retries):
        try:
            req = Request(url, headers={"User-Agent": USER_AGENT})
            return urlopen(req, timeout=timeout)
        except HTTPError as exc:
            last_error = exc
            if exc.code == 429 or 500 <= exc.code < 600:
                time.sleep(min(2 ** attempt, 16))
                continue
            raise
        except (URLError, TimeoutError, ConnectionError) as exc:
            last_error = exc
            time.sleep(min(2 ** attempt, 16))
    raise RuntimeError(f"Failed after {retries} attempts: {url}\n{last_error}")


def read_json_url(url: str) -> dict[str, Any]:
    with open_url(url) as response:
        return json.load(response)


# -----------------------------------------------------------------------------
# EEE index
# -----------------------------------------------------------------------------

def load_entries(manifest: dict[str, Any]) -> list[dict[str, Any]]:
    path = manifest["entries_path"]
    print(f"Downloading EEE snapshot index:\n  {path}")

    with open_url(hf_url(path)) as response:
        raw = response.read()

    expected_size = manifest.get("entries_size_bytes")
    if expected_size is not None and len(raw) != int(expected_size):
        raise RuntimeError(
            f"entries.jsonl size mismatch: got {len(raw):,}, "
            f"expected {int(expected_size):,}"
        )

    expected_sha = manifest.get("entries_sha256")
    if expected_sha:
        actual_sha = hashlib.sha256(raw).hexdigest()
        if actual_sha.lower() != str(expected_sha).lower():
            raise RuntimeError("entries.jsonl SHA-256 mismatch")

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


# -----------------------------------------------------------------------------
# Filtered benchmark list
# -----------------------------------------------------------------------------

def load_filtered_benchmarks(path: Path) -> dict[str, dict[str, int]]:
    if not path.exists():
        raise FileNotFoundError(f"Input file not found: {path.resolve()}")

    selected = {}

    with path.open("r", encoding="utf-8", newline="") as f:
        reader = csv.DictReader(f)
        required = {"benchmark", "n_items", "n_models"}
        missing = required - set(reader.fieldnames or [])
        if missing:
            raise ValueError(f"Missing required columns: {sorted(missing)}")

        for row in reader:
            benchmark = row["benchmark"].strip()
            if benchmark:
                selected[benchmark] = {
                    "expected_n_items": int(row["n_items"]),
                    "expected_n_models": int(row["n_models"]),
                }

    return selected


# -----------------------------------------------------------------------------
# Flatten common EEE item-level fields
# -----------------------------------------------------------------------------

def nested_get(obj: Any, *keys: str) -> Any:
    current = obj
    for key in keys:
        if not isinstance(current, dict):
            return None
        current = current.get(key)
    return current


def first_value(row: dict[str, Any], paths: list[tuple[str, ...]]) -> Any:
    for path in paths:
        value = nested_get(row, *path)
        if value is not None:
            return value
    return None


def text_value(value: Any) -> str:
    if value is None:
        return ""
    if isinstance(value, bool):
        return "true" if value else "false"
    if isinstance(value, (str, int, float)):
        return str(value)
    return json.dumps(value, ensure_ascii=False, sort_keys=True, separators=(",", ":"))


def json_value(value: Any) -> str:
    if value is None:
        return ""
    return json.dumps(value, ensure_ascii=False, sort_keys=True, separators=(",", ":"))


def flatten_row(row: dict[str, Any], entry: dict[str, Any]) -> dict[str, str]:
    evaluation_name = (
        row.get("evaluation_name")
        or entry.get("evaluation_name")
        or entry.get("evaluation")
        or ""
    )
    model_id = (
        row.get("model_id")
        or entry.get("model_id")
        or entry.get("model")
        or ""
    )
    sample_id = row.get("sample_id")
    if sample_id is None:
        sample_id = row.get("id")

    score = first_value(row, [("evaluation", "score"), ("score",)])
    is_correct = first_value(
        row,
        [("evaluation", "is_correct"), ("is_correct",), ("correct",)],
    )
    input_raw = first_value(
        row,
        [("input", "raw"), ("input", "prompt"), ("prompt",), ("question",)],
    )
    input_reference = first_value(
        row,
        [
            ("input", "reference"),
            ("reference",),
            ("target",),
            ("ground_truth",),
            ("answer",),
        ],
    )
    output_raw = first_value(
        row,
        [
            ("output", "raw"),
            ("output", "text"),
            ("response",),
            ("completion",),
            ("prediction",),
        ],
    )

    return {
        "eee_benchmark": str(entry.get("benchmark", "")),
        "evaluation_name": text_value(evaluation_name),
        "model_id": text_value(model_id),
        "sample_id": text_value(sample_id),
        "score": text_value(score),
        "is_correct": text_value(is_correct),
        "input_raw": text_value(input_raw),
        "input_reference": text_value(input_reference),
        "output_raw": text_value(output_raw),
        "input_json": json_value(row.get("input")),
        "output_json": json_value(row.get("output")),
        "evaluation_json": json_value(row.get("evaluation")),
        "source_instance_level_path": str(entry.get("instance_level_path", "")),
    }


FLAT_COLUMNS = [
    "eee_benchmark",
    "evaluation_name",
    "model_id",
    "sample_id",
    "score",
    "is_correct",
    "input_raw",
    "input_reference",
    "output_raw",
    "input_json",
    "output_json",
    "evaluation_json",
    "source_instance_level_path",
]


# -----------------------------------------------------------------------------
# Download one source file
# -----------------------------------------------------------------------------

def download_sample_file(entry: dict[str, Any]) -> dict[str, Any]:
    path = entry.get("instance_level_path")
    if not path:
        raise ValueError("Missing instance_level_path")

    rows = []
    with open_url(hf_url(path)) as response:
        for line_number, raw_line in enumerate(response, start=1):
            if not raw_line.strip():
                continue
            try:
                rows.append(json.loads(raw_line))
            except json.JSONDecodeError as exc:
                raise RuntimeError(
                    f"Invalid JSON in {path}, line {line_number}: {exc}"
                ) from exc

    return {"entry": entry, "rows": rows}


# -----------------------------------------------------------------------------
# Main
# -----------------------------------------------------------------------------

def main() -> int:
    parser = argparse.ArgumentParser(
        description="Build combined item-level EEE data for filtered benchmarks."
    )
    parser.add_argument(
        "--input",
        type=Path,
        default=Path("eee_item_level_benchmarks_filtered.csv"),
        help="Filtered benchmark CSV (default: eee_item_level_benchmarks_filtered.csv).",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=Path("."),
        help="Output directory (default: current directory).",
    )
    parser.add_argument(
        "--workers",
        type=int,
        default=6,
        help="Parallel downloads (default: 6).",
    )

    output_group = parser.add_mutually_exclusive_group()
    output_group.add_argument(
        "--csv-only",
        action="store_true",
        help="Write only the flattened CSV.",
    )
    output_group.add_argument(
        "--jsonl-only",
        action="store_true",
        help="Write only the full-fidelity JSONL.",
    )

    args = parser.parse_args()

    if args.workers < 1:
        parser.error("--workers must be >= 1")

    args.output_dir.mkdir(parents=True, exist_ok=True)

    selected = load_filtered_benchmarks(args.input)
    selected_names = set(selected)

    print(
        f"Loaded {len(selected_names):,} filtered benchmarks from:\n"
        f"  {args.input.resolve()}"
    )

    manifest = read_json_url(hf_url(MANIFEST_PATH))
    print(f"EEE snapshot created_at: {manifest.get('created_at', 'unknown')}")
    entries = load_entries(manifest)

    matching_entries = [
        e
        for e in entries
        if (
            e.get("benchmark") in selected_names
            and e.get("instance_level_available") is True
            and e.get("instance_level_path")
        )
    ]
    matching_entries.sort(
        key=lambda e: (
            str(e.get("benchmark", "")),
            str(e.get("instance_level_path", "")),
        )
    )

    found_benchmarks = {str(e.get("benchmark", "")) for e in matching_entries}
    missing_benchmarks = selected_names - found_benchmarks

    print(f"Matching EEE item-level source files: {len(matching_entries):,}")
    print(
        "Filtered benchmarks represented in current EEE snapshot: "
        f"{len(found_benchmarks):,}/{len(selected_names):,}"
    )

    if missing_benchmarks:
        print("\nWARNING: no current item-level entry found for:")
        for benchmark in sorted(missing_benchmarks):
            print(f"  - {benchmark}")

    if not matching_entries:
        raise RuntimeError("No matching item-level EEE files found.")

    declared_bytes = sum(
        int(e.get("instance_level_size_bytes") or 0) for e in matching_entries
    )
    if declared_bytes:
        print(
            f"Declared item-level download size: {declared_bytes:,} bytes "
            f"({declared_bytes / (1024 ** 3):.2f} GiB)"
        )

    csv_path = args.output_dir / "eee_item_level_filtered.csv"
    jsonl_path = args.output_dir / "eee_item_level_filtered.jsonl"
    summary_path = args.output_dir / "eee_item_level_dataset_summary.csv"
    failures_path = args.output_dir / "eee_item_level_download_failures.csv"

    write_csv_output = not args.jsonl_only
    write_jsonl_output = not args.csv_only

    csv_file = None
    csv_writer = None
    jsonl_file = None

    if write_csv_output:
        csv_file = csv_path.open("w", encoding="utf-8", newline="")
        csv_writer = csv.DictWriter(csv_file, fieldnames=FLAT_COLUMNS)
        csv_writer.writeheader()

    if write_jsonl_output:
        jsonl_file = jsonl_path.open("w", encoding="utf-8")

    row_counts = defaultdict(int)
    source_file_counts = defaultdict(int)
    models_by_benchmark = defaultdict(set)
    items_by_benchmark = defaultdict(set)
    failures = []

    completed = 0
    total_rows_written = 0
    start_time = time.time()

    try:
        with ThreadPoolExecutor(max_workers=args.workers) as executor:
            futures = {
                executor.submit(download_sample_file, entry): entry
                for entry in matching_entries
            }

            for future in as_completed(futures):
                entry = futures[future]
                completed += 1

                try:
                    result = future.result()
                except Exception as exc:
                    failures.append(
                        {
                            "benchmark": str(entry.get("benchmark", "")),
                            "instance_level_path": str(
                                entry.get("instance_level_path", "")
                            ),
                            "error": f"{type(exc).__name__}: {exc}",
                        }
                    )
                else:
                    source_entry = result["entry"]
                    benchmark = str(source_entry.get("benchmark", ""))
                    source_file_counts[benchmark] += 1

                    for source_row in result["rows"]:
                        flat = flatten_row(source_row, source_entry)

                        if write_csv_output:
                            csv_writer.writerow(flat)

                        if write_jsonl_output:
                            augmented = dict(source_row)
                            augmented["_eee_benchmark"] = benchmark
                            augmented["_eee_source_path"] = str(
                                source_entry.get("instance_level_path", "")
                            )
                            jsonl_file.write(
                                json.dumps(
                                    augmented,
                                    ensure_ascii=False,
                                    separators=(",", ":"),
                                )
                                + "\n"
                            )

                        row_counts[benchmark] += 1
                        total_rows_written += 1

                        model_id = flat["model_id"]
                        sample_id = flat["sample_id"]
                        evaluation_name = flat["evaluation_name"]

                        if model_id:
                            models_by_benchmark[benchmark].add(model_id)
                        if sample_id:
                            # evaluation_name is included to avoid collisions where
                            # multiple sub-evaluations reuse sample IDs such as 0,1,2.
                            items_by_benchmark[benchmark].add(
                                (evaluation_name, sample_id)
                            )

                if (
                    completed == 1
                    or completed % 25 == 0
                    or completed == len(matching_entries)
                ):
                    elapsed = time.time() - start_time
                    rate = completed / elapsed if elapsed > 0 else 0
                    print(
                        f"  {completed:,}/{len(matching_entries):,} files "
                        f"({rate:.2f} files/s), "
                        f"rows={total_rows_written:,}, "
                        f"failures={len(failures):,}",
                        flush=True,
                    )
    finally:
        if csv_file is not None:
            csv_file.close()
        if jsonl_file is not None:
            jsonl_file.close()

    summary_rows = []
    for benchmark in sorted(selected_names):
        expected = selected[benchmark]
        observed_n_items = len(items_by_benchmark.get(benchmark, set()))
        observed_n_models = len(models_by_benchmark.get(benchmark, set()))

        summary_rows.append(
            {
                "benchmark": benchmark,
                "expected_n_items": expected["expected_n_items"],
                "observed_n_items": observed_n_items,
                "expected_n_models": expected["expected_n_models"],
                "observed_n_models": observed_n_models,
                "n_rows": row_counts.get(benchmark, 0),
                "n_source_files": source_file_counts.get(benchmark, 0),
                "item_count_matches": observed_n_items
                == expected["expected_n_items"],
                "model_count_matches": observed_n_models
                == expected["expected_n_models"],
            }
        )

    summary_rows.sort(
        key=lambda r: (
            -r["observed_n_models"],
            -r["observed_n_items"],
            r["benchmark"].lower(),
        )
    )

    with summary_path.open("w", encoding="utf-8", newline="") as f:
        fields = [
            "benchmark",
            "expected_n_items",
            "observed_n_items",
            "expected_n_models",
            "observed_n_models",
            "n_rows",
            "n_source_files",
            "item_count_matches",
            "model_count_matches",
        ]
        writer = csv.DictWriter(f, fieldnames=fields)
        writer.writeheader()
        writer.writerows(summary_rows)

    if failures:
        with failures_path.open("w", encoding="utf-8", newline="") as f:
            writer = csv.DictWriter(
                f,
                fieldnames=["benchmark", "instance_level_path", "error"],
            )
            writer.writeheader()
            writer.writerows(failures)
    elif failures_path.exists():
        failures_path.unlink()

    print("\nDone.")
    print(f"Total item-level rows written: {total_rows_written:,}")

    if write_csv_output:
        print(f"\nFlattened analysis dataset:\n  {csv_path.resolve()}")
    if write_jsonl_output:
        print(f"\nFull-fidelity JSONL dataset:\n  {jsonl_path.resolve()}")

    print(f"\nCoverage summary:\n  {summary_path.resolve()}")

    if failures:
        print(
            f"\nWARNING: {len(failures):,} source files failed.\n"
            f"Failure log:\n  {failures_path.resolve()}"
        )

    print(
        "\n"
        f"{'benchmark':45} {'items':>10} {'models':>10} {'rows':>12}"
    )
    print("-" * 82)
    for row in summary_rows:
        print(
            f"{row['benchmark'][:45]:45} "
            f"{row['observed_n_items']:>10,} "
            f"{row['observed_n_models']:>10,} "
            f"{row['n_rows']:>12,}"
        )

    return 0 if not failures else 2


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except KeyboardInterrupt:
        print("\nInterrupted by user.", file=sys.stderr)
        raise SystemExit(130)
