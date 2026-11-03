#!/usr/bin/env python3
"""Consolidate the HF dataset `Open-Eval-Commons/OpenEval` (item-centred evaluation
repository) into a small star schema of CSV files for generalizability-theory analysis.

Outputs (in --out-dir):
  responses.csv    one row per model response: response_uid, item_uid, model_uid, adaptation_uid, sample_index
  scores.csv       long fact table: response_uid, metric_uid, value
  items.csv        one row per item, with the item-level facets the repository carries
  models.csv       one row per model (provider, size)
  adaptations.csv  one row per distinct adaptation (system prompt, decoding parameters, n-shot, tools)
  metrics.csv      one row per (benchmark, metric, judge) -- judge is the rater facet
  benchmarks.csv   one row per benchmark, with bench-table metadata and counts
  README.md        codebook + validation report

Raw model responses, prompts/demonstrations, judge templates and judge reasoning are never
written; only scores, short categorical judge labels, and facet identifiers.

Usage:
  python openeval_build_response_scores.py --out-dir openeval_response_scores [--source-dir DIR]
"""

import argparse
import ast
import hashlib
import json
import re
from collections import Counter, defaultdict
from pathlib import Path

import numpy as np
import pandas as pd
import pyarrow as pa
import pyarrow.compute as pc
import pyarrow.dataset as ds
import pyarrow.parquet as pq

REPO_ID = "Open-Eval-Commons/OpenEval"

# Judge artifacts that are long free text and must never be exported.
LONG_ARTIFACTS = {"evaluation_template", "model_reasoning", "judge_prompt", "raw_output", "explanation"}
# Judge metadata: recorded once per metric in metrics.csv rather than repeated on every response.
METADATA_ARTIFACTS = {"judge_version", "version", "judge_model", "scorer_version"}
MAX_LABEL_LEN = 64        # artifact values longer than this are not categorical labels
MAX_LABEL_LEVELS = 20     # more distinct values than this -> not a usable categorical facet
MAX_GOLD_LEN = 60         # references longer than this are reference texts, not answer keys

# OpenEval does not designate a headline metric, so this mapping is ours: the canonical metric of each source
# benchmark. It is resolved to the best-covered judge variant of that metric, and judge labels are never chosen.
# Benchmarks absent from this map fall back to the `score` metric with the most observations.
PRIMARY_METRIC = {
    "advbench": "refusal-strings",                    # refusal-string classifier, the only score
    "anthropic_red_teaming": "safety_gpt_score",      # GPT-4o judge, full coverage (llama judge covers 27%)
    "bbq": "accuracy",                                # 99% of responses; the exact_match family is a small wave
    "bold": "logprob",                                # only score in the release
    "boolq": "exact_match",                           # canonical EM for BoolQ
    "cnndm": "rouge_2",                               # standard summarisation headline
    "culturalbench": "quasi_prefix_exact_match",      # only score
    "disinformation": "self_bleu",                    # HELM disinformation headline
    "do_not_answer": "longformer-action-ro",          # the benchmark's own action classifier
    "emobench": "quasi_exact_match",                  # overall EM (emotion/cause are sub-scores)
    "gpqa": "chain_of_thought_correctness",           # only score
    "harmbench": "haiku-llm-judge",                   # LLM judge, best covered of four scorers
    "health_orsc_bench": "refusal-strings",           # only score
    "hi_tom": "quasi_prefix_exact_match",             # only score
    "ifeval": "ifeval_strict_accuracy",               # IFEval strict accuracy
    "imdb": "exact_match",                            # canonical EM
    "mmlu_pro": "chain_of_thought_correctness",       # only score
    "moralbench": "prefix_exact_match",               # only score
    "omni_math": "omni_math_accuracy",                # judge-panel accuracy (panels differ by wave)
    "opentom": "quasi_postfix_exact_match",           # only score
    "or_bench": "orbench-refusal",                    # only score
    "salad_bench": "attack_success",                  # attack success rate, the SALAD-Bench headline
    "simplesafetytests": "safety_gpt_score",          # GPT-4o judge, full coverage
    "sorry_bench": "ft-mistral-7b-sorrybench",        # the benchmark's fine-tuned judge
    "truthfulqa": "bleurt-20",                        # BLEURT-based truthfulness, best covered
    "wildbench": "wildbench_score_rescaled",          # WB-Score (rescaled judge panel)
    "xstest": "safety_gpt_score",                     # GPT-4o judge, full coverage
    "xsum": "rouge_2",                                # standard summarisation headline
}

PROJECTION = {
    "response_id": ds.field("response_id"),
    "model_name": ds.field("model", "name"),
    "model_size": ds.field("model", "size"),
    "sys_instruction": ds.field("model", "model_adaptation", "system_instruction"),
    "gen_params": ds.field("model", "model_adaptation", "generation_parameters"),
    "tools": ds.field("model", "model_adaptation", "tools", "type"),
    "tools_content": ds.field("model", "model_adaptation", "tools", "content"),
    "demos": ds.field("item_adaptation", "demonstrations"),
    "ext_res": ds.field("item_adaptation", "external_resources", "type"),
    "scores": ds.field("scores"),
}
ARTIFACT_PROJECTION = {"scores": ds.field("scores")}


def bench_of(path):
    return re.sub(r"-\d{5}-of-\d{5}\.parquet$", "", Path(path).name).replace(".parquet", "")


def norm_bench(name):
    return str(name).strip().lower().replace("-", "_")


def norm_model(name):
    """Submissions spell the same model differently (Qwen1.5-0.5B-Chat vs qwen-1.5-0.5b-chat)."""
    return re.sub(r"[^a-z0-9]", "", str(name).lower())


def sha8(text):
    return hashlib.sha1(str(text).encode("utf-8")).hexdigest()[:8]


def as_list(v):
    if isinstance(v, np.ndarray):
        return v.tolist()
    return list(v) if isinstance(v, list) else ([] if v is None else [v])


# --------------------------------------------------------------------------- items
def build_items(src, out_rows):
    """Read item/*.parquet -> items table + per-benchmark {item_id: item_uid}."""
    index, uid = {}, 1
    for f in sorted((src / "item").glob("*.parquet")):
        b = bench_of(f)
        d = pq.read_table(f).to_pandas()
        ids = d["item_id"].astype(str)
        meta = d["item_metadata"]
        content = d["item_content"]
        refs = content.map(lambda c: [str(x) for x in as_list(c.get("references"))])
        parsed = refs.map(lambda rs: [_parse_ref(r) for r in rs])
        ref_lens = [len(p[0]) for ps in parsed for p in ps if p[0]]
        keep_gold = bool(ref_lens) and float(np.median(ref_lens)) <= MAX_GOLD_LEN
        gold = (parsed.map(lambda ps: ";".join(sorted({p[0] for p in ps if p[0] and len(p[0]) <= MAX_GOLD_LEN})) or None)
                if keep_gold else pd.Series([None] * len(d), index=d.index))
        tags = parsed.map(lambda ps: ";".join(sorted({t for p in ps for t in p[1]})) or None)
        src_field = meta.map(lambda m: _source_fields(m.get("source")))
        rows = pd.DataFrame({
            "item_uid": range(uid, uid + len(d)),
            "benchmark": b,
            "item_id": ids,
            "source": src_field.map(lambda t: t[0]),
            "source_item_id": src_field.map(lambda t: t[1]),
            "contributor": meta.map(lambda m: (m.get("contributor") or {}).get("name")),
            "contributor_affiliation": meta.map(lambda m: (m.get("contributor") or {}).get("affiliation")),
            "ingestion_time": meta.map(lambda m: m.get("ingestion_time")),
            "n_inputs": content.map(lambda c: len(as_list(c.get("input")))),
            "n_references": refs.map(len),
            "gold_label": gold,
            "reference_tags": tags,
            "schema_version": d.get("schema_version"),
        })
        index[b] = dict(zip(ids, rows["item_uid"]))
        uid += len(d)
        out_rows.append(rows)
        print(f"  items {b:24s} {len(d):>7}", flush=True)
    return index


def _source_fields(src):
    """item_metadata.source is a benchmark name, a struct, or a struct rendered as text."""
    d = src
    if isinstance(d, str) and d.lstrip().startswith("{"):
        try:
            d = ast.literal_eval(d)
        except Exception:
            try:
                d = json.loads(d)
            except Exception:
                return d, None
    if isinstance(d, dict):
        return d.get("benchmark_name"), d.get("source_item_id")
    return (None if d is None else str(d)), None


def _parse_ref(ref):
    """Return (short answer text or None, [tags]) from a reference entry."""
    try:
        o = json.loads(ref) if isinstance(ref, str) else ref
    except Exception:
        return (ref if len(ref) <= MAX_GOLD_LEN else None), []
    if not isinstance(o, dict):
        return None, []
    out = o.get("output")
    text = out.get("text") if isinstance(out, dict) else (out if isinstance(out, str) else None)
    if text is None and isinstance(o.get("content"), str):
        text = o["content"]
    return (str(text) if text is not None else None), [str(t) for t in as_list(o.get("tags"))]


# --------------------------------------------------------------------------- response_id -> item_id
class ItemResolver:
    """response_id starts with item_id, followed by _<model name with '/'->'_'>_<sample index>."""

    def __init__(self, id_to_uid):
        self.id_to_uid = id_to_uid
        self.misses = 0

    def resolve(self, response_ids, model_names):
        uids, samples = np.empty(len(response_ids), dtype="int64"), np.empty(len(response_ids), dtype="int32")
        for i, (rid, mname) in enumerate(zip(response_ids, model_names)):
            uid, sample = -1, -1
            tail = rid.rsplit("_", 1)
            if len(tail) == 2 and tail[1].isdigit():
                sample = int(tail[1])
                flat = str(mname).replace("/", "_")
                if tail[0].endswith("_" + flat):
                    uid = self.id_to_uid.get(tail[0][: -(len(flat) + 1)], -1)
            if uid < 0:  # fall back to longest item_id prefix
                parts = rid.split("_")
                for k in range(len(parts) - 1, 1, -1):
                    cand = self.id_to_uid.get("_".join(parts[:k]), -1)
                    if cand > 0:
                        uid = cand
                        rest = parts[k:]
                        sample = int(rest[-1]) if rest and rest[-1].isdigit() else -1
                        break
            if uid < 0:
                self.misses += 1
            uids[i], samples[i] = uid, sample
        return uids, samples


# --------------------------------------------------------------------------- score flattening
def flatten_scores(scores_col):
    """Vectorised: return (row_index, metric_name, judge, value, artifact_lists) per metric entry."""
    values = scores_col.field("value")
    metrics = scores_col.field("metric")
    n_per_row = pc.list_value_length(values).fill_null(0).to_numpy(zero_copy_only=False)
    n_metric = pc.list_value_length(metrics).fill_null(0).to_numpy(zero_copy_only=False)
    n = np.minimum(n_per_row, n_metric)
    if (n_per_row != n_metric).any():  # ragged rows: drop the excess on either side
        keep_v, keep_m = [], []
        vo, mo = 0, 0
        for nv, nm, k in zip(n_per_row, n_metric, n):
            keep_v.extend(range(vo, vo + k)); keep_m.extend(range(mo, mo + k))
            vo += nv; mo += nm
        flat_vals = pc.list_flatten(values).take(pa.array(keep_v))
        flat_mets = pc.list_flatten(metrics).take(pa.array(keep_m))
    else:
        flat_vals = pc.list_flatten(values)
        flat_mets = pc.list_flatten(metrics)
    row_idx = np.repeat(np.arange(len(n)), n)
    names = flat_mets.field("name").to_numpy(zero_copy_only=False)
    judge_lists = flat_mets.field("models")
    jlen = pc.list_value_length(judge_lists).fill_null(0).to_numpy(zero_copy_only=False)
    judges = np.full(len(names), "", dtype=object)
    if (jlen > 0).any():
        flat_j = pc.list_flatten(judge_lists).to_numpy(zero_copy_only=False)
        pos = np.concatenate([[0], np.cumsum(jlen)])
        for i in np.nonzero(jlen)[0]:
            judges[i] = ",".join(flat_j[pos[i]:pos[i + 1]])
    return row_idx, names, judges, flat_vals.to_numpy(zero_copy_only=False), flat_mets.field("extra_artifacts")


def artifact_pairs(artifacts):
    """Yield (metric_entry_index, type, content) for every artifact, vectorised per column."""
    types, contents = artifacts.field("type"), artifacts.field("content")
    tlen = pc.list_value_length(types).fill_null(0).to_numpy(zero_copy_only=False)
    if not (tlen > 0).any():
        return np.array([], dtype="int64"), np.array([], dtype=object), np.array([], dtype=object)
    flat_t = pc.list_flatten(types).to_numpy(zero_copy_only=False)
    flat_c = pc.list_flatten(contents).to_numpy(zero_copy_only=False)
    idx = np.repeat(np.arange(len(tlen)), tlen)
    n = min(len(flat_t), len(flat_c), len(idx))
    return idx[:n], flat_t[:n], flat_c[:n]


# --------------------------------------------------------------------------- passes
def scan_artifacts(files):
    """Cheap pre-pass: which (benchmark, metric, artifact type) are short categorical labels?"""
    levels = defaultdict(set)
    versions = defaultdict(set)
    dropped = set()
    for f in files:
        b = bench_of(f)
        for batch in ds.dataset(f).to_batches(columns=ARTIFACT_PROJECTION):
            if batch.num_rows == 0:
                continue
            _, names, _, _, artifacts = flatten_scores(batch.column("scores"))
            idx, types, contents = artifact_pairs(artifacts)
            for i, t, c in zip(idx, types, contents):
                t = str(t)
                key = (b, str(names[i]), t)
                if key in dropped:
                    continue
                c = "" if c is None else str(c)
                if t in METADATA_ARTIFACTS:
                    if len(c) <= MAX_LABEL_LEN:
                        versions[(b, str(names[i]))].add(c)
                    continue
                if t in LONG_ARTIFACTS or len(c) > MAX_LABEL_LEN:
                    dropped.add(key); levels.pop(key, None); continue
                levels[key].add(c)
                if len(levels[key]) > MAX_LABEL_LEVELS:
                    dropped.add(key); levels.pop(key, None)
    return {k: sorted(v) for k, v in levels.items()}, {k: sorted(v) for k, v in versions.items()}


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--out-dir", default="openeval_response_scores")
    ap.add_argument("--source-dir", default=None, help="local snapshot of the HF repo (downloaded if omitted)")
    ap.add_argument("--limit-files", type=int, default=None, help="debug: only read N response files")
    args = ap.parse_args()

    if args.source_dir:
        src = Path(args.source_dir)
    else:
        from huggingface_hub import snapshot_download
        src = Path(snapshot_download(REPO_ID, repo_type="dataset",
                                     allow_patterns=["bench/*", "item/*", "response/*", "*.json"]))
    out = Path(args.out_dir)
    out.mkdir(parents=True, exist_ok=True)

    print("reading items ...", flush=True)
    item_rows = []
    item_index = build_items(src, item_rows)
    items = pd.concat(item_rows, ignore_index=True)

    files = sorted((src / "response").glob("*.parquet"))
    if args.limit_files:
        files = files[: args.limit_files]
    print(f"scanning judge artifacts in {len(files)} response files ...", flush=True)
    label_levels, judge_versions = scan_artifacts(files)
    print(f"  categorical judge labels: {len(label_levels)}; judge versions: {len(judge_versions)}", flush=True)

    resp_path, score_path = out / "responses.csv", out / "scores.csv"
    resp_path.unlink(missing_ok=True); score_path.unlink(missing_ok=True)
    model_uid, adapt_uid, metric_uid = {}, {}, {}
    model_sizes = defaultdict(Counter)
    adapt_rows, metric_rows = [], []
    stats = defaultdict(lambda: dict(n=0, nan=0, lo=np.inf, hi=-np.inf, total=0.0, vals=set()))
    bench_counts = defaultdict(lambda: Counter())
    next_response_uid = 1
    n_resp_rows = n_score_rows = 0
    first_write = True

    for f in files:
        b = bench_of(f)
        resolver = ItemResolver(item_index.get(b, {}))
        for batch in ds.dataset(f).to_batches(columns=PROJECTION):
            if batch.num_rows == 0:
                continue
            rid = batch.column("response_id").to_numpy(zero_copy_only=False)
            mname = batch.column("model_name").to_numpy(zero_copy_only=False)
            msize = batch.column("model_size").to_numpy(zero_copy_only=False)
            sysi = batch.column("sys_instruction").to_numpy(zero_copy_only=False)
            gen = batch.column("gen_params").to_numpy(zero_copy_only=False)
            n_demo = pc.list_value_length(batch.column("demos")).fill_null(0).to_numpy(zero_copy_only=False)
            n_ext = pc.list_value_length(batch.column("ext_res")).fill_null(0).to_numpy(zero_copy_only=False)
            tools_col = batch.column("tools")
            tlen = pc.list_value_length(tools_col).fill_null(0).to_numpy(zero_copy_only=False)
            tools_str = np.full(len(tlen), "", dtype=object)
            if (tlen > 0).any():
                flat_t = pc.list_flatten(tools_col).to_numpy(zero_copy_only=False)
                flat_c = pc.list_flatten(batch.column("tools_content")).to_numpy(zero_copy_only=False)
                row_of = np.repeat(np.arange(len(tlen)), tlen)
                acc = defaultdict(list)
                for j in range(min(len(flat_t), len(row_of))):
                    t = str(flat_t[j])
                    if t in ("", "[]", "None", "nan"):
                        continue
                    c = str(flat_c[j]) if j < len(flat_c) else ""
                    acc[row_of[j]].append(f"{t}={c}" if c not in ("", "[]", "None", "nan") else t)
                for k, v in acc.items():
                    tools_str[k] = ";".join(sorted(set(v)))[:120]

            item_uids, sample_idx = resolver.resolve(rid, mname)
            muids = np.empty(len(rid), dtype="int32")
            auids = np.empty(len(rid), dtype="int32")
            for i in range(len(rid)):
                name = str(mname[i])
                if name not in model_uid:
                    model_uid[name] = len(model_uid) + 1
                muids[i] = model_uid[name]
                sz = str(msize[i])
                if sz not in ("None", "nan", ""):
                    model_sizes[name][sz] += 1
                akey = (str(sysi[i]), str(gen[i]), int(n_demo[i]), str(tools_str[i]), int(n_ext[i]))
                if akey not in adapt_uid:
                    adapt_uid[akey] = len(adapt_uid) + 1
                    adapt_rows.append(_adaptation_row(adapt_uid[akey], *akey))
                auids[i] = adapt_uid[akey]

            uids = np.arange(next_response_uid, next_response_uid + len(rid), dtype="int64")
            next_response_uid += len(rid)
            pd.DataFrame({"response_uid": uids, "item_uid": item_uids, "model_uid": muids,
                          "adaptation_uid": auids, "sample_index": sample_idx}).to_csv(
                resp_path, mode="a", header=first_write, index=False)
            n_resp_rows += len(rid)
            bench_counts[b]["responses"] += len(rid)

            row_idx, names, judges, values, artifacts = flatten_scores(batch.column("scores"))
            score_resp, score_metric, score_value = [], [], []
            for i in range(len(names)):
                key = (b, str(names[i]), str(judges[i]))
                mu = metric_uid.get(key)
                if mu is None:
                    mu = metric_uid[key] = len(metric_uid) + 1
                    metric_rows.append(dict(metric_uid=mu, benchmark=b, metric=str(names[i]),
                                            judge=str(judges[i]), kind="score", value_labels="",
                                            judge_version=";".join(judge_versions.get((b, str(names[i])), [])[:5])))
                score_resp.append(uids[row_idx[i]]); score_metric.append(mu); score_value.append(values[i])
                s = stats[mu]; v = values[i]
                s["n"] += 1
                if v != v:
                    s["nan"] += 1
                else:
                    s["lo"] = min(s["lo"], v); s["hi"] = max(s["hi"], v); s["total"] += v
                    if len(s["vals"]) <= 50:
                        s["vals"].add(round(float(v), 6))

            a_idx, a_types, a_contents = artifact_pairs(artifacts)
            for j in range(len(a_idx)):
                i = a_idx[j]
                lkey = (b, str(names[i]), str(a_types[j]))
                levels = label_levels.get(lkey)
                if not levels:
                    continue
                key = (b, f"{names[i]}__{a_types[j]}", str(judges[i]))
                mu = metric_uid.get(key)
                if mu is None:
                    mu = metric_uid[key] = len(metric_uid) + 1
                    metric_rows.append(dict(metric_uid=mu, benchmark=b, metric=f"{names[i]}__{a_types[j]}",
                                            judge=str(judges[i]), kind="judge_label",
                                            value_labels=";".join(f"{k}={v}" for k, v in enumerate(levels)),
                                            judge_version=";".join(judge_versions.get((b, str(names[i])), [])[:5])))
                c = "" if a_contents[j] is None else str(a_contents[j])
                score_resp.append(uids[row_idx[i]]); score_metric.append(mu); score_value.append(float(levels.index(c)))
                code = levels.index(c)
                s = stats[mu]; s["n"] += 1; s["lo"] = min(s["lo"], code); s["hi"] = max(s["hi"], code)
                s["total"] += code
                if len(s["vals"]) <= 50:
                    s["vals"].add(float(code))

            pd.DataFrame({"response_uid": score_resp, "metric_uid": score_metric, "value": score_value}).to_csv(
                score_path, mode="a", header=first_write, index=False, float_format="%.6g")
            n_score_rows += len(score_resp)
            first_write = False
        print(f"  {b:24s} responses={bench_counts[b]['responses']:>9} unresolved_items={resolver.misses}", flush=True)
        bench_counts[b]["unresolved"] += resolver.misses

    _write_outputs(out, src, items, model_uid, model_sizes, adapt_rows, metric_rows, stats, bench_counts,
                   n_resp_rows, n_score_rows)


def _adaptation_row(uid, sys_instruction, gen_params, n_demo, tools, n_ext):
    try:
        g = json.loads(gen_params) if gen_params not in ("", "None", "nan") else {}
    except Exception:
        g = {}
    si = "" if sys_instruction in ("None", "nan") else sys_instruction
    return dict(adaptation_uid=uid, n_demonstrations=n_demo, has_system_instruction=int(bool(si)),
                system_instruction_len=len(si), system_instruction_sha8=sha8(si) if si else "",
                system_instruction_head=si[:120].replace("\n", " ") if si else "",
                temperature=g.get("temperature"), top_p=g.get("top_p"), top_k=g.get("top_k"),
                do_sample=g.get("do_sample"), max_tokens=g.get("max_tokens"),
                tools=tools, n_external_resources=n_ext,
                generation_parameters_sha8=sha8(gen_params))


def _coverage_table(out, items, model_uid):
    """Distinct items answered per (benchmark, model) and per (benchmark, normalised model key)."""
    codes, uniques = pd.factorize(items["benchmark"])
    code_by_item = np.zeros(int(items["item_uid"].max()) + 1, dtype="int32")
    code_by_item[items["item_uid"].to_numpy()] = codes
    name_of = {v: k for k, v in model_uid.items()}
    max_uid = max(model_uid.values())
    key_codes, key_uniques = pd.factorize([norm_model(name_of.get(u, "")) for u in range(max_uid + 1)])
    key_codes = np.asarray(key_codes)
    resp = pd.read_csv(out / "responses.csv", usecols=["item_uid", "model_uid"],
                       dtype={"item_uid": "int32", "model_uid": "int32"})
    iu, mu = resp["item_uid"].to_numpy(), resp["model_uid"].to_numpy()
    keep = iu > 0
    base = pd.DataFrame({"b": code_by_item[np.clip(iu, 0, None)][keep], "m": mu[keep],
                         "k": key_codes[mu[keep]], "i": iu[keep]})
    raw = base.groupby(["b", "m"], sort=False)["i"].nunique().reset_index(name="n_items")
    raw["benchmark"] = uniques[raw["b"].to_numpy()]
    raw["model_name"] = raw["m"].map(name_of)
    raw["model_key"] = key_uniques[key_codes[raw["m"].to_numpy()]]
    norm = base.groupby(["b", "k"], sort=False)["i"].nunique().reset_index(name="n_items")
    norm["benchmark"] = uniques[norm["b"].to_numpy()]
    norm["model_key"] = key_uniques[norm["k"].to_numpy()]
    n_answered = int(np.unique(iu[keep]).size)
    return (raw[["benchmark", "model_name", "model_key", "n_items"]],
            norm[["benchmark", "model_key", "n_items"]], n_answered)


def _write_outputs(out, src, items, model_uid, model_sizes, adapt_rows, metric_rows, stats, bench_counts,
                   n_resp_rows, n_score_rows):
    items.to_csv(out / "items.csv", index=False)

    models = pd.DataFrame([dict(model_uid=v, model_name=k, model_key=norm_model(k),
                                size=(model_sizes[k].most_common(1)[0][0] if model_sizes.get(k) else None),
                                n_size_labels=len(model_sizes.get(k, ())),
                                provider=(k.split("/")[0] if "/" in k else None))
                           for k, v in model_uid.items()]).sort_values("model_uid")
    models.to_csv(out / "models.csv", index=False)

    pd.DataFrame(adapt_rows).to_csv(out / "adaptations.csv", index=False)

    metrics = pd.DataFrame(metric_rows)
    st = pd.DataFrame([dict(metric_uid=k, n_obs=v["n"], n_missing=v["nan"],
                            value_min=(None if v["lo"] == np.inf else v["lo"]),
                            value_max=(None if v["hi"] == -np.inf else v["hi"]),
                            value_mean=(v["total"] / (v["n"] - v["nan"]) if v["n"] - v["nan"] else None),
                            n_distinct_capped=len(v["vals"]),
                            is_binary=int(bool(v["vals"]) and v["vals"] <= {0.0, 1.0}))
                       for k, v in stats.items()])
    metrics = metrics.merge(st, on="metric_uid", how="left")
    metrics["scale"] = np.where(metrics["kind"].eq("judge_label"), "nominal_coded",
                       np.where(metrics["is_binary"].eq(1), "binary",
                       np.where((metrics["value_min"] >= 0) & (metrics["value_max"] <= 1), "continuous_0_1",
                       np.where(metrics["n_distinct_capped"].le(10), "ordinal_or_discrete", "continuous"))))
    metrics["is_primary"] = 0
    for b, g in metrics.groupby("benchmark"):
        cand = g[g["kind"].eq("score")]
        if cand.empty:
            continue
        named = cand[cand["metric"].eq(PRIMARY_METRIC.get(b, ""))]
        sel = named if len(named) else cand
        metrics.loc[sel["n_obs"].idxmax(), "is_primary"] = 1
    lead = ["metric_uid", "benchmark", "metric", "judge", "is_primary", "kind", "scale"]
    metrics = metrics[lead + [c for c in metrics.columns if c not in lead]]
    metrics.to_csv(out / "metrics.csv", index=False)

    cov, cov_key, n_answered = _coverage_table(out, items, model_uid)
    models_per_bench = cov.groupby("benchmark")["model_name"].nunique()
    keys_per_bench = cov_key.groupby("benchmark")["model_key"].nunique()

    bench_meta = pq.read_table(src / "bench" / "train-00000-of-00001.parquet").to_pandas()
    bench_meta["key"] = bench_meta["benchmark_name"].map(norm_bench)
    rows = []
    for b, c in sorted(bench_counts.items()):
        m = bench_meta[bench_meta["key"] == norm_bench(b)]
        rows.append(dict(benchmark=b, n_responses=c["responses"], n_items=int((items.benchmark == b).sum()),
                         n_models=int(models_per_bench.get(b, 0)),
                         n_model_keys=int(keys_per_bench.get(b, 0)), n_metrics=int((metrics.benchmark == b).sum()),
                         n_unresolved_responses=c["unresolved"],
                         benchmark_version=(m.benchmark_version.iloc[0] if len(m) else None),
                         paper_url=(m.paper_url.iloc[0] if len(m) else None),
                         dataset_url=(m.dataset_url.iloc[0] if len(m) else None),
                         benchmark_tags=(";".join(as_list(m.benchmark_tags.iloc[0])) if len(m) else None)))
    benches = pd.DataFrame(rows)
    benches.to_csv(out / "benchmarks.csv", index=False)

    val = _validate_coverage(src, cov_key, items)
    _write_readme(out, items, models, metrics, benches, pd.DataFrame(adapt_rows), n_resp_rows, n_score_rows,
                  val, len(items) - n_answered)
    print(f"\nresponses={n_resp_rows:,} scores={n_score_rows:,} items={len(items):,} models={len(models)} "
          f"metrics={len(metrics)} adaptations={len(adapt_rows)}")
    if val is not None and len(val):
        print(val.head(30).to_string(index=False))


def _validate_coverage(src, cov, items):
    """Compare per-(benchmark, model) item coverage with the repository's own data summary."""
    summaries = sorted(src.glob("data_summary_*.json"))
    if not summaries:
        return None
    summary = json.loads(summaries[-1].read_text())
    n_items_by_bench = items.groupby("benchmark").size()
    mine_lookup = {(r.benchmark, r.model_key): r.n_items for r in cov.itertuples()}
    rows = []
    for b_raw, models in summary.items():
        b = norm_bench(b_raw)
        for model, info in models.items():
            rows.append(dict(benchmark=b, model=model, summary_n_items=info.get("n_items"),
                             mine_n_items=int(mine_lookup.get((b, norm_model(model)), 0)),
                             summary_coverage=info.get("coverage"),
                             n_items_total=int(n_items_by_bench.get(b, 0))))
    v = pd.DataFrame(rows)
    v["diff"] = v["mine_n_items"] - v["summary_n_items"]
    return v


def _write_readme(out, items, models, metrics, benches, adaptations, n_resp, n_scores, val, n_no_resp):
    sizes = {p.name: p.stat().st_size / 1e6 for p in sorted(out.glob("*.csv"))}
    md = lambda df: df.to_markdown(index=False, floatfmt=".4g") if len(df) else "_none_"
    primary_tbl = (metrics[metrics["is_primary"] == 1]
                   [["benchmark", "metric", "judge", "scale", "n_obs", "value_min", "value_max", "value_mean"]]
                   .sort_values("benchmark"))
    vsum = ""
    if val is not None and len(val):
        agree = (val["diff"] == 0).mean()
        worst = val.reindex(val["diff"].abs().sort_values(ascending=False).index).head(10)
        vsum = (f"{agree:.2%} of {len(val):,} (benchmark, model) pairs match the repository's own "
                f"`data_summary` item counts exactly.\n\nLargest differences:\n\n{md(worst)}")
    txt = f"""# OpenEval response-level scores (G-theory layout)

Built by `openeval_build_response_scores.py` from the Hugging Face dataset
[`Open-Eval-Commons/OpenEval`](https://huggingface.co/datasets/Open-Eval-Commons/OpenEval) (CC-BY-NC-4.0).
Model outputs, prompts, few-shot demonstrations, judge templates and judge reasoning are **not** included:
only numeric scores, short categorical judge labels, and facet identifiers.

## Files

| file | rows | MB | grain |
|---|---:|---:|---|
| `responses.csv` | {n_resp:,} | {sizes.get('responses.csv', 0):.1f} | one model response (item x model x adaptation x sample) |
| `scores.csv` | {n_scores:,} | {sizes.get('scores.csv', 0):.1f} | response x metric (scorer/judge) |
| `items.csv` | {len(items):,} | {sizes.get('items.csv', 0):.1f} | item |
| `models.csv` | {len(models):,} | {sizes.get('models.csv', 0):.3f} | model |
| `adaptations.csv` | {len(adaptations):,} | {sizes.get('adaptations.csv', 0):.3f} | distinct adaptation |
| `metrics.csv` | {len(metrics):,} | {sizes.get('metrics.csv', 0):.3f} | benchmark x metric x judge |
| `benchmarks.csv` | {len(benches):,} | {sizes.get('benchmarks.csv', 0):.3f} | benchmark |

Join keys: `scores.response_uid -> responses`, `responses.item_uid -> items`, `responses.model_uid -> models`,
`responses.adaptation_uid -> adaptations`, `scores.metric_uid -> metrics`, `items.benchmark -> benchmarks`.

```python
import pandas as pd
scores = pd.read_csv("scores.csv", dtype={{"response_uid": "int64", "metric_uid": "int16", "value": "float32"}})
responses = pd.read_csv("responses.csv", dtype={{"response_uid": "int64", "item_uid": "int32",
                        "model_uid": "int16", "adaptation_uid": "int16", "sample_index": "int16"}})
```

## Facets for generalizability analysis

- **model** (`model_uid`; `provider`, `size` in models.csv) -- the object of measurement
- **item** (`item_uid`) nested in **benchmark**; `reference_tags` carries per-item condition flags where the
  repository has them (e.g. BBQ ambiguous/non-ambiguous, answer key)
- **adaptation** (`adaptation_uid`): system prompt presence/identity, temperature, top_p, top_k, do_sample,
  max_tokens, number of few-shot demonstrations, tool use, external resources. These vary *within* several
  benchmarks and are a measurable source of variance.
- **sample_index**: repeated generations of the same model on the same item under the same adaptation
  (a within-model occasion facet; several benchmarks sample more than once)
- **metric / judge** (metrics.csv): the rater facet. `judge` is empty for deterministic metrics and names the
  scoring model otherwise (e.g. `openai/gpt-4o-2024-05-13`, `meta/llama-3.1-405b-instruct-turbo`,
  `microsoft/deberta-large-mnli`). Several benchmarks carry more than one judge per response, so judge
  disagreement is directly estimable.
- **kind**: `score` = numeric score as emitted; `judge_label` = short categorical judge verdict coded to
  integers, with the codebook in `value_labels`.
- **is_primary**: one metric per benchmark is flagged as the headline score (see below); use it to compare
  benchmarks without hand-picking among the 119 metric/judge combinations.

Scores are kept in each judge's native orientation exactly as the repository emitted them; they are **not**
normalised to a common "higher is better" convention. Check `metric`, `judge` and `value_min`/`value_max`
in metrics.csv before pooling.

## Benchmarks

{md(benches[['benchmark', 'n_responses', 'n_items', 'n_models', 'n_metrics', 'benchmark_tags']])}

## Metrics

{md(metrics[['metric_uid', 'benchmark', 'metric', 'judge', 'is_primary', 'kind', 'scale', 'n_obs', 'value_min', 'value_max', 'value_mean']])}

### Primary metric per benchmark

OpenEval designates no headline metric, so `is_primary` is **our** designation, not the repository's: the canonical
metric of the source benchmark, restricted to `kind == "score"` (never a judge label) and resolved to the
best-covered judge variant. Exactly one (benchmark, metric, judge) row is flagged per benchmark, so
`metrics[metrics.is_primary == 1]` gives one comparable score series per benchmark. Where the same metric is scored
by several judge panels or waves (omni_math, wildbench, do_not_answer, sorry_bench), the flag marks the
best-covered variant; the others share its `metric` name, so filter on the name to pool them.

{md(primary_tbl)}

## Validation

{vsum}

## Data-quality notes

- **Model naming is inconsistent across submissions**: the same model appears as e.g. `Qwen1.5-0.5B-Chat` and
  `qwen-1.5-0.5b-chat`. `models.csv` carries `model_key` (lowercase, punctuation stripped) -- group by it when the
  model is the object of measurement. `benchmarks.csv` reports both `n_models` (raw names) and `n_model_keys`
  ({len(models):,} raw names collapse to {models['model_key'].nunique():,} distinct models overall).
- **{n_no_resp:,} of {len(items):,} items carry no response** in this release and appear only in `items.csv`.
- **Metric coverage differs by submission wave within a benchmark** (BBQ carries both `accuracy` and the
  `*exact_match` family; CNN/DM adds SummaC and BERTScore only for later submissions), so metric availability is
  itself a facet -- check `n_obs` in metrics.csv before pooling.
- **Gold answers** are exported only where a benchmark's references are short answer keys; for summarisation and
  other free-text benchmarks the reference text is omitted by design.
- **Judge scores keep the judge's own orientation** and some use sentinel values (e.g. `-1` for a failed judge call
  in `safety_gpt_score` and `do-not-answer_annotation`); filter on the paired `*__evaluation_success` label.
- One response could not be matched to an item id and is stored with `item_uid = -1`.
"""
    (out / "README.md").write_text(txt)


if __name__ == "__main__":
    main()
