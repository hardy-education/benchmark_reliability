# Data collection scripts

These scripts rebuild the item-level datasets used for the cross-source
G-studies from public Hugging Face datasets. None of them needs credentials.
They write scores and identifiers only, never raw model responses, prompts, or
judge reasoning. Run them from this folder and move their output folders into
`../data/` (or point `BENCHREL_DATA` at them).

| Script | Source | Output | Dependencies |
|---|---|---|---|
| `eee_item_level_inventory.py` | [Every Eval Ever datastore](https://huggingface.co/datasets/evaleval/EEE_datastore) | `eee_item_level_evaluations.csv`, `eee_item_level_benchmarks.csv`: which evaluations have item-level results, with item and model counts | standard library |
| `eee_build_filtered_item_dataset.py` | same | `eee_item_level_filtered.csv` / `.jsonl`: item-level results for the benchmarks in `eee_item_level_benchmarks_filtered.csv` (included) | standard library |
| `openeval_build_response_scores.py` | [Open-Eval-Commons/OpenEval](https://huggingface.co/datasets/Open-Eval-Commons/OpenEval) (CC-BY-NC-4.0) | `openeval_response_scores/`: responses, scores, items, models, adaptations, metrics (judge facet) | pandas, numpy, pyarrow, huggingface_hub |
| `wabm_build_item_scores.py` | [madesai/what-ai-benchmarks-actually-measure](https://huggingface.co/datasets/madesai/what-ai-benchmarks-actually-measure) | `wabm_item_scores/`: scores, items, metrics (scorer facet), models, benchmarks | pandas, numpy, pyarrow, huggingface_hub, unidecode |

```bash
pip install pandas numpy pyarrow huggingface_hub unidecode
python eee_item_level_inventory.py --limit 20          # quick check; the full scan downloads a lot
python eee_build_filtered_item_dataset.py --output-dir ../data
python openeval_build_response_scores.py --out-dir ../data/openeval_response_scores
python wabm_build_item_scores.py --out-dir ../data/wabm_item_scores
```

The main 18-benchmark response matrix, `aggdf.csv`, is not built by these
scripts; see `../data/README.md`.
