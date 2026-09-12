# Data

Neither input file is in version control (`data` is git-ignored), because
`aggdf.csv` is ~37 MB of derived data that is better distributed as a release
asset than as repository history.

## Inputs

| File | Size | MD5 | Contents |
|---|---|---|---|
| `aggdf.csv` | 36,567,644 B | `732ff4a37b25a225920c19c580073269` | Pooled model×item response matrix, all 18 benchmarks |
| `openllm_all_model_info_full.csv` | 4,201,054 B | `cd42d8098c7236ed0d7990bb40e8546f` | Open LLM Leaderboard model metadata |

Download `aggdf.csv`:
<https://drive.google.com/file/d/1SL7MNKiynqWhtn5pCN59xp4XpZpK1Qln/view>

Verify after downloading:

```bash
md5 -q data/aggdf.csv    # macOS
md5sum data/aggdf.csv    # Linux
```

## Format

`aggdf.csv` is **wide**: `item`, `benchmark`, then one column per evaluated
model (585 of them). A cell is that model's score on that item — binary for
most benchmarks. A model that was not evaluated on a benchmark has `NA` in
those rows, so the matrix is ragged across benchmarks even though it is
rectangular on disk.

`load_responses()` pivots it long to `benchmark`, `item`, `model`, `resp`,
which is the shape everything downstream expects. Filter benchmarks in that
call rather than afterwards: all 18 come to roughly 25 million rows.

## Provenance

Seven benchmarks come from the Hugging Face
[Open LLM Leaderboard](https://huggingface.co/open-llm-leaderboard)
(official-providers subset, scored with the EleutherAI LM Evaluation Harness):
ARC-Challenge, BBH, GPQA, IFEval, MMLU-Pro, MUSR, OpenLLM-MATH.

Eleven come from [HELM](https://crfm.stanford.edu/helm/): AIR-Bench 2024, BBQ,
BoolQ, CommonSense, LegalBench, MedQA, MMLU Clinical Knowledge, MMLU Math,
MMLU Professional Medicine, Thai Exam, WikiFact.

Per-benchmark item counts, model counts and mean scores are in App. H, Table 4
of the paper, and `benchmark_summary()` recomputes them from `aggdf.csv`.

## Outputs

Pipeline stages write here:

```
data/results/
  boots/
    item_metrics_<benchmark>_n<k>_<population>_<date>.rds   stage 10
    item_criteria_<date>.rds                                stage 20
    gdstudy_n<k>_<date>.rds                                 stage 30
    rank_stability_<date>.rds                               stage 40
  facet_gstudy_<spec>_<date>.rds                            stage 50
  table1_population_gcoefficients.csv                       stage 60
  lambda_comparison.csv                                      stage 60
```

The `_n<k>_` in a filename is the item budget; `read_dstudy_results()` parses it
back out, so do not rename these files.
