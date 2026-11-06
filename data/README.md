# Data

`aggdf.csv` (about 37 MB) is not tracked in git. Download it from the project
Google Drive (link in the legacy `metric_bootstraps.R`:
https://drive.google.com/file/d/1SL7MNKiynqWhtn5pCN59xp4XpZpK1Qln/view) and
place it here, or set `BENCHREL_DATA` to the folder that contains it.

Layout: one row per `(benchmark, item)` and one column per model. Cells are 0/1,
or NA when the model was not evaluated on that item. There are 18 benchmarks
(7 Open LLM Leaderboard, 11 HELM), 252–12,032 items each, and 40–429 models
each. Only boolq, legalbench, and wikifact have unobserved cells.

Expected MD5: `732ff4a37b25a225920c19c580073269` (36,567,644 bytes).

Scripts that rebuild the other item-level sources (EEE, OpenEval, WABM) from
Hugging Face are in [`../data_collection/`](../data_collection/README.md).

Check the file with:

```bash
Rscript -e 'pkgload::load_all(); print(benchmark_summary(load_responses()))'
```

## Complete benchmark set (optional; reliability landscape only)

`complete_cells.csv.gz` contains 109 further benchmarks, one row per
model × item cell. Build it from the outputs of the `data_collection/` scripts:

```bash
Rscript analysis/00_build_complete.R --raw=/dir/with/sources
# or name each source:
Rscript analysis/00_build_complete.R --wabm=.../wabm_item_scores --openeval=.../openeval_response_scores \
  --eee=.../eee_item_level_filtered.csv --livebench=.../livebench.parquet
```

| Source | Files read | Score used |
|---|---|---|
| WABM | `wabm_item_scores/{scores,metrics,items}.csv` | primary metric, averaged over replicates |
| OpenEval | `openeval_response_scores/{scores,metrics,responses}.csv` | primary metric, averaged over runs and prompt setups |
| Every Eval Ever | `eee_item_level_filtered.csv` (6 GB; only 4 columns are read) | `is_correct` |
| LiveBench | `livebench.parquet` (needs the `arrow` package) | `score` |

Benchmarks whose cells are all identical are dropped. On the paper's data
these are the EEE IFEval, AgentHarm, HealthBench and LINGOLY sets and the
WABM SALAD-Bench, which score 0 everywhere.

`openllm_all_model_info_full.csv` (Open LLM Leaderboard model metadata;
4,201,054 bytes, MD5 `cd42d8098c7236ed0d7990bb40e8546f`) gives the upload
dates for the temporal populations `early_half` / `late_half`. To
use them: `model_scores(long, model_upload_dates())`.
