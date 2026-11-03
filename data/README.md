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
