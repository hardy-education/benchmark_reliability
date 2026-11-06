# benchrel — Benchmarks as Measurement Instruments

Analysis code for **"Benchmarks as Measurement Instruments: Quantifying Signal
and Noise for More Efficient AI Evaluations Under Distribution Shift."**

The repository does three things:

1. **Variance decomposition.** It splits a benchmark's model × item scores into
   model signal, item difficulty, and residual. From these it reports relative
   reliability G(n) and absolute reliability Φ(n) for any model population
   (paper Sec. 3).
2. **Item scoring.** It scores items with λ⋆₆ and the comparison criteria:
   raw difficulty, 2PL discrimination, the corrected inter-item correlation,
   Loevinger's Hᵢ, and random order (Sec. 4.3, App. D).
3. **Reliability curves.** It re-estimates the variance components as each
   selected item is added, which gives empirical reliability against the
   number of items (Fig. 5, App. Figs. 7–8).

Everything except the 2PL fit uses an **exact closed form** that reproduces the
legacy `fixest` / `mokken` / `psych` / `lme4` computations to floating-point
precision; see [`docs/closed_forms.md`](docs/closed_forms.md). In practice:

* Scoring every item of every benchmark takes seconds.
* A reliability curve with a point at *every* item count costs about 0.1–1.5 s
  per ordering.
* No bootstrap over item subsets is needed.

## Quick start

```bash
make deps        # install R packages
make test        # closed forms vs. fixest, mokken, psych, lme4 and a numerical REML optimiser
make all         # item scores, curves, G-studies and timing for all 18 benchmarks; about 30 min with 2PL on one core (6-7 min with WORKERS=8)
make paper WORKERS=8   # the paper's figures and positive-shift tables (docs/FIGURES.md)
```

Data: put `aggdf.csv` in `data/` (see [`data/README.md`](data/README.md)), or set
`BENCHREL_DATA=/path/to/dir`.

## Layout

```
R/                      the package (functions only; no side effects on load)
  data.R                load_responses(), response_matrix(), handle_missing(), BENCHMARK_SETS
  complete.R            the 100+ benchmark Complete set: build_complete_cells(), complete_responses()
  populations.R         model populations as strings: "all", "top_frac:0.1", "middle_half", ...; eligibility
  gtheory.R             variance_components(), reliability_coefficients(), dstudy_curve(), items_needed()
  item_metrics.R        lambda_star(), item_difficulty(), irt_2pl_discrimination(),
                        corrected_iic(), loevinger_hi(), selection_order()
  curves.R              reliability_curve(): G, Phi, Kendall tau at every prefix of an item order
  pipeline.R            per-benchmark drivers used by analysis/
  cache.R               cached_item_metrics(), cached_curves(), cached_gstudies(), check_curves()
  figures.R             one plot function per paper figure type, shared theme and scales
  latex.R               figure environments and the collected .tex file
analysis/               the pipeline; every stage takes --flags and skips finished work
  00_build_complete.R   cell-level table of the Complete benchmark set from the raw sources
  01_item_metrics.R     item selection values per benchmark x reference population
  02_reliability_curves.R  empirical variance decomposition as items are added
  03_population_gstudies.R full-benchmark G-studies and D-study curves by population
  04_paper_figures.R    the paper's figures, with their LaTeX captions in one .tex file
  05_timing_report.R    run-time estimates from the recorded job timings
  06_shift_summary.R    criteria vs random at 10% of items under positive shift; CSVs and LaTeX tables
  07_run_diagnostics.R  per-benchmark table to read a run's curves (constant items, ceiling, 2PL convergence, boundaries)
  99_validate_reml.R    closed-form REML vs lmer on real benchmarks and prefixes (boundary, degenerate, missing cells)
tests/testthat/         closed forms vs. fixest/mokken/psych/lme4; curve vs. brute force
docs/                   closed-form derivations, figure pipeline (FIGURES.md), timing results, migration notes
data_collection/        Python scripts that rebuild the EEE / OpenEval / WABM item-level data from Hugging Face
```

## The two populations

An item-selection study involves two model populations, and both are explicit
here:

* `--reference`: the models whose responses score, and therefore choose, the
  items.
* `--measured`: the models on which the reliability of the chosen items is
  evaluated.

Population strings are `all`, `top_half`, `bottom_half`, `middle_half`,
`top_frac:<p>`, `bottom_frac:<p>`, `top_n:<k>`, `random_frac:<p>:<seed>`, its
complement `random_comp:<p>:<seed>`, and the temporal halves `early_half` /
`late_half` (these need upload dates: `model_scores(long, model_upload_dates())`).
The complements `not_top_frac:<p>` and `not_top_n:<k>` (every model not in the
top group) give positive-shift pairs that split the models in two, such as
`not_top_frac:0.1->top_frac:0.1` (bottom 90% → top 10%).
A pair is written `reference->measured`. `population_eligible()` drops a
top-Y% or top-n population that has 5 or fewer models or covers more than half
of a benchmark's models (so top 10% needs at least 60 models); bottom groups
only need more than 5 models, and the temporal halves need dates for at least
90% of a benchmark's models. The paper's
original design is `--reference=bottom_half --measured=top_half`. When the two
populations are the same, the curves are in-sample: the models that chose the
items are the ones being evaluated.

```bash
# Score items on the top 10% of models; evaluate on both the top 10% and everyone
Rscript analysis/01_item_metrics.R --reference=top_frac:0.1
Rscript analysis/02_reliability_curves.R --reference=top_frac:0.1 --measured=top_frac:0.1,all
```

Plot the result with `plot_selection_curves()` (see "Cached, modular use" below).

Curve figures are smoothed by default (`geom_smooth()` at ggplot2's defaults,
with its standard-error band). Pass `--lines=raw` to stage 4, or
`lines = "raw"` to the plot functions, for the raw item-by-item curves, which
show the noise of each added item; stage 4 writes these with a `_raw` suffix.
A smooth can extrapolate slightly outside 0–1 at the ends; the axis is
clipped to 0–1.

## Paper figures

`analysis/04_paper_figures.R` draws the paper's 16 figures, listed in
[`docs/FIGURES.md`](docs/FIGURES.md), and `analysis/06_shift_summary.R` the
positive-shift tables. They write the following to `paper_figures/`, which is
git-ignored:

* one PDF per figure, 5.5 in wide, in Times;
* `figures.tex`, with a `figure` environment and caption for each figure,
  grouped into main text, appendix and supplementary sections;
* `manifest.csv`, which lists every figure's section, size and caption;
* `tables/`, the shift-summary tables (LaTeX) and their CSVs.

```bash
make complete-data RAW=/path/raw  # once: the Complete benchmarks from the data_collection/ outputs
make paper-inputs WORKERS=8       # item scores, curves and G-studies the figures need (2PL is the slow part)
make paper-figures WORKERS=4      # the figures + LaTeX
make shift-summary                # the tables
Rscript analysis/04_paper_figures.R --only=panel,shift --lines=raw   # some families; raw instead of smoothed curves
```

Without the Complete data, stage 4 draws every figure except the
reliability landscape.

Each figure type is also a function you can call on your own results:

| Function | Figure type |
|---|---|
| `plot_reliability_contrast()` | 1a: relative and absolute reliability vs. items, by population |
| `plot_selection_curves()` | 1b: an outcome vs. items, by selection method |
| `plot_pair_curves()` | 1b by shift: one selection method, one line per reference → measured pair |
| `plot_dstudy()` | 1c: G-theory projection for random draws |
| `plot_snr_r2_path()` | 2: R²_τ = sin²(πτ/2) vs. G(n) as items are added |
| `plot_reliability_landscape()` | 3: model vs. residual variance share, classified by an SNR criterion |
| `plot_population_dots()` | 4: full-benchmark G and Φ by population |
| `plot_population_histograms()` | 5: model score distributions by population |
| `plot_items_needed()` | 6: benchmark length vs. items needed |

All of them share one theme (`theme_benchrel()`) and one set of scales
(`scale_method()`, `scale_population()`, `scale_reliability_type()`), so each
method, population and reliability type is drawn the same way in every figure.
`save_figure()` writes a PDF at paper size.

### Cached, modular use

The `cached_*()` functions read a stage's output from `results/` if it is
there, and otherwise compute and save it. They use the same file layout as
the scripts, so the two can be mixed:

```r
long   <- load_responses(benchmarks = "main")
scores <- model_scores(long)
cv <- cached_curves(long, scores, "main", "bottom_half->top_half")   # scores items on the bottom half if needed
check_curves(cv)                                                      # NA / boundary shares, range checks
check_curves(cv, long, scores, verify = 5,                            # refit 5 prefixes per curve with lmer
             metrics = cached_item_metrics(long, scores, "main", "bottom_half"))
plot_selection_curves(cv, "kendall", methods = c("lambda_star", "difficulty", "random"))

# Your own criterion: any item scores (larger = earlier), or explicit orders
mine <- order_to_metrics(list(gpqa = my_gpqa_order), method = "my_order")
cv2 <- cached_curves(long, scores, "gpqa", "all", "all", methods = c("my_order", "random"),
                     metrics = mine, run_tag = "mine")    # cached under results/curves/all__all__mine/

g <- cached_gstudies(long, scores, "main", c("all", "top_half", "top_n:10"), name = "main")
plot_population_dots(g)
```

## Using the functions directly

```r
pkgload::load_all()                       # or library(benchrel) after R CMD INSTALL .
long   <- load_responses(benchmarks = "core4")
scores <- model_scores(long)
X      <- response_matrix(long, "gpqa", population_models(scores, "gpqa", "top_frac:0.1"))
X      <- X[, !colnames(X) %in% constant_items(X)]

l6  <- lambda_star(X)                      # milliseconds
ord <- selection_order(l6, colnames(X), "lambda_star")
cv  <- reliability_curve(X, ord)           # G(n), Phi(n), Kendall tau for n = 2..I
variance_components(X)                     # = lmer(resp ~ 1 + (1|item) + (1|model)), boundaries included; `status` flags them
```

To add a selection criterion, write `f(X) -> named numeric vector` (larger =
select earlier). Then register it in `.metric_fun()` and `SELECTION_METHODS`.

## Mapping to the paper

| Paper | Code |
|---|---|
| Eq. 1 crossed decomposition | `variance_components()` |
| Eqs. 2–4 G, Φ, SNR | `reliability_coefficients()` |
| Eq. 5 items needed | `items_needed()` |
| App. C E[τ] ↔ G | `g_to_expected_tau()`, `tau_to_snr()` |
| Sec. 3.4 populations | `population_models()` |
| Eqs. 16–18 λ⋆₆ | `lambda_star()` (exact), `lambda_star_feols()` (regression form) |
| App. D.8.1 2PL | `irt_2pl_discrimination()` |
| App. D.8.2 Loevinger's Hᵢ | `loevinger_hi()` |
| App. D.8.3 Eq. 34 | `corrected_iic()` |
| Figures | `analysis/04_paper_figures.R`, [`docs/FIGURES.md`](docs/FIGURES.md) |
| Positive-shift tables | `analysis/06_shift_summary.R` |
| Table 2 timing | `analysis/05_timing_report.R`, [`docs/timing.md`](docs/timing.md) |

[`docs/MIGRATION.md`](docs/MIGRATION.md) lists every behaviour change from the
legacy scripts and maps legacy column names (`L6`, `CA`, `Hi`, `a`, `diff`,
`rand`) to the new method names.

## The Complete benchmark set

The reliability landscape (Fig. 3) covers 127 benchmarks: the 18 in `aggdf`,
plus 109 from WABM, OpenEval, Every Eval Ever and LiveBench.
`analysis/00_build_complete.R` builds `data/complete_cells.csv.gz` from the
outputs of the `data_collection/` scripts. For each source it keeps the primary
metric and averages replicates, runs and prompt setups within each model × item
cell. `complete_responses()` loads the result.

Most of these matrices are incomplete. `variance_components()` then fits
`lmer` to the observed cells, which is valid there; the closed form is used
whenever a matrix is complete.

Still not ported: the multi-facet designs, such as the judge, run and prompt
facets of CareBench, SOCRATES and the multi-judge studies. They would need a
`variance_components_facets()` built on `lme4`.

## License

MIT. See `LICENSE`.
