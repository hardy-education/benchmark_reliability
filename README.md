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
make all         # stages 1-6 for all 18 benchmarks; about 30 min with 2PL on one core (6-7 min with WORKERS=8), under 2 min without
```

Data: put `aggdf.csv` in `data/` (see [`data/README.md`](data/README.md)), or set
`BENCHREL_DATA=/path/to/dir`.

## Layout

```
R/                      the package (functions only; no side effects on load)
  data.R                load_responses(), response_matrix(), handle_missing()
  populations.R         model populations as strings: "all", "top_frac:0.1", "top_n:10", ...
  gtheory.R             variance_components(), reliability_coefficients(), dstudy_curve(), items_needed()
  item_metrics.R        lambda_star(), item_difficulty(), irt_2pl_discrimination(),
                        corrected_iic(), loevinger_hi(), selection_order()
  curves.R              reliability_curve(): G, Phi, Kendall tau at every prefix of an item order
  pipeline.R            per-benchmark drivers used by analysis/
  plots.R               figure functions
analysis/               the pipeline; every stage takes --flags and skips finished work
  01_item_metrics.R     item selection values per benchmark x reference population
  02_reliability_curves.R  empirical variance decomposition as items are added
  03_population_gstudies.R full-benchmark G-studies and D-study curves by population (Fig. 2)
  04_figures.R          figures/ from results/ (--runs=<ref>__<meas> for one run)
  05_timing_report.R    run-time estimates from the recorded job timings
  06_paper_figures.R    selection figures: core4 panel (all models / top 10%) and appendix figures
  07_run_diagnostics.R  per-benchmark table to read a run's curves (constant items, ceiling, 2PL convergence, boundaries)
  99_validate_reml.R    closed-form REML vs lmer on real benchmarks and prefixes (boundary, degenerate, missing cells)
tests/testthat/         closed forms vs. fixest/mokken/psych/lme4; curve vs. brute force
docs/                   closed-form derivations, timing results, migration notes
data_collection/        Python scripts that rebuild the EEE / OpenEval / WABM item-level data from Hugging Face
```

## The two populations

An item-selection study involves two model populations, and both are explicit
here:

* `--reference`: the models whose responses score, and therefore choose, the
  items.
* `--measured`: the models on which the reliability of the chosen items is
  evaluated.

Population strings are `all`, `top_half`, `bottom_half`, `top_frac:<p>`,
`bottom_frac:<p>`, `top_n:<k>`, and `random_frac:<p>:<seed>`. The paper's
original design is `--reference=bottom_half --measured=top_half`. When the two
populations are the same, the curves are in-sample: the models that chose the
items are the ones being evaluated.

```bash
# Score items on the top 10% of models; evaluate on both the top 10% and everyone
Rscript analysis/01_item_metrics.R --reference=top_frac:0.1
Rscript analysis/02_reliability_curves.R --reference=top_frac:0.1 --measured=top_frac:0.1,all
Rscript analysis/04_figures.R --benchmarks=core4
```

Curve figures are smoothed by default (`geom_smooth()` at ggplot2's defaults,
with its standard-error band). Pass `--smooth=false` to stage 4 or 6, or
`smooth = FALSE` to `plot_reliability_curves()`, for the raw item-by-item
curves, which show the noise of each added item; stage 6 writes these with a
`_raw` suffix. A smooth can extrapolate slightly outside 0–1 at the ends; the
axis is clipped to 0–1.

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
| Fig. 2 reliability vs. n by cohort | `analysis/03_population_gstudies.R`, `plot_dstudy_populations()` |
| Fig. 5, Figs. 7–8 selection curves | `analysis/02_reliability_curves.R`, `plot_reliability_curves()` |
| Table 2 timing | `analysis/05_timing_report.R`, [`docs/timing.md`](docs/timing.md) |

[`docs/MIGRATION.md`](docs/MIGRATION.md) lists every behaviour change from the
legacy scripts and maps legacy column names (`L6`, `CA`, `Hi`, `a`, `diff`,
`rand`) to the new method names.

## Not yet ported

The cross-source 102-benchmark G-study (`gstudy_agg.R`: CareBench, SOCRATES,
LiveBench, EEE, WABM, OpenEval, multi-judge and multi-run facets) needs its own
data loaders. `variance_components()` and `reliability_coefficients()` already
apply to those sources once they are in long format with columns `model`,
`item`, and `resp`. Multi-facet designs (judge, run, prompt) would add a
`variance_components_facets()` built on `lme4`.

## License

MIT. See `LICENSE`.
