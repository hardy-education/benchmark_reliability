# Benchmarks as Measurement Instruments

Analysis code for **"Benchmarks as Measurement Instruments: Quantifying Signal
and Noise for More Efficient AI Evaluations Under Distribution Shift."**

The paper recasts AI benchmark reliability as a measurement problem. A
benchmark score is an observation of a latent capability, so the useful
question is not what a model scored but how much of the observed variation
reflects stable capability differences rather than instrument noise. Framing
that as a signal-to-noise ratio over a crossed random-effects decomposition
yields three results this repository reproduces:

1. **A mapping from variance components to expected rank stability.** The
   generalizability coefficient determines expected Kendall concordance across
   independent item resamples, so leaderboard stability becomes predictable
   from variance components rather than only observable after the fact.
2. **λ⋆₆, an item-level reliability proxy for the many-items/few-models
   regime.** Classical Guttman λ₆ needs more models than items; modern
   benchmarks have the reverse. λ⋆₆ reparameterises item predictability as a
   slope-activated two-way fixed-effect problem, keeping every item and
   preserving inter-item dependence rather than repairing identifiability by
   redefining the estimand. Selecting items by λ⋆₆ reaches higher ranking
   reliability than random subsampling at the same evaluation budget.
3. **Reliability is population-dependent, not intrinsic.** Restricting
   evaluation to higher-performing models usually *lowers* rank reliability,
   because between-model variance contracts near the ceiling while residual
   variance does not contract proportionally.

## Repository layout

```
R/                       analysis modules -- functions only, no side effects
  00_setup.R             packages, paths, parallel backend, plotting theme
  01_data.R              loading, reshaping, model-population splits
  02_gtheory.R           variance decomposition, G/D studies, SNR <-> tau
  03_lambda_star.R       lambda*_6 and classical Guttman lambda_6
  04_item_metrics.R      alpha, omega, PCA loadings, Mokken H_i/Z_i, MS
  04b_mokken_single.R    Molenaar-Sijtsma single-item reliability (adapted)
  05_irt.R               2PL models, held-out scoring, test information
  06_rank_stability.R    item selection and cumulative Kendall curves
  07_plots.R             one function per figure panel
  load_all.R             sources the above in dependency order

scripts/                 the pipeline, one stage per file
  10_item_metrics.R      per-item metrics, bootstrapped over item samples
  20_adjust_metrics.R    remove draw-level fixed effects, aggregate per item
  30_gd_studies.R        G/D studies over item budgets and criteria
  40_rank_stability.R    empirical Kendall tau curves
  50_facet_gstudy.R      Open LLM metadata facet G-study
  60_figures.R           figures and Table 1

tests/testthat/          92 unit tests, mostly over the paper's identities
legacy/                  the pre-refactor scripts, kept for reference
data/                    inputs and results (git-ignored; see data/README.md)
figures/                 figure output
```

`R/` defines functions and constants and nothing else — no data loading, no
`future::plan()`, no model fitting. So you can get the whole toolkit with

```r
source("R/load_all.R")
```

without launching any analysis, and each `scripts/` stage is a thin driver over
those functions.

## Reproducing the paper

### Requirements

R ≥ 4.2 and the packages in `DESCRIPTION`:

```bash
make deps
```

`ggmirt` is not on CRAN; install it from GitHub if you want the IRT diagnostic
plots (`remotes::install_github("masurp/ggmirt")`). Everything else in the
pipeline runs without it.

### Data

`data/aggdf.csv` (≈37 MB) holds the pooled model×item response matrix for all
18 benchmarks; `data/openllm_all_model_info_full.csv` holds the Open LLM
Leaderboard model metadata. Neither is in version control. See
[`data/README.md`](data/README.md) for the download link, checksums, and
provenance.

### Pipeline

```bash
make data            # verify the inputs are present
make item-metrics    # stage 10: per-item metrics        (hours to days)
make adjust          # stage 20: residualise and aggregate
make gd-studies      # stage 30: G/D studies over budgets (hours)
make rank-stability  # stage 40: Kendall tau curves
make figures         # stage 60: figures into figures/
```

or `make all` for stages 10–60. Every stage takes flags, so a smoke test over
two benchmarks and three budgets is:

```bash
Rscript scripts/10_item_metrics.R --benchmarks=musr,gpqa --budgets=25,50,100 --boots=2
Rscript scripts/20_adjust_metrics.R
Rscript scripts/30_gd_studies.R --budgets=25,50,100
Rscript scripts/40_rank_stability.R --max-items=100
Rscript scripts/60_figures.R
```

Stage 10 is the expensive one. For scale: the paper reports 3,579 s for a
single λ⋆₆ estimation on AirBench at 3,758 items × 21 models (App. D.4), on an
M1 Max.

Stages 10, 30 and 40 write one file per unit of work and skip work whose output
already exists, so an interrupted run resumes rather than restarting.

### Tests

```bash
make test
```

92 tests. Most check the paper's identities directly rather than comparing
against stored numbers — that `G ↔ SNR ↔ E[τ]` round-trips, that relative
reliability is invariant to item variance while dependability is not, that the
D-study curve follows the 1/I law, that λ⋆₆ stays informative when M ≪ I and
classical λ₆ does not. Two files pin numerical behaviour instead: the
Molenaar-Sijtsma implementation is checked cell-for-cell against the original,
and λ⋆₆'s regression output against the pre-refactor estimator.

## Where the paper's claims live in the code

| Paper | Code |
|---|---|
| Eq. (1) crossed random-effects model | `fit_gstudy()` |
| Eq. (3) G-coefficient for relative decisions | `gstudy_coefficients()` → `m_rel` |
| Eq. (4)–(5) SNR ↔ reliability | `g_to_snr()`, `snr_to_g()` |
| Eq. (6), (17) expected Kendall τ | `g_to_expected_tau()`, `snr_to_expected_tau()` |
| Eq. (18) expected τ² | `snr_to_expected_tau2()` |
| Eq. (19) inverse mapping τ → SNR | `tau_to_snr()` |
| Eq. (9)–(10) model population splits | `make_hilo_splits()` |
| Eq. (11)–(12) slope-activated design | `lambda_star_components()` |
| Eq. (13), (26) L⋆₆(i) and λ⋆₆ | `lambda_star_components()`, `lambda_star()` |
| Eq. (22)–(24) classical Guttman λ₆ | `guttman_lambda6()` |
| Prop. D.1 identification by connectivity | `check_activation_connectivity()` |
| D-study projections | `dstudy_project()`, `dstudy_curve()` |
| Fig. 1 distribution shift | `scripts/60_figures.R`, panels in `R/07_plots.R` |
| Fig. 2 reliability vs. item count | `plot_reliability_vs_items()` |
| Fig. 3 item selection at fixed budget | `plot_selection_panel()` |
| Fig. 4 λ⋆₆ vs λ₆ | `plot_lambda_comparison()` |
| Table 1 population G-coefficients | `scripts/60_figures.R --figures=table1` |
| App. H benchmark summary | `benchmark_summary()` |

## Two naming conventions worth knowing

**`train` / `test` are model populations, not a fitting split.** `train` is the
lower-performing half of models and `test` the higher-performing half
(Eqs. 9–10). No model is fitted on one and evaluated on the other; both halves
are analysed the same way, and the comparison between them *is* the
distribution-shift result. The labels are kept because the saved result files
use them. `make_hilo_splits()` also returns an unambiguous `population` column.

**`L6` in the results tables is λ⋆₆, not classical λ₆.** The two are
distinguished as `lambda_star_components()` / `lambda_star()` versus
`guttman_lambda6()`, and `METRIC_LABELS` renders `L6` as "Lambda 6\*".

## Benchmarks

18 benchmarks, 7 from the Hugging Face Open LLM Leaderboard (official-providers
subset) and 11 from HELM, spanning 40–429 evaluated models and 252–12,032
items. `benchmark_summary()` reproduces the counts in App. H, Table 4.

## Citation

See [`CITATION.cff`](CITATION.cff).

## License

MIT — see [`LICENSE`](LICENSE). `R/04b_mokken_single.R` is adapted from the
`mokken` package; see the header of that file for attribution.
