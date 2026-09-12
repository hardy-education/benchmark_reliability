# Refactoring notes

What changed when the analysis scripts in `legacy/` became the modules in `R/`
and the pipeline in `scripts/`. Kept as a record so any number in the paper can
be traced to the code that now produces it, and so the behaviour changes below
are explicit rather than buried in a diff.

The originals are preserved in `legacy/` and in git history.

## Duplication removed

The pre-refactor tree was four overlapping generations of the same analysis,
about 360 KB of R across nine files.

| Implemented | Times | Where |
|---|---|---|
| λ⋆₆ estimator | 4 | `lambda6.R::L6_est`, `lambda_star_full.R::L6_est`, `metric_bootstraps.R::estimate_lambda6_components`, `metric_bootstraps.R::feols_estimations` |
| Full metric pipeline | 2 | `metric_bootstraps.R` (newer), `metric_bootstraps_plots_experiments.R` (3,937 lines) |
| Figure code | 2 | `plot_all_code.R` (2,174 lines), `plot_eda.R` |
| G/D study | 3 | `gstudy_dstudy_basic.R`, `metric_bootstraps.R::make_gstudy_df`, `full_gstudy_openllmbenches.R` |
| Mokken `compute.PP` / `singlemokken` | 3 | `item_reliab_boot.R`, `metric_bootstraps.R`, `startup.R` |
| D-study coefficient block (~20 expressions) | 2 | `plot_all_code.R`, verbatim between `pldf` and `gpldf` |
| Saved-results loader loop | 3 | `plot_all_code.R` |
| Metric display-name `if_else` chain (13 branches) | 3 | `plot_all_code.R` |

The four λ⋆₆ copies ran the same regression but returned different column sets
under two different function names, and one named its R² output `r2` where
another named it `L6`. They are now `lambda_star_components()`, verified to
reproduce the original's output exactly (`tests/testthat/test-lambda-star.R`,
and a direct column-by-column comparison during the refactor: differences at
floating-point epsilon only, from `collapse::fmean` versus `base::mean`).

## Bugs found and fixed

Ordered roughly by consequence.

1. **`full_gstudy_openllmbenches.R`: the correlated-slopes model was saved with
   the wrong summary table.** `m3c` (correlated random slopes) was written to
   disk as `list(mod = m3c, sum_tab = gstudy_summary)`, where
   `gstudy_summary` is the variance-component table of `mbase`, a different
   specification. Any downstream read of that file got a model and a summary
   that disagreed. Now `facet_variance_table()` is computed from each fit and
   saved with it (`scripts/50_facet_gstudy.R`).

2. **`full_gstudy_openllmbenches.R`: `m3sum` could not run.** It applied
   `across(bench:base_type_bench, ...)` to a table where those indicator
   columns were never created — they are built in the `m1`/`mbase` summaries,
   not this one — so the expression referenced a column range that did not
   exist. Facet tagging now happens inside `facet_variance_table()` for every
   specification.

3. **`metric_bootstraps.R`: IRT item and model results were silently dropped.**
   The bootstrap loop collected `itdf = mirt_results$itdfs` and
   `moddf = mirt_results$moddfs`, but `mirt_fits()` returns `itdf` and `moddf`.
   Both were therefore `NULL` in every saved draw.

4. **`metric_bootstraps.R`: a loop variable was read outside its loop.** After
   the bootstrap loops closed, `mutate(iter = glue("{ifelse(ordered_models, ...)}"))`
   referenced `ordered_models`, whose last value happened to persist. The result
   label depended on loop-exit state rather than on anything meaningful.

5. **`make_sample_response()`: requesting the test population returned zero
   rows.** Item sampling was guarded by `if (is.null(items) & !(samp == "test"))`,
   so calling it with `samp = "test"` and no explicit `items` left `items`
   `NULL`, and the subsequent `filter(item %in% items)` matched nothing — no
   error, just an empty matrix. Selection no longer depends on which population
   was asked for (`sample_response_matrix()`); pass `items` when two draws must
   cover the same items.

6. **`metrics_for_study` was defined twice.** The second definition, ~80 lines
   below the first, silently replaced a 30-name vector with a 12-name one for
   everything downstream. The reported set is now `DEFAULT_METRICS` in
   `scripts/30_gd_studies.R`, in one place.

7. **`MS_est()` computed the most expensive metric twice.** It called
   `singlemokken()`, discarded the result, then called it again inside a
   `tryCatch`. Now called once (`item_ms()`).

8. **`fit_gstudy()`'s optimizer choice was computed and then ignored.** The
   original picked `Nelder_Mead` for fewer than 20 items into a local variable,
   and then hard-coded `optimizer = "bobyqa"` in the `lmerControl()` call. The
   choice is now passed through.

9. **A failed G-study returned a one-column tibble.** `make_gstudy_df()`
   returned `tibble(benchmark = b)` on a convergence failure, which after
   `bind_rows()` produced all-NA columns whose presence depended on which
   benchmarks converged; a later `select()` of a missing column would error.
   `gstudy_na_row()` now returns the full column set.

10. **`ggpubr` was used but never loaded.** `mirt_fits(plots = TRUE)` called
    `ggpubr::ggarrange()`, and no script in the repository loaded or declared
    `ggpubr`. It is now in `DESCRIPTION` under `Suggests`, and `fit_irt()`
    checks for it and fails with an actionable message.

11. **`theta_range` leaked from the calling environment.** `mirt_test_preds()`
    used `theta_range` without taking it as an argument, so its behaviour
    depended on a global of that name existing. Now a parameter with a default
    (`score_held_out()`).

12. **`gstudy_dstudy_basic.R` hard-coded the benchmark count.**
    `bind_cols(tibble(num_items = rep(1, 6), num_mod = 1))` assumed exactly six
    benchmarks, and a later `bind_cols(tibble(num_items = 1:30, ...))` was
    applied to a five-row variance table, where recycling does not align.
    `dstudy_curve()` takes the grid as an argument and works for any number of
    benchmarks.

13. **Conflicting parallel backends.** `future::plan("multisession")` ran at
    load time and `plan(multicore)` a few hundred lines later, so which backend
    was in force depended on how much of the file had been evaluated. There is
    now one entry point, `br_plan()`.

14. **λ⋆₆ reset the parallel plan to sequential.** All three copies ended with
    `plan(sequential)`, de-parallelising every later stage of the bootstrap
    loop. The caller's plan is now restored via `on.exit()`.

15. **`lambda_star_full.R`'s documented example did not run.** The roxygen
    example assigned `overall_lambda6` and then printed `lambda6`, an undefined
    name.

16. **Stage 10 wrote quadratically.** The original re-serialised the entire
    accumulated result list on every item budget, so I/O grew with the square
    of the number of draws. One file per (benchmark, budget) now.

17. **The random baseline was redrawn on every run.** `benchranddf` was built in
    an ungated top-level block with no seed, so the control series the selection
    criteria were compared against changed between runs.
    `add_random_baselines()` and `make_random_split()` take seeds.

18. **`cumulative_kendall_by_metric()` serialised its data twice.** It passed
    `benched` through `future.globals` while the worker closure already
    captured it, sending the whole response table to every worker twice. It also
    carried dead initialisation (`result_list <- vector(...)`, `i <- 1L`) that
    the `future_lapply` assignment immediately overwrote.

## Redundancy found in the reported quantities

`plot_all_code.R` computed two families of D-study coefficient per row, `*_D`
and `*_Scale`, differing in whether the components are also divided by the
number of models. For the **relative** (ranking) coefficients the divisor
cancels:

```
(sigma_M^2 / n_M) / (sigma_M^2 / n_M + sigma_e^2 / (I n_M))
  = sigma_M^2 / (sigma_M^2 + sigma_e^2 / I)
```

so `m_rel_Scale` is identically equal to `m_rel_D`, and `i_rel_Scale` to
`i_rel_D`, for every design size. The two were carried and plotted as if they
were different quantities. Only the **dependability** pair genuinely differs,
because the item term is not divided by `n_models` and so does not cancel.

Both names are kept, since the saved result files use them, but
`dstudy_project()` documents the identity and `tests/testthat/test-gtheory.R`
asserts it — so the pair cannot be edited apart by mistake, and nobody has to
rediscover that two of the columns carry no independent information.

## A trap worth naming

`dplyr`'s data masking makes one class of mistake invisible: if a function
parameter shares a name with a column of the data frame being filtered, the
column wins and the filter silently matches nothing. The reliability frame
carries a `population` column, so `filter(samp == population)` inside a
function whose argument is also called `population` compares the column against
itself and returns zero rows — no error, no warning, just an empty plot.

Every such argument in `R/07_plots.R` is therefore qualified with `.env$`, and
`plot_selection_panel()` raises an informative error rather than handing an
empty frame to `ggplot2`, whose message ("Faceting variables must have at least
one value") points nowhere near the cause.

## Behaviour that changed deliberately

These produce different results from the originals, and are the only places
that do.

- **Item parameters are no longer extracted from display-only plots.**
  `ggmirt`'s `personDist()`, `itemDist()` and `conRelPlot()` are data sources —
  their `@data` slots carry the tidied parameters — so they are still built
  unconditionally. But `testInfoPlot()` and the `ggpubr` arrangement are
  decoration, and those are now skipped when `plots = FALSE`, which the original
  built regardless before gating only the `print()`. Numbers are unchanged;
  `plots = FALSE` is faster and no longer needs `ggpubr`.
- **λ⋆₆ can check its own identification.** `check_connectivity = TRUE` tests
  the Prop. D.1 condition — that the activated bipartite graph is connected —
  and warns when it is not. The paper states the condition; no version of the
  code tested it, so a benchmark decomposing into near-disjoint skills would
  have produced L⋆₆(i) values comparable only within components, with nothing
  to indicate it.
- **The τ → SNR inversion is completed.** The plotting code computed the
  intermediate `greiner_kcor = sin(pi/2 * kcor)` and stopped. `tau_to_snr()`
  implements Eq. (19) in full, so Fig. 1B can be drawn against the theoretical
  curve rather than leaving the comparison to the eye.
- **The facet G-study formula is generated, not hand-written.** `facet_terms()`
  enumerates all main effects and interactions up to order 3 from the four
  facet definitions. Verified set-equal to the original's 14 hand-written terms.
  The original's `m1` and `mbase` specifications differed in their facet
  definitions — `base` + `author` versus `architecture:generation` +
  `author:removed:not_avail` — with nothing recording that the two models were
  not the same model; `FACETS` now states the definitions once.

## Structural changes

- **Functions and drivers are separated.** `R/` has no side effects, so
  `source("R/load_all.R")` gets the toolkit without launching an analysis. The
  originals interleaved definitions with 200-line driver loops.
- **`if (rerun...)` flags became command-line arguments.** Stages were gated
  behind hand-edited booleans (`rerunbootstraps`, `rerungstudies`,
  `rerunitemsamples`), one of which was assigned at two different points in the
  same file. Each stage now takes `--flags`, and skips work whose output file
  already exists.
- **Paths are derived from the project root.** The originals mixed
  `here("benchmark_reliability/data/...")`, bare `here("data/...")`, and
  absolute paths under `/Users/mhardy/`, so some scripts ran only on one
  machine from one working directory.
- **Dependencies are declared once,** in `DESCRIPTION`. Each original script
  carried a 30–40 line `library()` preamble; `data.table` was loaded four times
  in one file, `future` three times, and `tictoc`, `mokken`, `cowplot`,
  `ggthemes`, `ggpubr` and the `collapse` verbs were used without being loaded
  anywhere.
- **Legacy scripts moved out of `R/`.** They had been sitting beside the new
  modules, and anything that sourced `R/*.R` wholesale would execute the entire
  old pipeline — data load, bootstrap loops and all — as a side effect of
  loading functions. `R/load_all.R` matches only `NN_*.R`.
- **92 unit tests added.** There were none.

## Not addressed

- `item_similarity_enhanced()` supplies the "bonus" binary-similarity criteria
  (Jaccard, F1, Forbes, and the `star_*` / `hardy_*` families) and is not in
  this repository. Stage 10 uses it when it is on the search path and proceeds
  without it otherwise — the same `exists()` check the original used, which is
  why some saved result files carry those columns and others do not. Several
  entries in `REVERSE_CODED_METRICS` and `METRIC_LABELS` refer to them.
- `legacy/kendall_tau_replication_yauney.R` (the Yauney et al. comparison) was
  not folded into the pipeline; it is a standalone replication of related work
  rather than part of this paper's analysis.
- `metric_bootstraps_plots_experiments.R` held exploratory variants
  (`make_ggstudy_df()`, `make_irt_df()`) with no path into the reported results.
  They stay in `legacy/`.
