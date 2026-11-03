# Migration from the legacy scripts

Sources: `refactoring_code/metric_bootstraps v2.R`, `topk_q_gstudies.R`,
`plot_all_code v2.R`, `lambda_star_full.R`, and the earlier refactor in
`benchmark_reliability/`.

## Names

| Legacy column | `benchrel` method | Label |
|---|---|---|
| `L6` (fixest R²) | `lambda_star` | Lambda 6* |
| `diff` | `difficulty` | Raw Difficulty |
| `a` (mirt 2PL) | `irt_a` | 2PL discrimination |
| `CA` | `ciic` | Corrected Interitem Corr. |
| `Hi` (mokken) | `loevinger_hi` | Loevinger's Hi |
| `rand`, `rand2`… | `random` (draws 1…n) | Random |
| split `train` | population `bottom_half` | Bottom 50% |
| split `test` | population `top_half` | Top 50% |
| ntile `q10` | population `top_frac:0.1` | Top 10% |

## Behaviour changes

1. **No bootstrap over item subsets.** The legacy pipeline scored items on
   random subsets of 1–200 items × 10 draws. It then removed draw fixed effects
   (`feols(metric ~ 1 | iter^boot^benchmark)`) and averaged, because λ⋆₆ by
   regression was too slow on full benchmarks. `lambda_star()` is now exact and
   instant, so every metric is computed once on the full item set of the
   reference population. The resulting orderings differ from the legacy
   residualized ones by design: they are the paper's estimand rather than an
   approximation to it.
2. **`CA` now follows Eq. 34** (item–rest r²/α-if-deleted). The legacy column
   used psych `r.cor`, which is SMC-corrected and ill-defined when M < I.
3. **Every prefix length.** Curves are computed at every n = 2…N rather than
   on a ~100-point grid. The legacy `nis` grid can still be passed as `steps`.
4. **Items constant in the reference population** are ranked last (in random
   order) for every method, including difficulty. Legacy `diff` was computed on
   all items, so items all models failed in `train` were selected first.
5. **Missing cells** (boolq, legalbench, wikifact only): `handle_missing("auto")`
   drops whichever of incomplete items or incomplete models loses fewer cells,
   so the exact estimators apply. Legacy behaviour (lmer on the observed cells)
   remains available with `missing = "none"`, which makes `variance_components()`
   and `reliability_curve()` use `lmer`.
6. **Populations are configurable** for both the item-scoring (reference) and
   evaluation (measured) roles. Legacy hard-coded scoring on `train` and
   evaluation on `train`/`test`/`q10`.
7. **Ties** in population cut-offs are broken by model name, and ties in item
   scores by a seeded random draw. Both make results deterministic.

## Bugs in the legacy code that the new code avoids

* `make_gstudy_df()` chose an optimizer and then hard-coded `"bobyqa"`.
* `metric_bootstraps v2.R` read `mirt_results$itdfs` / `$moddfs`, which do not
  exist (`itdf` / `moddf`), so IRT item tables were `NULL` in saved draws.
* `metrics_for_study` was defined twice, and the second definition replaced
  the first.
* `estimate_lambda6_components()` ended with `plan(sequential)`, which
  de-parallelised everything after it.
* `make_sample_response(samp = "test")` without `items` returned zero rows.
