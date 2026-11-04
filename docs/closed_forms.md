# Exact closed forms used in `benchrel`

All four expensive computations in the legacy pipeline have exact closed forms
for complete 0/1 response matrices. Each is checked against the package the
legacy code called (`tests/testthat/`). With `X` the M × I response matrix
(models × items), `p_i` the proportion of models passing item *i*, and `T_m` model
*m*'s total score:

| Quantity | Legacy implementation | Closed form | Agreement | Cost |
|---|---|---|---|---|
| λ⋆₆ item R² (Eqs. 16–18) | one `fixest::feols` per item | below | 1e-13 | O(MI) total |
| Loevinger's Hᵢ | `mokken::coefH` | below | 1e-15 | O(MI + I log I) |
| Attenuation-corrected item–rest r (Eq. 34) | `psych::alpha` | below | 1e-15 | O(MI) |
| Crossed variance components | `lme4::lmer` | balanced REML | 1e-5 (lmer tolerance) | O(MI) |
| …at every prefix n = 2…N | one `lmer` per n | cumulative sums | same | O(MN) per order |

## λ⋆₆

For target item *i* the paper fits, over all models *m* and other items *j ≠ i*,

    Y_mj = X_mi = α_i + S_mj γ_j + S_mj δ_m + η_mj,    S_mj = X_mj.

With binary S, every cell where model *m* passed item *j* (S_mj = 1) has fitted value
α_i + γ_j + δ_m. Because the response X_mi depends only on *m*, setting
δ_m = X_mi − α_i and γ_j = 0 fits all activated cells exactly. Cells with S_mj = 0
are fitted by α_i alone. Let z_mi be the number of other items model *m* failed. The
least-squares solution is then

    α_i   = Σ_m z_mi X_mi / Σ_m z_mi
    RSS_i = (Σ_m z_mi) · α_i (1 − α_i)
    TSS_i = (I − 1) · M · p_i (1 − p_i)
    L⋆₆(i) = 1 − RSS_i / TSS_i.

Writing f̄ for the mean fraction of other items failed, this is

    L⋆₆(i) = 1 − f̄ · α_i(1 − α_i) / (p_i(1 − p_i)),

where α_i is the pass rate on item *i* with each model weighted by how many
other items it failed.

**What this implies for the paper.** The other items enter L⋆₆(i) only through
each model's failure count on them, which is the rest score. The fit does not
use which items are failed together. Two consequences are worth checking
against the text before resubmission:

1. *Baseline.* If failures were spread evenly across models, α_i = p_i and
   L⋆₆(i) = 1 − f̄, which is roughly the benchmark's mean accuracy. This is why
   λ⋆₆ values cluster tightly; on GPQA (all models) the SD across items is 0.008
   around 0.288.
2. *Interaction with difficulty.* An item whose solvers are strong models has
   α_i < p_i. That raises L⋆₆ when p_i < ½ (α(1−α) < p(1−p)) but can *lower*
   it when p_i > ½, since α_i moves toward ½. So an easy item that
   discriminates well can score below the baseline.

The statements that λ⋆₆ "preserves inter-item dependence" and "keeps all items
in the estimation problem" (Sec. 4.3, App. D.3) are true of the regression as
written. They are worth qualifying, though, because the fitted R² depends on
the other items only through rest scores.

`lambda_star_feols()` keeps the regression form for non-binary scores, where
the saturation argument does not apply.

## Loevinger's Hᵢ

For dichotomous items, H_i = Σ_{j≠i} Cov(X_i, X_j) / Σ_{j≠i} Cov_max(X_i, X_j),
with Cov_max = min(p_i, p_j) − p_i p_j.

* The numerator is Cov(X_i, T − X_i), which needs one pass over X.
* In the denominator, Σ_{j≠i} min(p_i, p_j) comes from the sorted p's and a
  cumulative sum.

## Attenuation-corrected item–rest correlation (Eq. 34)

ρ²(X_i, R_i) / α(R_i), where R_i = T − X_i. Both parts follow from the item
variances, Var(T), and Cov(X_i, T):

    Var(R_i)  = Var(T) − 2 Cov(X_i, T) + Var(X_i)
    α(R_i)    = (k−1)/(k−2) · (1 − (ΣVar − Var(X_i)) / Var(R_i)).

The legacy code used psych's `r.cor` instead of `r.drop`. `r.cor` is an
item–total correlation with SMCs on the diagonal, and SMCs are not identified
when M < I. The orderings from the two versions correlate only about 0.66
(Spearman) on GPQA. `corrected_iic()` follows Eq. 34. Squaring discards the
sign, so an item that runs against the rest of the test can still rank high;
`signed = TRUE` keeps the sign.

## Variance components and prefix curves

### The REML likelihood of a balanced crossed design

For a complete M × n matrix, the REML likelihood of
`resp ~ 1 + (1|item) + (1|model)` factors into three independent strata, each
a scaled χ²:

| stratum | df | sum of squares | expected mean square λ |
|---|---|---|---|
| models | M − 1 | SS_M | λ_M = σ²_R + n σ²_M |
| items | n − 1 | SS_I | λ_I = σ²_R + M σ²_I |
| residual | (M − 1)(n − 1) | SS_R | λ_R = σ²_R |

    −2 log L_REML = Σ_s [ df_s log λ_s + SS_s / λ_s ] + const.

The constraints σ²_M, σ²_I ≥ 0 are λ_R ≤ λ_M and λ_R ≤ λ_I. In the precisions
τ_s = 1/λ_s the objective is convex (−df log τ + SS τ) and the constraints are
linear (τ_M ≤ τ_R, τ_I ≤ τ_R). So the KKT conditions identify the unique
constrained maximiser: a tree-ordered isotonic regression of the mean squares
(weights df) with the residual stratum at the root.

`.balanced_reml()` computes it as follows:

1. **No violation** (MS_M ≥ MS_R and MS_I ≥ MS_R): the ANOVA estimates
   σ²_M = (MS_M − MS_R)/n, σ²_I = (MS_I − MS_R)/M, σ²_R = MS_R.
2. **One violation**, e.g. MS_M < MS_R: set σ²_M = 0 and re-estimate the
   residual from the pooled stratum, σ²_R = (SS_M + SS_R)/(df_M + df_R). The
   remaining component is *re-estimated* against the new residual,
   σ²_I = (MS_I − σ²_R)/M; it is not just the old moment estimate. The pooled
   residual lies between MS_M and MS_R, so σ²_I stays ≥ 0.
3. **Both violated** (MS_M < MS_R and MS_I < MS_R): pool the residual with the
   stratum of *smaller* mean square first. Pooling lowers the residual, so the
   other component is checked again. If its mean square now exceeds the
   pooled residual, that component is **positive**. Only if it still falls
   below are all three strata pooled, with σ²_M = σ²_I = 0. Pooling with the
   larger mean square first can never be optimal, because its pooled value
   exceeds the smaller mean square.

So "both moment estimates negative" does not mean "both components zero".
For example, MS_I < MS_M < MS_R with MS_M > (SS_I + SS_R)/(df_I + df_R)
gives σ²_M > 0 and G > 0. Before this was fixed, the code pooled all three
strata whenever both were negative. That returned G = 0 or a wrong σ²_I
where lmer did not (`tests/testthat/test-gtheory.R` covers each case;
`analysis/99_validate_reml.R` checks the real benchmarks).

### Degenerate designs

* **Zero residual** (SS_R = 0, e.g. a handful of models that answer every
  selected item identically, or two identical item columns): −2 log L → −∞ as
  σ²_R → 0, so there is no regular maximiser. lmer silently returns
  σ²_R ≈ 1e-17 and G ≈ 1. These designs get `status = "degenerate"` and NA
  components.
* **Constant matrix**: `status = "constant"`, NA components.
* **Fewer than 2 models or items**: `status = "too_small"`.

Every estimate carries a `status` (`interior`, `boundary_model`,
`boundary_item`, `boundary_both`, or one of the three above), so plots and
summaries can show which points rest on a boundary or have no fit.

### Exact sums of squares

For 0/1 (any integer) data, with sx2 = Σx², sr2 = Σ_m r_m², sc2 = Σ_j c_j² and
grand total T:

    M n SS_M = M sr2 − T²,   M n SS_I = n sc2 − T²,   M n SS_R = M n sx2 − M sr2 − n sc2 + T².

Every term is an integer below 2^53 for all the benchmarks here (the code
checks), so doubles represent the numerators exactly. The usual
`ssr <- sum(Xc^2) - ssm - ssi` subtracts nearly equal quantities. When the
residual is small it can come out slightly negative, or turn an exact tie
between mean squares into a spurious boundary case; the test suite has a
3 × 3 matrix where it does. With exact numerators SS_R = 0 is detected
exactly and is never negative. Non-integer data falls back to centring,
clamping at zero, and a relative tolerance for "zero residual".

### Prefix curves

Along an item order the same statistics are prefix sums: r_m(n) are row
cumulative sums, and sx2(n), sc2(n) are cumulative column sums. So all N − 1
prefixes cost a single O(MN) sweep. `.balanced_reml()` is vectorised over
prefixes and recycles scalar M against the vector of n explicitly (ragged
inputs are an error, not NA). For an interior fit the empirical relative
reliability equals 1 − MS_R/MS_M, which is Cronbach's α of the selected items
in the measured population. On a boundary it does not: G = 0 when σ²_M = 0,
and the pooled residual enters when σ²_I = 0.

### Missing cells

With missing cells the row and column effects are no longer orthogonal, the
cell counts per row and column differ, and the balanced expected mean squares
do not hold. Adding `na.rm = TRUE` to the sums of squares therefore does not
give REML or any other valid estimator. `engine = "anova"` refuses incomplete
matrices; `"auto"` fits lmer to the observed cells, and the curve code does the
same per prefix. In this data only boolq, legalbench and wikifact have missing
cells, and stage 2 by default drops the cheaper of incomplete items or models
first (`handle_missing("auto")`), so the curves themselves use complete
matrices.

Prefix totals are kept uncentred (exact integers for 0/1 data) when computing
Kendall's τ-b, because centring introduces floating-point noise that breaks
exact ties between models.
