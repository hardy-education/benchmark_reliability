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

For a complete M × n matrix, the REML estimates of the crossed model
`resp ~ 1 + (1|item) + (1|model)` are the ANOVA estimates

    σ²_M = (MS_M − MS_R)/n,   σ²_I = (MS_I − MS_R)/M,   σ²_R = MS_R.

When one of these is negative, REML sets it to zero and pools its sum of
squares into the residual (`.balanced_reml()`).

Along an item order, the prefix sums of squares come from row cumulative sums
r_m(n):

    SS_M(n) = Σ_m r_m(n)²/n − S(n)²/(Mn),   SS_I(n) = Σ_{j≤n} c_j²/M − S(n)²/(Mn).

So all N − 1 prefixes cost a single O(MN) sweep. The empirical relative
reliability of a complete prefix equals 1 − MS_R/MS_M, which is Cronbach's α
of the selected items in the measured population.

Prefix totals are kept uncentred (exact integers for 0/1 data) when computing
Kendall's τ-b, because centring introduces floating-point noise that breaks
exact ties between models.
