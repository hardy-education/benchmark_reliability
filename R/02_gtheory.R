# ==============================================================================
# 02_gtheory.R -- variance decomposition, G/D-study coefficients, and the
#                 SNR <-> Kendall's tau mapping.
#
# This is the mathematical core of the paper (Sec. 3.1, App. C, App. F). Every
# reliability number reported anywhere in the pipeline is produced by one of the
# functions below, so the formulas appear exactly once and each is annotated
# with the equation it implements.
#
# In the original code these formulas were written inline, by hand, inside
# `mutate()` calls in five different files -- and the D-study block of roughly
# twenty such expressions was copy-pasted verbatim between `pldf` and `gpldf`
# in plot_all_code.R. Having one definition per quantity is the main reason
# this module exists.
# ==============================================================================

# ------------------------------------------------------------------------------
# Reliability <-> SNR (paper Eqs. 4-5)
# ------------------------------------------------------------------------------

#' Convert a generalizability coefficient to a signal-to-noise ratio
#'
#' Implements the monotone transform of Eq. (5): SNR = G / (1 - G).
#'
#' @param g Generalizability coefficient in [0, 1).
#' @return SNR, or `Inf` where `g == 1`.
#' @examples
#' g_to_snr(0.5)  # 1
g_to_snr <- function(g) {
  g <- pmin(pmax(g, 0), 1)
  ifelse(g >= 1, Inf, g / (1 - g))
}

#' Convert a signal-to-noise ratio to a generalizability coefficient
#'
#' Implements Eq. (5): G = SNR / (1 + SNR).
#'
#' @param snr Non-negative signal-to-noise ratio.
snr_to_g <- function(snr) {
  snr <- pmax(snr, 0)
  ifelse(is.infinite(snr), 1, snr / (1 + snr))
}

#' Standard error of measurement tolerance implied by a reliability
#'
#' `sqrt((1 - G) / G)`, the error-tolerance ratio reported alongside the
#' G-coefficients in the D-study tables.
g_to_error_tolerance <- function(g) {
  g <- pmin(pmax(g, 0), 1)
  ifelse(g <= 0, Inf, sqrt((1 - g) / g))
}

# ------------------------------------------------------------------------------
# SNR <-> expected Kendall concordance (paper Thm. C.2, Eqs. 17-19)
# ------------------------------------------------------------------------------

#' Expected Kendall's tau implied by a reliability coefficient
#'
#' Greiner's identity for a Gaussian copula, Eq. (6) / Eq. (17):
#'   E[tau] = (2 / pi) * arcsin(rho),  rho = SNR / (1 + SNR) = G.
#'
#' This is the theoretical curve that the empirical tau values are plotted
#' against in Fig. 1B; deviations from it are the diagnostic the paper calls for
#' (distribution shift, heteroskedasticity, non-exchangeable items).
#'
#' @param g Generalizability coefficient (equivalently, the correlation between
#'   two independent replications of the design).
#' @return Expected Kendall's tau.
g_to_expected_tau <- function(g) {
  g <- pmin(pmax(g, -1), 1)
  (2 / pi) * asin(g)
}

#' Expected Kendall's tau implied by an SNR
#'
#' Eq. (17): E[tau] = (2 / pi) * arcsin(SNR / (1 + SNR)).
snr_to_expected_tau <- function(snr) g_to_expected_tau(snr_to_g(snr))

#' Expected squared Kendall's tau implied by an SNR
#'
#' Eq. (18): E[tau]^2 = (4 / pi^2) * arcsin^2(SNR / (1 + SNR)). The squared
#' form is the "variance explained" analogue used as the y-axis of Fig. 1B.
snr_to_expected_tau2 <- function(snr) snr_to_expected_tau(snr)^2

#' Recover the implied SNR from an observed Kendall's tau
#'
#' Inverse mapping, Eq. (19): SNR = sin(pi/2 * tau) / (1 - sin(pi/2 * tau)).
#'
#' The original plotting code computed only the intermediate
#' `greiner_kcor = sin(pi/2 * kcor)` inline and never completed the inversion,
#' so the SNR axis of the empirical panels had to be reasoned about by eye.
#'
#' @param tau Observed Kendall's tau.
#' @return Implied SNR; `Inf` as tau approaches 1.
tau_to_snr <- function(tau) {
  rho <- tau_to_rho(tau)
  ifelse(rho >= 1, Inf, rho / (1 - rho))
}

#' Gaussian-copula correlation implied by a Kendall's tau
#'
#' The inverse of Greiner's identity, rho = sin(pi/2 * tau) (Lemma C.1). Named
#' `greiner_kcor` in the original plotting code.
tau_to_rho <- function(tau) sin(pi / 2 * pmin(pmax(tau, -1), 1))

# ------------------------------------------------------------------------------
# G-study: variance components from a crossed random-effects fit
# ------------------------------------------------------------------------------

#' Fit the crossed random-effects G-study model for one benchmark
#'
#' Fits paper Eq. (1), `Y_mi = mu + U_m + V_i + W_mi`, as
#' `resp ~ 1 + (1 | item) + (1 | model)`, and returns the variance components
#' together with every reliability coefficient derived from them.
#'
#' Two fixes relative to the original `make_gstudy_df()`:
#'   * the `optimizer` chosen for small item counts was computed into a local
#'     variable and then never used -- the `lmerControl()` call always hard-coded
#'     "bobyqa". The choice is now actually passed through.
#'   * a failed fit returned a one-column tibble holding only `benchmark`, which
#'     silently produced all-NA columns after `bind_rows()`. It now returns the
#'     full column set with NA variance components, so downstream `select()`
#'     calls cannot fail depending on which benchmarks happened to converge.
#'
#' @param dat Long data frame with columns `benchmark`, `item`, `model`, `resp`.
#' @param verbose Print the model summary.
#' @return A one-row tibble of variance components and coefficients.
fit_gstudy <- function(dat, verbose = FALSE) {
  b <- dat$benchmark[1]

  # Nelder-Mead is more robust than bobyqa when one facet has very few levels.
  optimizer <- if (n_unique(dat$item) < 20L) "Nelder_Mead" else "bobyqa"

  if (verbose) {
    message(glue(
      "G-study for {b}: {n_unique(dat$item)} items x {n_unique(dat$model)} models",
      " (optimizer: {optimizer})"
    ))
  }

  mod <- try(
    lme4::lmer(
      formula   = resp ~ 1 + (1 | item) + (1 | model),
      data      = dat,
      na.action = stats::na.exclude,
      control   = lme4::lmerControl(
        optimizer = optimizer,
        optCtrl   = list(maxfun = 1e5),
        calc.derivs = FALSE
      )
    ),
    silent = TRUE
  )

  if (inherits(mod, "try-error")) {
    warning(glue("G-study failed to converge for benchmark {b}; returning NA row."))
    return(gstudy_na_row(b))
  }
  if (verbose) print(summary(mod))

  vc <- lme4::VarCorr(mod) |>
    as.data.frame() |>
    janitor::clean_names() |>
    dplyr::select(grp, vcov) |>
    tidyr::pivot_wider(names_from = grp, values_from = vcov)

  gstudy_coefficients(
    var_model    = vc$model,
    var_item     = vc$item,
    var_residual = vc$Residual,
    benchmark    = b
  )
}

#' Reliability coefficients from a set of variance components
#'
#' Single-item / single-model coefficients, i.e. the G-study quantities before
#' any D-study averaging. Separated from `fit_gstudy()` so the arithmetic can be
#' unit-tested without fitting a mixed model, and so already-estimated variance
#' components (for example, ones read back from a saved fit) can be rescored.
#'
#' * `m_rel` -- relative (ranking) G-coefficient for models, Eq. (3) with I = 1:
#'   `var_model / (var_model + var_residual)`. This is the paper's G_rel.
#' * `m_dep` -- absolute (dependability) coefficient for models, which charges
#'   item variance to error as well: `var_model / (var_model + var_item + var_residual)`.
#' * `i_rel`, `i_dep` -- the same two quantities with the facets exchanged,
#'   treating items as the object of measurement.
#' * `*_snr` -- the corresponding SNRs, Eq. (4).
#'
#' @param var_model,var_item,var_residual Variance components.
#' @param benchmark Optional benchmark label carried into the output.
gstudy_coefficients <- function(var_model, var_item, var_residual,
                                benchmark = NA_character_) {
  m_rel <- var_model / (var_model + var_residual)
  i_rel <- var_item  / (var_item  + var_residual)
  total <- var_model + var_item + var_residual
  m_dep <- var_model / total
  i_dep <- var_item  / total

  tibble::tibble(
    benchmark = benchmark,
    model     = var_model,
    item      = var_item,
    Residual  = var_residual,
    m_rel = m_rel, i_rel = i_rel,
    m_dep = m_dep, i_dep = i_dep,
    m_snr = g_to_snr(m_rel), i_snr  = g_to_snr(i_rel),
    md_snr = g_to_snr(m_dep), id_snr = g_to_snr(i_dep)
  )
}

#' The NA-filled G-study row used when a fit does not converge
#'
#' Kept beside `gstudy_coefficients()` so the two can never drift out of column
#' agreement.
gstudy_na_row <- function(benchmark = NA_character_) {
  gstudy_coefficients(NA_real_, NA_real_, NA_real_, benchmark = benchmark)
}

# ------------------------------------------------------------------------------
# D-study: projecting reliability to other numbers of items and models
# ------------------------------------------------------------------------------

#' D-study projection of reliability to a given design size
#'
#' Applies the 1/I averaging law of Eq. (3) to already-estimated variance
#' components: increasing the number of items divides the residual variance by
#' the number of items, while the model variance -- the signal -- is unchanged.
#'
#' Two families of coefficient are returned, distinguished by whether the object
#' of measurement is itself averaged:
#'
#' * `m_rel_D` / `m_dep_D` -- reliability of a single model's score computed
#'   from `n_items` items. This is the quantity plotted in Fig. 2 and Fig. 3A.
#' * `m_rel_scale` / `m_dep_scale` -- reliability of a *mean over `n_models`
#'   models*, which divides every component by `n_models`. Named `*_Scale` in the
#'   original plotting code.
#'
#' @param components Data frame with columns `model`, `item`, `Residual`.
#' @param n_items Number of items in the projected design.
#' @param n_models Number of models in the projected design.
#' @return `components` with the D-study columns added.
dstudy_project <- function(components, n_items, n_models = 1L) {
  stopifnot(all(c("model", "item", "Residual") %in% names(components)))

  components |>
    dplyr::mutate(
      n_items  = .env_or_col(n_items,  dplyr::n()),
      n_models = .env_or_col(n_models, dplyr::n())
    ) |>
    dplyr::mutate(
      # --- relative (ranking) coefficients: item variance is common to all
      # models and so cannot reorder them (paper Eq. 2), leaving only the
      # model-specific residual as a source of rank instability.
      m_rel_D     = model / (model + Residual / n_items),
      m_dep_D     = model / (model + item / n_items + Residual / n_items),

      # --- averaged over n_models as well
      m_rel_scale = (model / n_models) /
        (model / n_models + Residual / (n_items * n_models)),
      m_dep_scale = (model / n_models) /
        (model / n_models + item / n_items + Residual / (n_items * n_models)),

      # --- the same, with items as the object of measurement
      i_rel_D     = item / (item + Residual / n_models),
      i_dep_D     = item / (item + model / n_models + Residual / (n_models * n_items)),
      i_rel_scale = (item / n_items) /
        (item / n_items + Residual / (n_items * n_models)),
      i_dep_scale = (item / n_items) /
        (item / n_items + model / n_models + Residual / (n_items * n_models)),

      # --- SNRs and the model-over-item dominance contrasts
      m_rel_D_snr     = g_to_snr(m_rel_D),
      m_dep_D_snr     = g_to_snr(m_dep_D),
      m_rel_scale_snr = g_to_snr(m_rel_scale),
      m_dep_scale_snr = g_to_snr(m_dep_scale),
      i_rel_D_snr     = g_to_snr(i_rel_D),
      i_dep_D_snr     = g_to_snr(i_dep_D),
      i_rel_scale_snr = g_to_snr(i_rel_scale),
      i_dep_scale_snr = g_to_snr(i_dep_scale),

      m_D_dom         = m_rel_D - i_rel_D,
      m_scale_dom     = m_rel_scale - i_rel_scale,
      m_dep_D_dom     = m_dep_D - i_dep_D,
      m_dep_scale_dom = m_dep_scale - i_dep_scale,

      # --- expected rank concordance implied by the projected reliability
      # (Eq. 17). Having this next to the empirical tau is what makes the
      # theory/observation comparison in Fig. 1B a one-line join.
      expected_tau   = g_to_expected_tau(m_rel_D),
      expected_tau2  = g_to_expected_tau(m_rel_D)^2,
      error_tolerance = g_to_error_tolerance(m_rel_D)
    )
}

# Internal: allow `n_items` to be either a constant or an existing column name.
.env_or_col <- function(x, n) if (length(x) == 1L) rep(x, n) else x

#' D-study curve over a grid of item counts
#'
#' Replaces the `for (n in c(1:20, 25, 30, ...))` loop that rebuilt the whole
#' coefficient block on every iteration in gstudy_dstudy_basic.R.
#'
#' @param components One row of variance components per benchmark.
#' @param n_items_grid Item counts to project onto.
#' @param n_models Number of models (scalar, or a column in `components`).
#' @return Long data frame, one row per (benchmark, n_items).
dstudy_curve <- function(components, n_items_grid, n_models = 1L) {
  purrr::map_dfr(n_items_grid, function(ni) {
    dstudy_project(components, n_items = ni, n_models = n_models)
  })
}
