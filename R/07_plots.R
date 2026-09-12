# ==============================================================================
# 07_plots.R -- figure builders for the paper.
#
# One function per panel, each taking a prepared data frame and returning a
# ggplot. Composition into the multi-panel figures happens in
# scripts/60_figures.R.
#
# The original plot_all_code.R was 2,174 lines for roughly a dozen figures,
# because each plot carried its own inline copy of:
#   * a 13-branch `if_else` chain renaming metrics for display,
#   * a 20-expression `mutate()` recomputing every D-study coefficient,
#   * four mutually-overriding `ggthemes::scale_*` calls, and
#   * an 8-line `theme()` block.
# Those four things are now `METRIC_LABELS`, `dstudy_project()` (R/02_gtheory.R),
# `scale_br_colour()` and `theme_br()` (R/00_setup.R) respectively.
# ==============================================================================

# ------------------------------------------------------------------------------
# Display labels
# ------------------------------------------------------------------------------

#' Display names for the item-selection criteria
#'
#' Replaces the chain of `if_else(metric == "...", "...", metric)` calls that
#' was pasted into every plot. Because it was pasted, the versions drifted:
#' `L6` was labelled "Lambda 6" in one place and "Lambda 6*" in another, for the
#' same quantity. The starred form is correct -- `L6` in the results tables is
#' lambda*_6, the paper's new estimator, not classical Guttman lambda_6.
METRIC_LABELS <- c(
  rand          = "Random",
  a             = "2PL discrimination",
  MS            = "Molenaar-Sijtsma",
  L6            = "Lambda 6*",
  L6d           = "Lambda 6* (D)",
  d_miso_cov    = "Miso",
  diff          = "Raw difficulty",
  CA            = "Corrected inter-item corr.",
  cralpha_rv    = "ISU metric",
  forbes_adj    = "Forbes adjusted",
  std_error_rv  = "Lambda 6* precision",
  war2_pgl6_rv  = "Lambda 6* uniqueness",
  Zh_rv         = "Zh uniqueness",
  Hi            = "Loevinger H_i",
  Zi            = "Mokken Z_i",
  itcor         = "Item-total corr.",
  snr_alpha     = "Alpha SNR",
  ctt_disc      = "CTT discrimination",
  G2            = "G2 misfit"
)

#' Apply the display labels, leaving unmapped criteria unchanged
label_metrics <- function(df, col = "metric", keep_raw = TRUE) {
  raw <- df[[col]]
  if (keep_raw) df$mtc <- raw
  labelled <- METRIC_LABELS[raw]
  df[[col]] <- dplyr::coalesce(unname(labelled), raw)
  df
}

#' Display labels for the model populations
#'
#' The stored labels are `train` / `test` / `rand`, which read as a fitting
#' split. They are not one: see `make_hilo_splits()`.
POPULATION_LABELS <- c(
  train = "Sample of Worst Models",
  test  = "Sample of Best Models",
  rand  = "Random Model Sample"
)

label_populations <- function(df, col = "samp") {
  df[[col]] <- factor(df[[col]], levels = names(POPULATION_LABELS),
                      labels = POPULATION_LABELS)
  df
}

# The benchmark subset shown in the main-text figures; the appendix shows all 18.
FIGURE_BENCHMARKS <- c("arc_challenge", "bbh", "gpqa", "mmlu_pro", "musr")

# ------------------------------------------------------------------------------
# Assembling the analysis frame
# ------------------------------------------------------------------------------

#' Join D-study results to empirical rank concordance and derive coefficients
#'
#' This is the function that replaces the two verbatim copies of the same
#' 60-line block (`pldf` and `gpldf`) in plot_all_code.R. They differed only in
#' which saved result set they read and in whether one `fmean()` passed
#' `na.rm = TRUE`, so the parameterisation here is the source frame plus that
#' flag.
#'
#' @param dstudy Variance components per (benchmark, metric, samp, n_items).
#' @param kendall Empirical `kcor` per (benchmark, metric, samp, n_items).
#' @param model_counts Per-benchmark `ntrain` / `ntest` model counts.
#' @param metrics Criteria to retain.
#' @param na_rm Passed to the Kendall averaging.
#' @return One row per (benchmark, metric, samp, n_items), carrying the raw
#'   variance components, every D-study coefficient, the empirical tau, and the
#'   tau the reliability implies.
build_reliability_frame <- function(dstudy, kendall, model_counts,
                                    metrics = NULL, na_rm = TRUE) {
  kendall_avg <- kendall |>
    dplyr::filter(samp %in% c("test", "train")) |>
    pool_random_metrics() |>
    dplyr::group_by(n_items, benchmark, metric, samp) |>
    dplyr::summarise(
      dplyr::across(dplyr::any_of(c("kcor", "dcor")),
                    \(x) mean(x, na.rm = na_rm)),
      .groups = "drop"
    )

  out <- dstudy |>
    pool_random_metrics() |>
    dplyr::select(-dplyr::any_of("kcor")) |>
    dplyr::inner_join(kendall_avg,
                      by = c("benchmark", "metric", "samp", "n_items")) |>
    dplyr::inner_join(model_counts, by = "benchmark")

  if (!is.null(metrics)) out <- dplyr::filter(out, metric %in% metrics)

  # Number of models in the evaluated population: the stronger/weaker halves
  # have different sizes, and the reliability of a *mean over models* depends
  # on it (see `dstudy_project()`).
  out <- out |>
    dplyr::mutate(n_models_pop = dplyr::if_else(samp == "test", ntest, ntrain))

  out |>
    dstudy_project(n_items = out$n_items, n_models = out$n_models_pop) |>
    dplyr::mutate(
      # Gaussian-copula correlation implied by the observed tau (Lemma C.1),
      # and the SNR that implies (Eq. 19).
      rho_from_tau = tau_to_rho(kcor),
      snr_from_tau = tau_to_snr(kcor),
      tau2         = kcor^2
    ) |>
    label_metrics() |>
    dplyr::arrange(n_items)
}

#' Read a directory of saved per-budget D-study results
#'
#' Each file holds the results for one item budget, with the budget encoded in
#' the file name as `_n<k>_`. This loader appeared three times in
#' plot_all_code.R, once per result family.
#'
#' @param pattern Regex matching the file family.
#' @param dir Directory to search.
read_dstudy_results <- function(pattern, dir = BR_BOOTS) {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (!length(files)) {
    stop(glue("No result files matching '{pattern}' in {dir}. Run scripts/30_gd_studies.R first."))
  }
  purrr::map(files, function(f) {
    n_items <- stringr::str_extract(f, "_n[0-9]+_") |>
      stringr::str_remove_all("_n|_") |>
      as.integer()
    readRDS(f) |> dplyr::mutate(n_items = n_items)
  }) |>
    purrr::list_rbind()
}

# ------------------------------------------------------------------------------
# Figure 1 -- reliability under distribution shift
# ------------------------------------------------------------------------------

#' Fig. 1A: score distributions by benchmark and model population
plot_score_distributions <- function(splits, benchmarks = FIGURE_BENCHMARKS) {
  splits |>
    dplyr::filter(benchmark %in% .env$benchmarks) |>
    label_populations("split") |>
    ggplot(aes(x = resp, fill = split)) +
    geom_histogram(bins = 30) +
    facet_wrap(~benchmark, scales = "free", nrow = 1) +
    scale_br_fill() +
    labs(title = "Score distributions by benchmark and model population",
         x = "Percent correct", y = "Count") +
    theme_br()
}

#' Fig. 1B: empirical squared tau against the SNR-implied reliability
#'
#' The x-axis is `SNR / (1 + SNR)` -- the reliability the variance components
#' imply -- and the y-axis is the observed `tau^2`. The reference line is
#' Eq. (18), so points on the line mean the Gaussian-copula mapping holds and
#' departures are the diagnostic the paper asks for.
#'
#' The original panel plotted the two quantities against each other with no
#' reference curve, which left "validating the theoretical SNR-rank mapping"
#' to the reader's eye.
plot_tau2_vs_snr <- function(frame, benchmarks = FIGURE_BENCHMARKS) {
  reference <- tibble::tibble(g = seq(0, 1, length.out = 200)) |>
    dplyr::mutate(tau2 = g_to_expected_tau(g)^2)

  frame |>
    dplyr::filter(benchmark %in% .env$benchmarks) |>
    dplyr::mutate(implied_reliability = snr_to_g(m_rel_D_snr)) |>
    ggplot(aes(implied_reliability, tau2)) +
    geom_line(data = reference, aes(g, tau2),
              inherit.aes = FALSE, linewidth = 0.5, colour = "grey40") +
    geom_point(aes(colour = metric), alpha = 0.7, size = 1.2) +
    facet_wrap(~benchmark, nrow = 1) +
    scale_br_colour() +
    labs(
      title = expression(paste("Empirical ", tau^2,
                               " against the SNR-implied reliability")),
      subtitle = "Grey curve: Eq. (18), the Gaussian-copula prediction",
      x = expression(SNR(Phi) / (1 + SNR(Phi))),
      y = expression(tau^2)
    ) +
    theme_br()
}

#' Fig. 1C: empirical rank reliability against item count, by model population
plot_rank_reliability_by_population <- function(kendall,
                                                benchmarks = FIGURE_BENCHMARKS,
                                                max_items = 250L,
                                                threshold = 0.8) {
  kendall |>
    pool_random_metrics() |>
    dplyr::filter(metric == "rand", n_items <= .env$max_items,
                  benchmark %in% .env$benchmarks) |>
    label_populations("samp") |>
    ggplot(aes(n_items, kcor, colour = samp, fill = samp, linetype = samp)) +
    geom_hline(yintercept = threshold, linewidth = 0.4, colour = "darkred") +
    geom_smooth(linewidth = 0.6, method = "loess", formula = y ~ x, se = TRUE) +
    facet_wrap(~benchmark, nrow = 1, scales = "free") +
    scale_br_colour() +
    scale_br_fill() +
    labs(
      title = "Empirical rank reliability by model population",
      x = "Number of items", y = expression(paste("Kendall's ", tau))
    ) +
    theme_br()
}

# ------------------------------------------------------------------------------
# Figure 2 -- reliability against item count
# ------------------------------------------------------------------------------

#' Fig. 2: estimated model-ranking reliability against item count
#'
#' The D-study curve of Eq. (3) on a log item axis, with the 0.9 threshold
#' marked. Curves end where a benchmark runs out of items.
plot_reliability_vs_items <- function(curve, threshold = 0.9) {
  curve |>
    ggplot(aes(n_items, m_rel_D, colour = benchmark, linetype = benchmark)) +
    geom_hline(yintercept = threshold, linewidth = 0.4, colour = "darkred") +
    geom_line(linewidth = 1) +
    scale_x_log10() +
    scale_br_colour() +
    labs(
      title = "Estimated model-ranking reliability vs. number of items",
      x = "Number of items", y = "Estimated model-ranking reliability"
    ) +
    theme_br(legend = "bottom")
}

# ------------------------------------------------------------------------------
# Figure 3 -- item selection at fixed budget
# ------------------------------------------------------------------------------

#' One panel of Fig. 3: a reliability quantity against budget, by criterion
#'
#' @param frame Output of `build_reliability_frame()`.
#' @param y Column to plot: `m_dep_D` for Fig. 3A, `m_dep_D_snr` for 3B,
#'   `kcor` for 3C.
#' @param y_label Axis label.
#' @param benchmarks Benchmarks to facet over.
#' @param population Which model population to show.
#' @param max_items Right-hand limit.
plot_selection_panel <- function(frame, y, y_label,
                                 benchmarks = FIGURE_BENCHMARKS,
                                 population = "test",
                                 max_items = 300L,
                                 log_x = TRUE) {
  # `.env$` on every argument: the frame carries its own `population` column
  # (added by stage 30), so a bare `samp == population` silently compares the
  # column against itself and filters everything out. Data masking makes that
  # failure mode invisible -- you get an empty frame, not an error -- so the
  # arguments are qualified rather than relying on the frame's column names
  # never colliding with them.
  d <- frame |>
    dplyr::filter(samp == .env$population,
                  benchmark %in% .env$benchmarks,
                  n_items <= .env$max_items) |>
    dplyr::group_by(n_items, benchmark, metric) |>
    dplyr::summarise(y = stats::median(.data[[y]], na.rm = TRUE), .groups = "drop")

  if (nrow(d) == 0L) {
    stop(glue(
      "No rows for population '{population}' and benchmarks ",
      "{paste(benchmarks, collapse = ', ')}. Available populations: ",
      "{paste(unique(frame$samp), collapse = ', ')}; benchmarks: ",
      "{paste(unique(frame$benchmark), collapse = ', ')}."
    ))
  }

  p <- ggplot(d, aes(n_items, y, colour = metric)) +
    geom_line(linewidth = 0.7) +
    facet_wrap(~benchmark, nrow = 1, scales = "free_y") +
    scale_br_colour() +
    labs(x = "Number of items", y = y_label) +
    theme_br()
  if (log_x) p <- p + scale_x_log10()
  p
}

# ------------------------------------------------------------------------------
# Figure 4 -- lambda*_6 against classical lambda_6
# ------------------------------------------------------------------------------

#' Fig. 4: lambda*_6 compared with classical Guttman lambda_6
#'
#' The point of the panel is the `M << I` regime: classical lambda_6 saturates
#' at 1 once there are fewer models than items, while lambda*_6 stays
#' informative. The 1:1 line makes the saturation visible.
#'
#' @param comparison Frame with `benchmark`, `lambda_star`, `lambda6`,
#'   `n_items`, `n_models`.
plot_lambda_comparison <- function(comparison) {
  comparison |>
    dplyr::mutate(regime = dplyr::if_else(n_models <= n_items,
                                          "M <= I (lambda_6 ill-posed)",
                                          "M > I")) |>
    ggplot(aes(lambda_star, lambda6, colour = regime, label = benchmark)) +
    geom_abline(slope = 1, intercept = 0, colour = "grey50", linewidth = 0.4) +
    geom_point(size = 2.5) +
    ggrepel::geom_text_repel(size = 3, show.legend = FALSE) +
    scale_br_colour() +
    labs(
      title = expression(paste(lambda[6]^"*", " against classical Guttman ",
                               lambda[6])),
      x = expression(lambda[6]^"*"), y = expression(lambda[6])
    ) +
    theme_br(legend = "bottom")
}
