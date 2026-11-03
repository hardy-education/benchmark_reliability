# Figures. Each function takes a results table and returns a ggplot; saving is
# left to the caller (see analysis/03_figures.R).

#' Colour per selection method (Okabe-Ito; Random in grey)
METHOD_COLOURS <- c(
  "Lambda 6*" = "#D55E00", "Raw Difficulty" = "#0072B2", "2PL discrimination" = "#009E73",
  "Corrected Interitem Corr." = "#CC79A7", "Loevinger's Hi" = "#E69F00", "Random" = "grey45"
)

#' Summarise random-order draws to a mean line and a 95% band
#'
#' Non-random methods pass through with `lo = hi = value`.
#' @param curves Output of [benchmark_curves()] (one or more benchmarks).
#' @param y Column to summarise, e.g. `"g_rel"`.
summarise_curves <- function(curves, y = "g_rel") {
  keys <- intersect(c("benchmark", "reference", "measured", "method", "n_items"), names(curves))
  curves[, .(value = mean(get(y), na.rm = TRUE),
             lo = stats::quantile(get(y), 0.025, na.rm = TRUE, names = FALSE),
             hi = stats::quantile(get(y), 0.975, na.rm = TRUE, names = FALSE)), by = keys]
}

#' Empirical reliability vs number of selected items
#'
#' x = items selected (log scale), y = empirical relative reliability G(n) of
#' the selected subset in the measured population, one line per selection
#' method, one panel per benchmark. Random orders are a mean line with a 95%
#' band over draws.
#'
#' @param curves Output of [benchmark_curves()], row-bound across benchmarks.
#' @param y `"g_rel"` (default), `"g_abs"`, or `"kendall"`.
#' @param linetype_by Optional column mapped to linetype (e.g. `"measured"`
#'   when two populations share a figure).
#' @param row_by Optional column giving one facet row per value (e.g. a run
#'   label); benchmarks then become the columns of a `facet_grid()`.
plot_reliability_curves <- function(curves, y = "g_rel", linetype_by = NULL, ncol = 4, row_by = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.", call. = FALSE)
  keep_row <- if (!is.null(row_by)) unique(curves[, c(row_by, "reference", "measured"), with = FALSE])
  s <- summarise_curves(curves, y)
  if (!is.null(row_by)) s <- merge(s, keep_row, by = c("reference", "measured"))
  s <- merge(s, SELECTION_METHODS[, .(method, label)], by = "method")
  if (!is.null(linetype_by)) s[, lt := vapply(get(linetype_by), population_label, "")]
  ylab <- c(g_rel = "Empirical relative reliability G(n)", g_abs = "Empirical dependability Φ(n)",
            kendall = "Kendall τ with full-benchmark ranking")[[y]]
  aes_line <- if (is.null(linetype_by)) ggplot2::aes(n_items, value, colour = label) else
    ggplot2::aes(n_items, value, colour = label, linetype = lt)
  ggplot2::ggplot(s, aes_line) +
    ggplot2::geom_ribbon(data = s[method == "random"], ggplot2::aes(ymin = lo, ymax = hi, fill = label),
                         colour = NA, alpha = 0.2, inherit.aes = TRUE, show.legend = FALSE) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_colour_manual(values = METHOD_COLOURS, name = "Selection method") +
    ggplot2::scale_fill_manual(values = METHOD_COLOURS) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    (if (is.null(row_by)) ggplot2::facet_wrap(~benchmark, ncol = ncol, scales = "free_x") else
      ggplot2::facet_grid(stats::as.formula(paste(row_by, "~ benchmark")), scales = "free_x")) +
    ggplot2::labs(x = "Number of items selected", y = ylab) +
    (if (!is.null(linetype_by)) ggplot2::labs(linetype = "Measured population")) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank())
}

#' D-study reliability vs number of items by population (paper Fig. 2a)
#'
#' @param dstudy `population_gstudies()$dstudy`.
plot_dstudy_populations <- function(dstudy, ncol = 4) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.", call. = FALSE)
  d <- data.table::melt(dstudy, measure.vars = c("g_rel", "g_abs"), variable.name = "type", value.name = "value")
  d[, type := ifelse(type == "g_rel", "Relative", "Absolute")]
  d[, pop := vapply(population, population_label, "")]
  ggplot2::ggplot(d, ggplot2::aes(n_items, value, colour = pop, linetype = type)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_log10() +
    ggplot2::facet_wrap(~benchmark, ncol = ncol) +
    ggplot2::labs(x = "Number of items", y = "Reliability", colour = "LLM cohort", linetype = "Reliability type") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")
}
