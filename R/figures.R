# Paper figures: one function per figure type (docs/FIGURES.md), sharing one
# theme and one set of scales so that a variable always gets the same colour,
# shape and line type.
#
#   1a plot_reliability_contrast()   reliability vs items; colour = population, linetype = type
#   1b plot_selection_curves()       outcome vs items; colour/linetype = selection method
#   1c plot_dstudy()                 G-theory projection for random draws from the universe
#   2  plot_snr_r2_path()            R2_tau = sin^2(pi tau / 2) vs G(n), path over n
#   3  plot_reliability_landscape()  model vs residual variance share; class by SNR criterion
#   4  plot_population_dots()        full-benchmark G and Phi by population
#   5  plot_population_histograms()  model score distributions by population
#   6  plot_items_needed()           benchmark length vs items needed for a threshold
#
# Every function returns a ggplot without a title or caption (those belong in
# LaTeX) and draws reference lines beneath the data. save_figure() writes a
# PDF at paper size with the Times font.

#' Text width of the single-column paper, in inches
PAPER_WIDTH <- 5.5

#' Default SNR thresholds: primary 9 (reliability 0.9), secondary 4 (0.8)
SNR_THRESHOLDS <- c(9, 4)

#' Line, colour and shape per selection method (Okabe-Ito; Random in grey)
METHOD_STYLE <- data.table::data.table(
  method = c("lambda_star", "difficulty", "irt_a", "ciic", "loevinger_hi", "random"),
  colour = c("#D55E00", "#0072B2", "#009E73", "#CC79A7", "#E69F00", "#5F5F5F"),
  linetype = c("solid", "longdash", "dotdash", "twodash", "42", "22"),
  shape = c(16, 17, 15, 18, 8, 4)
)

#' Colour and shape per population type
POPULATION_STYLE <- data.table::data.table(
  type = c("all", "top_half", "bottom_half", "middle_half", "middle_frac", "top_frac", "bottom_frac",
           "top_n", "not_top_frac", "not_top_n", "random_frac", "random_comp", "early_half", "late_half"),
  colour = c("#000000", "#E69F00", "#56B4E9", "#CC79A7", "#CC79A7", "#009E73", "#0072B2",
             "#D55E00", "#56B4E9", "#88CCEE", "#8C8C8C", "#332288", "#0072B2", "#AA4499"),
  shape = c(16, 17, 15, 18, 18, 25, 22, 8, 15, 0, 1, 2, 23, 24)
)

#' Colour, line type and shape for relative vs absolute reliability
RELIABILITY_TYPE_STYLE <- data.table::data.table(
  type = c("g_rel", "g_abs"), label = c("Relative", "Absolute"),
  colour = c("#0072B2", "#D55E00"), linetype = c("solid", "22"), shape = c(16, 17)
)

#' Colour and shape per reliability-landscape class
LANDSCAPE_STYLE <- data.table::data.table(
  category = c("both", "relative", "neither"),
  label = c("Absolute and relative", "Relative only", "Neither"),
  colour = c("#009E73", "#0072B2", "#D55E00"), shape = c(17, 18, 6)
)

.fallback_colours <- c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#882255", "#661100", "#AA4499")

#' The shared ggplot2 theme of the paper figures
#'
#' @param base_size Base font size in points at the saved size (10 pt prints
#'   at about caption size when the figure is placed at `\linewidth`).
#' @param base_family Font family; `"Times"`.
theme_benchrel <- function(base_size = 10, base_family = "Times") {
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "grey92", linewidth = 0.25),
      panel.border = ggplot2::element_rect(colour = "grey70", fill = NA, linewidth = 0.3),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = base_size, margin = ggplot2::margin(1, 1, 2, 1)),
      axis.text = ggplot2::element_text(size = base_size - 2, colour = "grey15"),
      axis.ticks = ggplot2::element_line(colour = "grey60", linewidth = 0.25),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.title = ggplot2::element_text(size = base_size),
      legend.text = ggplot2::element_text(size = base_size - 1),
      legend.key.width = ggplot2::unit(1.8, "lines"),
      legend.key.height = ggplot2::unit(0.9, "lines"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.spacing = ggplot2::unit(3, "pt"),
      legend.spacing.y = ggplot2::unit(1, "pt"),
      panel.spacing = ggplot2::unit(5, "pt"),
      plot.margin = ggplot2::margin(3, 5, 2, 2)
    )
}

# ---------------------------------------------------------------------------
# Scales
# ---------------------------------------------------------------------------

#' Legend labels for selection methods (plotmath for lambda-star-6 and H_i)
#' @param wrap Break the two longest labels over two lines (narrow legends).
method_labels <- function(methods, wrap = FALSE) {
  lab <- lapply(methods, function(m) switch(m,
    lambda_star = quote(lambda[6]^"*"),
    difficulty = "Raw difficulty",
    irt_a = if (wrap) "2PL\ndiscrimination" else "2PL discrimination",
    ciic = if (wrap) "Inter-item\ncorr." else "Inter-item corr.",
    loevinger_hi = quote("Loevinger's" ~ H[i]),
    random = "Random",
    m))
  stats::setNames(lab, methods)
}

#' Matching colour, fill, line-type and shape scales for selection methods
#'
#' Methods not in [METHOD_STYLE] (your own criteria) get colours from a
#' colour-blind-safe fallback palette and a solid line.
#' @param methods Method codes present in the plot, in legend order.
#' @param aesthetics Which scales to return.
scale_method <- function(methods, aesthetics = c("colour", "fill", "linetype", "shape"), name = "Selection method",
                         wrap = FALSE) {
  st <- METHOD_STYLE[match(methods, method)]
  unknown <- is.na(st$method)
  st$method <- methods
  st$colour[unknown] <- rep_len(.fallback_colours, sum(unknown))
  st$linetype[unknown] <- "solid"
  st$shape[unknown] <- rep_len(c(0, 2, 5, 6, 3), sum(unknown))
  labs <- method_labels(methods, wrap)
  # Plotmath labels must be expressions for the legend to parse them.
  labs <- lapply(labs, function(l) if (is.language(l)) as.expression(l) else l)
  labs <- do.call(c, unname(labs))
  vals <- list(colour = st$colour, fill = st$colour, linetype = st$linetype, shape = st$shape)
  lapply(aesthetics, function(a) ggplot2::scale_discrete_manual(
    a, values = stats::setNames(vals[[a]], methods), breaks = methods, labels = labs, name = name, drop = TRUE))
}

#' Matching colour, fill and shape scales for populations
#' @param populations Population strings present in the plot, in legend order.
scale_population <- function(populations, aesthetics = c("colour", "fill", "shape"), name = "Population",
                             labels = population_label(populations)) {
  types <- vapply(populations, function(p) parse_population(p)$type, "")
  st <- POPULATION_STYLE[match(types, type)]
  dup <- duplicated(st$colour)
  st$colour[dup] <- rep_len(.fallback_colours, sum(dup))
  vals <- list(colour = st$colour, fill = st$colour, shape = st$shape)
  lapply(aesthetics, function(a) ggplot2::scale_discrete_manual(
    a, values = stats::setNames(vals[[a]], populations), breaks = populations, labels = labels, name = name))
}

#' Matching scales for relative vs absolute reliability
scale_reliability_type <- function(types = c("g_rel", "g_abs"), aesthetics = c("colour", "linetype", "shape"),
                                   name = "Reliability") {
  st <- RELIABILITY_TYPE_STYLE[match(types, type)]
  vals <- list(colour = st$colour, fill = st$colour, linetype = st$linetype, shape = st$shape)
  lapply(aesthetics, function(a) ggplot2::scale_discrete_manual(
    a, values = stats::setNames(vals[[a]], types), breaks = types, labels = st$label, name = name))
}

#' Value of an SNR threshold on another scale
#'
#' @param snr SNR thresholds.
#' @param scale `"g"` (reliability, SNR / (1 + SNR)), `"tau"` (Kendall's tau
#'   between an n-item subset and the full benchmark, (2/pi) asin(sqrt(G))),
#'   `"tau_rep"` (tau between independent replications, (2/pi) asin(G)), or
#'   `"snr"`.
snr_threshold_value <- function(snr, scale = c("g", "tau", "tau_rep", "snr")) {
  scale <- match.arg(scale)
  g <- snr / (1 + snr)
  switch(scale, g = g, tau = (2 / pi) * asin(sqrt(g)), tau_rep = (2 / pi) * asin(g), snr = snr)
}

#' Reference lines for SNR thresholds, drawn beneath the data
#'
#' Grey; the first threshold solid, the second dotted, further ones dashed.
#' @param thresholds SNR values (first = primary).
#' @param scale See [snr_threshold_value()].
#' @param orientation `"h"` (horizontal lines) or `"v"`.
snr_reference_lines <- function(thresholds = SNR_THRESHOLDS, scale = "g", orientation = c("h", "v")) {
  orientation <- match.arg(orientation)
  if (!length(thresholds)) return(list())
  v <- snr_threshold_value(thresholds, scale)
  lt <- rep_len(c("solid", "11", "22", "4212"), length(v))
  lapply(seq_along(v), function(i) {
    if (orientation == "h") ggplot2::geom_hline(yintercept = v[i], colour = "grey55", linewidth = 0.35, linetype = lt[i])
    else ggplot2::geom_vline(xintercept = v[i], colour = "grey55", linewidth = 0.35, linetype = lt[i])
  })
}

.log_items_axis <- function(name = "Number of items") {
  ggplot2::scale_x_log10(name = name, breaks = 10^(0:5), minor_breaks = NULL,
                         labels = c("1", "10", "100", "1k", "10k", "100k"))
}

.outcome_info <- function(outcome) {
  switch(outcome,
    g_rel = list(lab = "Relative reliability G(n)", scale = "g", range = c(0, 1)),
    g_abs = list(lab = quote("Absolute reliability " * Phi(n)), scale = "g", range = c(0, 1)),
    kendall = list(lab = quote("Kendall " * tau * " with full-benchmark ranking"), scale = "tau", range = c(0, 1)),
    r2_tau = list(lab = expression(R[tau]^2 == sin^2*(pi*tau/2)), scale = "g", range = c(0, 1)),
    snr_rel = list(lab = "Relative SNR", scale = "snr", range = NULL),
    snr_abs = list(lab = "Absolute SNR", scale = "snr", range = NULL),
    stop("Unknown outcome '", outcome, "'.", call. = FALSE))
}

# Add derived outcome columns a curve table may lack.
.with_outcomes <- function(curves) {
  cv <- data.table::copy(curves)
  if (!"snr_abs" %in% names(cv)) cv[, snr_abs := n_items * var_model / (var_item + var_residual)]
  if ("kendall" %in% names(cv)) cv[, r2_tau := sin(pi / 2 * pmax(kendall, 0))^2]
  cv
}

# Mean over random draws (and a 95% band) at each n; other methods pass through.
.summarise_draws <- function(cv, y, keys = c("benchmark", "reference", "measured", "method", "n_items")) {
  keys <- intersect(keys, names(cv))
  cv[, .(value = mean(get(y), na.rm = TRUE),
         lo = stats::quantile(get(y), 0.025, na.rm = TRUE, names = FALSE),
         hi = stats::quantile(get(y), 0.975, na.rm = TRUE, names = FALSE)), by = keys][is.finite(value)]
}

.facet_benchmarks <- function(d, ncol, scales = "free_x") {
  lev <- unique(d$benchmark)
  d[, benchmark_lab := factor(benchmark_label(benchmark), levels = benchmark_label(lev))]
  ggplot2::facet_wrap(~benchmark_lab, ncol = ncol, scales = scales)
}

# Lines for the curve figures: smoothed (default), raw, or both.
.curve_layers <- function(lines, band_data = NULL) {
  lines <- match.arg(lines, c("smooth", "raw", "both"))
  smooth <- ggplot2::geom_smooth(ggplot2::aes(fill = colour_key), method = NULL, formula = NULL,
                                 linewidth = 0.7, alpha = 0.18, na.rm = TRUE)
  raw <- ggplot2::geom_line(linewidth = if (lines == "both") 0.3 else 0.55,
                            alpha = if (lines == "both") 0.45 else 1, na.rm = TRUE)
  ribbon <- if (!is.null(band_data) && nrow(band_data) && lines == "raw")
    ggplot2::geom_ribbon(data = band_data, ggplot2::aes(ymin = lo, ymax = hi, fill = colour_key),
                         colour = NA, alpha = 0.15, show.legend = FALSE)
  switch(lines, smooth = list(smooth), raw = list(ribbon, raw), both = list(raw, smooth))
}

# ---------------------------------------------------------------------------
# 1b  Item-selection contrasts
# ---------------------------------------------------------------------------

#' Outcome vs number of selected items, one line per selection method (type 1b)
#'
#' Items are added in the order each method gives in the reference
#' population; the outcome is measured in the measured population. Random is
#' the mean over draws at each n.
#'
#' @param curves Curves for one reference -> measured pair ([cached_curves()]).
#' @param outcome `"g_rel"`, `"g_abs"`, `"kendall"`, `"r2_tau"`, `"snr_rel"`
#'   or `"snr_abs"`.
#' @param methods Methods to show, in legend order (default: all present).
#' @param thresholds SNR thresholds for the reference lines (first = primary).
#' @param lines `"smooth"` (default: `geom_smooth()` at ggplot2's defaults on
#'   the log-x scale, with its standard-error band), `"raw"` (item-by-item
#'   curves; random with a 95% band over draws) or `"both"`.
#' @param ncol Facet columns.
#' @param pair_rows `TRUE` to lay out one row per reference -> measured pair
#'   (benchmarks as columns), for curves row-bound across pairs.
#' @param pairs Pairs `"ref->meas"` to show as rows, in order.
#' @param mark_frac Draw a dotted vertical line at this share of each
#'   benchmark's items (e.g. 0.1 for a 10% budget).
#' @param legend_wrap Break long method labels over two lines (for a legend
#'   placed beside the panels).
plot_selection_curves <- function(curves, outcome = "g_rel", methods = NULL, thresholds = SNR_THRESHOLDS,
                                  lines = "smooth", ncol = 2, pair_rows = FALSE, pairs = NULL, mark_frac = NULL,
                                  legend_wrap = FALSE) {
  info <- .outcome_info(outcome)
  cv <- .with_outcomes(curves)
  methods <- methods %||% intersect(c(METHOD_STYLE$method, unique(cv$method)), unique(cv$method))
  s <- .summarise_draws(cv[method %in% methods], outcome)
  s[, colour_key := factor(method, levels = methods)]
  if (pair_rows) {
    s[, pair := paste0(reference, "->", measured)]
    pairs <- pairs %||% unique(s$pair)
    s <- s[pair %in% pairs]
    # Two-line row labels ("Bottom 50%" / "-> Top 50%") keep the strips narrow.
    pl <- stats::setNames(gsub(" -> ", "\n\u2192 ", population_label(pairs), fixed = TRUE), pairs)
    s[, pair_lab := factor(pl[pair], levels = pl)]
    lev <- unique(s$benchmark)
    s[, benchmark_lab := factor(benchmark_label(benchmark), levels = benchmark_label(lev))]
    fac <- ggplot2::facet_grid(pair_lab ~ benchmark_lab, scales = "free_x")
  } else {
    fac <- .facet_benchmarks(s, ncol)
  }
  # Optional marker at a fixed share of each benchmark's items (e.g. a 10% budget).
  marks <- if (!is.null(mark_frac)) {
    mk <- s[, .(x = max(2, round(mark_frac * max(n_items)))), by = intersect(c("benchmark", "benchmark_lab", "pair_lab"), names(s))]
    list(ggplot2::geom_vline(data = mk, ggplot2::aes(xintercept = x), colour = "grey55", linewidth = 0.35, linetype = "13"))
  }
  p <- ggplot2::ggplot(s, ggplot2::aes(n_items, value, colour = colour_key, linetype = colour_key, group = colour_key)) +
    snr_reference_lines(thresholds, info$scale) + marks +
    .curve_layers(lines, band_data = s[method == "random"]) +
    .log_items_axis("Number of items selected") +
    scale_method(methods, c("colour", "fill", "linetype"), wrap = legend_wrap) +
    fac + ggplot2::labs(y = info$lab) + theme_benchrel() +
    ggplot2::theme(legend.title.position = "top") +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = if (length(methods) > 4) 2 else 1))
  .y_range(p, info, outcome)
}

.y_range <- function(p, info, outcome) {
  if (!is.null(info$range)) p + ggplot2::coord_cartesian(ylim = info$range) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.25), labels = c("0", ".25", ".5", ".75", "1"))
  else p + ggplot2::scale_y_log10()
}

# ---------------------------------------------------------------------------
# 1b  One selection method across population shifts
# ---------------------------------------------------------------------------

#' Matching colour, fill and line-type scales for reference -> measured pairs
#'
#' Colour is the measured population's colour (as everywhere else); line type
#' is solid for an in-sample pair (reference = measured) and a distinct dash
#' pattern for each shift, so two pairs measured on the same population still
#' differ.
#' @param pairs Pair strings `"ref->meas"`, in legend order.
scale_pair <- function(pairs, aesthetics = c("colour", "fill", "linetype"), name = "Reference \u2192 measured") {
  pr <- lapply(pairs, parse_pair)
  same <- vapply(pr, function(x) x$reference == x$measured, NA)
  cols <- vapply(pr, function(x) .pop_colour(x$measured), "")
  lt <- character(length(pairs))
  lt[same] <- "solid"
  lt[!same] <- rep_len(c("22", "longdash", "dotdash", "twodash", "42", "1343"), sum(!same))
  labs <- vapply(pairs, function(p) {
    x <- parse_pair(p)
    if (x$reference == x$measured) population_label(x$measured)
    else paste(population_label(x$reference), "\u2192", population_label(x$measured))
  }, "", USE.NAMES = FALSE)
  vals <- list(colour = cols, fill = cols, linetype = lt)
  lapply(aesthetics, function(a) ggplot2::scale_discrete_manual(
    a, values = stats::setNames(vals[[a]], pairs), breaks = pairs, labels = labs, name = name))
}

#' One selection method's curves across reference -> measured pairs (type 1b, by shift)
#'
#' Shows how a single criterion behaves under different population changes:
#' each line scores items on a pair's reference population and measures the
#' outcome on its measured population. Random is the mean over draws (its
#' order does not depend on the reference population).
#'
#' @param curves Row-bound curves of several pairs ([cached_curves()]).
#' @param method Selection method.
#' @param outcome As in [plot_selection_curves()].
#' @param pairs Pairs `"ref->meas"` to show, in legend order (default: all present).
plot_pair_curves <- function(curves, method = "lambda_star", outcome = "g_rel", pairs = NULL,
                             thresholds = SNR_THRESHOLDS, lines = "smooth", ncol = 4) {
  info <- .outcome_info(outcome)
  m <- method
  cv <- .with_outcomes(curves[method == m])
  cv[, pair := paste0(reference, "->", measured)]
  pairs <- pairs %||% unique(cv$pair)
  s <- .summarise_draws(cv[pair %in% pairs], outcome, c("benchmark", "pair", "method", "n_items"))
  s[, colour_key := factor(pair, levels = pairs)]
  fac <- .facet_benchmarks(s, ncol)
  p <- ggplot2::ggplot(s, ggplot2::aes(n_items, value, colour = colour_key, linetype = colour_key, group = colour_key)) +
    snr_reference_lines(thresholds, info$scale) +
    .curve_layers(lines, band_data = s[0]) +
    .log_items_axis("Number of items selected") +
    scale_pair(pairs, if (identical(lines, "raw")) c("colour", "linetype") else c("colour", "fill", "linetype")) +
    fac + ggplot2::labs(y = info$lab) + theme_benchrel() +
    ggplot2::theme(legend.title.position = "top") +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = if (length(pairs) > 3) 2 else 1))
  .y_range(p, info, outcome)
}

# ---------------------------------------------------------------------------
# 1a  Reliability contrasts
# ---------------------------------------------------------------------------

#' Relative and absolute reliability vs number of items by population (type 1a)
#'
#' @param curves Curves for one or more measured populations (row-bound);
#'   only `method` is used (default random order, averaged over draws).
#' @param populations Measured populations to show, in legend order.
#' @param method Ordering to use (`"random"` for reliability as a function of
#'   budget; a selection method to show its curves).
#' @param types `c("g_rel", "g_abs")`.
#' @param labels Legend labels for `populations` (e.g. marking reference and
#'   measured).
plot_reliability_contrast <- function(curves, populations = NULL, method = "random", types = c("g_rel", "g_abs"),
                                      thresholds = SNR_THRESHOLDS, lines = "smooth", ncol = 2,
                                      labels = NULL) {
  m <- method
  cv <- curves[method == m]
  populations <- populations %||% unique(cv$measured)
  cv <- unique(cv[measured %in% populations], by = c("benchmark", "measured", "method", "draw", "n_items"))
  s <- data.table::rbindlist(lapply(types, function(t) .summarise_draws(cv, t, c("benchmark", "measured", "n_items"))[, type := t]))
  s[, `:=`(colour_key = factor(measured, levels = populations), type = factor(type, levels = types))]
  s[, grp := interaction(colour_key, type)]
  fac <- .facet_benchmarks(s, ncol)
  sm <- match.arg(lines, c("smooth", "raw", "both"))
  layers <- list(
    if (sm != "smooth") ggplot2::geom_line(linewidth = if (sm == "both") 0.3 else 0.55,
                                           alpha = if (sm == "both") 0.45 else 1, na.rm = TRUE),
    if (sm != "raw") ggplot2::geom_smooth(ggplot2::aes(fill = colour_key), method = NULL, formula = NULL,
                                          linewidth = 0.7, alpha = 0.15, na.rm = TRUE))
  p <- ggplot2::ggplot(s, ggplot2::aes(n_items, value, colour = colour_key, linetype = type, group = grp)) +
    snr_reference_lines(thresholds, "g") + layers +
    .log_items_axis() +
    scale_population(populations, c("colour", "fill"), labels = labels %||% population_label(populations)) +
    scale_reliability_type(types, "linetype") +
    fac + ggplot2::labs(y = "Empirical reliability") + theme_benchrel() +
    ggplot2::guides(fill = "none", linetype = ggplot2::guide_legend(order = 2, override.aes = list(colour = "grey15", fill = NA)),
                    colour = ggplot2::guide_legend(order = 1, override.aes = list(fill = NA)))
  .y_range(p, list(range = c(0, 1)), "g")
}

# ---------------------------------------------------------------------------
# 1c  Theoretical random draws (D-study)
# ---------------------------------------------------------------------------

#' G-theory projection of reliability for random draws of n items (type 1c)
#'
#' Holds each population's full-benchmark variance components fixed and
#' projects G(n) and Phi(n) for n items drawn at random from the same task
#' universe.
#'
#' @param gstudy [cached_gstudies()] rows (eligible ones are used).
#' @param colour_by `"population"` (panels = benchmarks) or `"benchmark"`
#'   (one panel per population).
#' @param n_grid Test lengths.
plot_dstudy <- function(gstudy, colour_by = c("population", "benchmark"), types = c("g_rel", "g_abs"),
                        thresholds = SNR_THRESHOLDS, n_grid = unique(round(10^seq(0, 4, by = 0.02))), ncol = 2) {
  colour_by <- match.arg(colour_by)
  g <- gstudy[eligible == TRUE & !is.na(var_model)]
  d <- dstudy_curve(g[, .(benchmark, population, var_model, var_item, var_residual)], n_grid)
  d <- data.table::melt(d, measure.vars = types, variable.name = "type", value.name = "value")
  d[, type := factor(as.character(type), levels = types)]
  pops <- unique(g$population)
  if (colour_by == "population") {
    d[, colour_key := factor(population, levels = pops)]
    fac <- .facet_benchmarks(d, ncol, scales = "fixed")
    sc <- scale_population(pops, "colour")
  } else {
    bl <- unique(g$benchmark)
    d[, colour_key := factor(benchmark, levels = bl)]
    d[, pop_lab := factor(population_label(population), levels = population_label(pops))]
    fac <- ggplot2::facet_wrap(~pop_lab, ncol = ncol)
    sc <- list(ggplot2::scale_colour_manual(values = stats::setNames(rep_len(c(METHOD_STYLE$colour[1:5], .fallback_colours), length(bl)), bl),
                                            labels = benchmark_label(bl), name = "Benchmark"))
  }
  d[, grp := interaction(colour_key, type)]
  p <- ggplot2::ggplot(d, ggplot2::aes(n_items, value, colour = colour_key, linetype = type, group = grp)) +
    snr_reference_lines(thresholds, "g") +
    ggplot2::geom_line(linewidth = 0.6) + .log_items_axis() + sc + scale_reliability_type(types, "linetype") +
    fac + ggplot2::labs(y = "Projected reliability") + theme_benchrel() +
    ggplot2::guides(linetype = ggplot2::guide_legend(order = 2, override.aes = list(colour = "grey15")),
                    colour = ggplot2::guide_legend(order = 1))
  .y_range(p, list(range = c(0, 1)), "g")
}

# ---------------------------------------------------------------------------
# 2  SNR - R2 path
# ---------------------------------------------------------------------------

#' Empirical R2_tau against empirical relative reliability, as items are added (type 2)
#'
#' Each point is one prefix length n: x = G(n) of the selected subset, y =
#' R2_tau = sin^2(pi tau / 2), with tau = Kendall's tau between the models'
#' subset and full-benchmark scores (Greiner's identity). Theory gives
#' R2_tau = G(n) / G(N), i.e. about the x = y line for a long benchmark;
#' departures diagnose item selection that trades rank stability for
#' variance-based reliability, or the reverse (paper App. C). Points become
#' more opaque as n grows. Random is averaged over draws before plotting.
#'
#' @param curves Curves (row-bound across pairs when colouring by population).
#' @param colour_by `"measured"` (populations) or `"method"`.
#' @param methods Methods to keep (default random only when colouring by
#'   population, all otherwise).
#' @param n_points Log-spaced prefix lengths kept per curve (keeps long
#'   benchmarks readable).
plot_snr_r2_path <- function(curves, colour_by = c("measured", "method"), methods = NULL, n_points = 40,
                             ncol = 2, populations = NULL) {
  colour_by <- match.arg(colour_by)
  cv <- .with_outcomes(curves)
  if (is.null(methods)) methods <- if (colour_by == "measured") "random" else
    intersect(c(METHOD_STYLE$method, unique(cv$method)), unique(cv$method))
  cv <- cv[method %in% methods]
  if (colour_by == "measured") cv <- unique(cv, by = c("benchmark", "measured", "method", "draw", "n_items"))
  keys <- c("benchmark", "measured", "method", "n_items")
  s <- cv[, .(x = mean(g_rel, na.rm = TRUE), y = mean(r2_tau, na.rm = TRUE)), by = keys][is.finite(x) & is.finite(y)]
  s <- s[, .SD[n_items %in% .log_spaced(n_items, n_points)], by = c("benchmark", "measured", "method")]
  data.table::setorderv(s, keys)
  if (colour_by == "measured") {
    lev <- populations %||% unique(s$measured)
    s[, colour_key := factor(measured, levels = lev)]
    sc <- scale_population(lev, c("colour", "shape"))
  } else {
    s[, colour_key := factor(method, levels = methods)]
    sc <- scale_method(methods, c("colour", "shape"))
  }
  fac <- .facet_benchmarks(s, ncol, scales = "fixed")
  ggplot2::ggplot(s, ggplot2::aes(x, y, colour = colour_key, group = colour_key)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey55", linewidth = 0.35, linetype = "22") +
    ggplot2::geom_path(ggplot2::aes(alpha = n_items), linewidth = 0.45) +
    ggplot2::geom_point(ggplot2::aes(alpha = n_items, shape = colour_key), size = 1.1, stroke = 0.4) +
    sc +
    ggplot2::scale_alpha_continuous(trans = "log10", range = c(0.12, 1), name = "Number of items",
                                    breaks = c(10, 100, 1000, 10000), labels = c("10", "100", "1k", "10k")) +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.25), labels = c("0", ".25", ".5", ".75", "1")) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.25), labels = c("0", ".25", ".5", ".75", "1")) +
    ggplot2::coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
    fac + ggplot2::labs(x = "Empirical relative reliability G(n)",
                        y = expression("Empirical " * R[tau]^2 == sin^2 * (pi * tau / 2))) +
    theme_benchrel() + ggplot2::theme(legend.title.position = if (colour_by == "method") "top" else "left") +
    ggplot2::guides(colour = ggplot2::guide_legend(order = 1, nrow = if (length(levels(s$colour_key)) > 3) 2 else 1),
                    shape = ggplot2::guide_legend(order = 1),
                    alpha = ggplot2::guide_legend(order = 2, override.aes = list(shape = 16, linewidth = 0.45)))
}

.log_spaced <- function(n, k) {
  u <- sort(unique(n))
  if (length(u) <= k) return(u)
  target <- exp(seq(log(min(u)), log(max(u)), length.out = k))
  unique(u[vapply(target, function(t) which.min(abs(log(u) - log(t))), 1L)])
}

# ---------------------------------------------------------------------------
# 3  Reliability landscape
# ---------------------------------------------------------------------------

#' Classify benchmarks by an SNR criterion at n anticipated tasks
#'
#' With proportions y = s2_M / V and x = s2_R / V (V = total variance), the
#' relative criterion n s2_M / s2_R >= snr is y >= snr x / n and the absolute
#' criterion n s2_M / (s2_I + s2_R) >= snr is y >= snr / (n + snr). Meeting the
#' absolute criterion implies meeting the relative one.
#' @return `"both"`, `"relative"` or `"neither"`.
landscape_category <- function(prop_model, prop_residual, n_tasks, snr) {
  rel <- n_tasks * prop_model >= snr * prop_residual
  abs_ok <- prop_model >= snr / (n_tasks + snr)
  ifelse(abs_ok, "both", ifelse(rel, "relative", "neither"))
}

#' Reliability landscape of many benchmarks (type 3)
#'
#' One point per benchmark: x = share of variance that is residual, y = share
#' due to models, both on square-root axes. Colour and shape give whether the
#' benchmark meets the SNR criterion for absolute (and therefore relative)
#' reliability, only for relative reliability, or neither, when used for
#' `n_tasks` new tasks; the two boundary lines separate the classes. Panels:
#' populations (rows) x anticipated task counts (columns).
#'
#' @param gstudy [cached_gstudies()] rows with `prop_model`, `prop_residual`.
#' @param snr SNR threshold.
#' @param n_tasks Anticipated numbers of new tasks.
#' @param populations Populations (rows), in order.
plot_reliability_landscape <- function(gstudy, snr = 9, n_tasks = c(1, 10, 1000), populations = NULL) {
  g <- gstudy[eligible == TRUE & is.finite(prop_model) & is.finite(prop_residual)]
  populations <- populations %||% unique(g$population)
  g <- g[population %in% populations]
  d <- data.table::rbindlist(lapply(n_tasks, function(n) g[, .(benchmark, population, prop_model, prop_residual, n_tasks = n)]))
  d[, category := factor(landscape_category(prop_model, prop_residual, n_tasks, snr), levels = LANDSCAPE_STYLE$category)]
  lab_n <- function(n) paste(format(n, big.mark = ",", trim = TRUE), ifelse(n == 1, "task", "tasks"))
  pl <- stats::setNames(population_label(populations), populations)
  tl <- stats::setNames(lab_n(n_tasks), n_tasks)
  d[, `:=`(pop_lab = factor(pl[population], levels = pl), task_lab = factor(tl[as.character(n_tasks)], levels = tl))]
  # Boundaries; they meet the simplex edge x + y = 1 at x = n / (n + snr).
  bd <- data.table::rbindlist(lapply(n_tasks, function(n) {
    xe <- n / (n + snr)
    xs <- seq(0, 1, length.out = 400)^2 * xe
    data.table::rbindlist(list(
      data.table::data.table(n_tasks = n, x = xs, y = snr * xs / n, boundary = "relative"),
      data.table::data.table(n_tasks = n, x = c(0, xe), y = snr / (n + snr), boundary = "absolute")))
  }))
  bd <- bd[rep(seq_len(nrow(bd)), times = length(populations))][, population := rep(populations, each = nrow(bd))]
  bd[, `:=`(pop_lab = factor(pl[population], levels = pl), task_lab = factor(tl[as.character(n_tasks)], levels = tl))]
  brk <- c(0, 0.05, 0.25, 0.5, 1)
  ggplot2::ggplot(d, ggplot2::aes(prop_residual, prop_model)) +
    ggplot2::geom_line(data = bd, ggplot2::aes(x, y, linetype = boundary), colour = "grey35", linewidth = 0.4) +
    ggplot2::geom_point(ggplot2::aes(colour = category, shape = category), size = 1.3, stroke = 0.5, alpha = 0.9) +
    ggplot2::scale_colour_manual(values = stats::setNames(LANDSCAPE_STYLE$colour, LANDSCAPE_STYLE$category),
                                 labels = LANDSCAPE_STYLE$label, name = bquote("SNR" >= .(snr) ~ "met for"), drop = FALSE) +
    ggplot2::scale_shape_manual(values = stats::setNames(LANDSCAPE_STYLE$shape, LANDSCAPE_STYLE$category),
                                labels = LANDSCAPE_STYLE$label, name = bquote("SNR" >= .(snr) ~ "met for"), drop = FALSE) +
    ggplot2::scale_linetype_manual(values = c(absolute = "solid", relative = "22"),
                                   labels = c(absolute = "Absolute", relative = "Relative"), name = "Boundary") +
    ggplot2::scale_x_sqrt(limits = c(0, 1), breaks = brk, labels = c("0", ".05", ".25", ".5", "1")) +
    ggplot2::scale_y_sqrt(limits = c(0, 1), breaks = brk, labels = c("0", ".05", ".25", ".5", "1")) +
    ggplot2::coord_fixed() +
    ggplot2::facet_grid(pop_lab ~ task_lab) +
    ggplot2::labs(x = expression("Proportion of residual variance  " * sigma[R]^2 / V),
                  y = expression("Proportion of model variance  " * sigma[M]^2 / V)) +
    theme_benchrel() +
    ggplot2::guides(colour = ggplot2::guide_legend(order = 1), shape = ggplot2::guide_legend(order = 1),
                    linetype = ggplot2::guide_legend(order = 2))
}

# ---------------------------------------------------------------------------
# 4  Relative-absolute population dots
# ---------------------------------------------------------------------------

#' Full-benchmark relative and absolute reliability by population (type 4)
#'
#' One row per benchmark. Shape = population, colour and line type =
#' reliability type; a segment spans the populations' range for each type.
#' Values are the empirical coefficients at each benchmark's own length.
#'
#' @param gstudy [cached_gstudies()] rows.
#' @param populations Populations, in legend order.
plot_population_dots <- function(gstudy, populations = NULL, types = c("g_rel", "g_abs"),
                                 thresholds = SNR_THRESHOLDS) {
  g <- gstudy[eligible == TRUE & !is.na(var_model)]
  populations <- populations %||% unique(g$population)
  g <- g[population %in% populations]
  d <- data.table::melt(g, id.vars = c("benchmark", "population"), measure.vars = types,
                        variable.name = "type", value.name = "value")
  d[, type := factor(as.character(type), levels = types)]
  bl <- sort(unique(d$benchmark), decreasing = TRUE)
  off <- stats::setNames(c(0.16, -0.16)[seq_along(types)], types)
  d[, y := match(benchmark, bl) + off[as.character(type)]]
  rng <- d[, .(lo = min(value), hi = max(value)), by = .(benchmark, type, y)]
  ggplot2::ggplot(d, ggplot2::aes(value, y)) +
    snr_reference_lines(thresholds, "g", orientation = "v") +
    ggplot2::geom_segment(data = rng, ggplot2::aes(x = lo, xend = hi, y = y, yend = y, colour = type, linetype = type),
                          linewidth = 0.4) +
    ggplot2::geom_point(ggplot2::aes(colour = type, shape = population, fill = type), size = 1.7, stroke = 0.5) +
    scale_reliability_type(types, c("colour", "linetype", "fill")) +
    scale_population(populations, "shape") +
    ggplot2::scale_y_continuous(breaks = seq_along(bl), labels = benchmark_label(bl), expand = ggplot2::expansion(add = 0.6)) +
    ggplot2::scale_x_continuous(limits = c(min(0.5, floor(min(d$value, na.rm = TRUE) * 10) / 10), 1),
                                breaks = seq(0, 1, 0.1), labels = function(x) sub("^0\\.", ".", sprintf("%g", x))) +
    ggplot2::labs(x = "Reliability of the full benchmark", y = NULL) +
    theme_benchrel() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_line(colour = "grey94", linewidth = 0.25),
                   legend.position = "right", legend.box = "vertical") +
    ggplot2::guides(colour = ggplot2::guide_legend(order = 1), linetype = ggplot2::guide_legend(order = 1),
                    fill = ggplot2::guide_legend(order = 1), shape = ggplot2::guide_legend(order = 2))
}

# ---------------------------------------------------------------------------
# 5  Population distributions
# ---------------------------------------------------------------------------

#' Distribution of model scores, filled by population membership (type 5)
#'
#' @param scores [model_scores()].
#' @param benchmarks Benchmarks (panels).
#' @param pair `"reference->measured"` (default the paper's bottom 50% ->
#'   top 50%), or a single population. Models in neither population are grey.
#' @param bins Histogram bins per panel.
plot_population_histograms <- function(scores, benchmarks, pair = "bottom_half->top_half", bins = 25, ncol = 2) {
  pr <- parse_pair(pair)
  d <- data.table::rbindlist(lapply(benchmarks, function(b) {
    s <- scores[benchmark == b]
    r <- population_models(scores, b, pr$reference); m <- population_models(scores, b, pr$measured)
    s[, role := ifelse(model %in% r & model %in% m, "both", ifelse(model %in% r, "reference", ifelse(model %in% m, "measured", "neither")))]
    s
  }))
  same <- pr$reference == pr$measured
  roles <- c("reference", "measured", "both", "neither")
  cols <- c(reference = .pop_colour(pr$reference), measured = .pop_colour(pr$measured),
            both = .pop_colour(pr$measured), neither = "grey80")
  labs <- c(reference = paste(population_label(pr$reference), "(reference)"),
            measured = paste(population_label(pr$measured), "(measured)"),
            both = if (same) population_label(pr$measured) else "Both", neither = "Neither")
  present <- intersect(roles, unique(d$role))
  d[, role := factor(role, levels = roles)]
  fac <- .facet_benchmarks(d, ncol, scales = "free")
  ggplot2::ggplot(d, ggplot2::aes(score, fill = role)) +
    ggplot2::geom_histogram(bins = bins, colour = "white", linewidth = 0.15, position = "stack") +
    ggplot2::scale_fill_manual(values = cols, breaks = present, labels = labs[present], name = NULL) +
    ggplot2::scale_x_continuous(labels = function(x) sub("^0\\.", ".", format(x, trim = TRUE))) +
    fac + ggplot2::labs(x = "Model mean score", y = "Number of models") + theme_benchrel()
}

.pop_colour <- function(p) POPULATION_STYLE$colour[match(parse_population(p)$type, POPULATION_STYLE$type)]

# ---------------------------------------------------------------------------
# 6  Benchmark length vs items needed
# ---------------------------------------------------------------------------

#' Items in each benchmark vs items needed to meet an SNR threshold (type 6)
#'
#' Items needed is n* = ceil(snr B / s2_M) with B = s2_R (relative) or
#' s2_I + s2_R (absolute) (Eq. 5). Points above the dashed y = x line are
#' benchmarks too short for the criterion in that population. Benchmarks with
#' no model variance (n* infinite) are left out; the caller can count them.
#'
#' @param gstudy [cached_gstudies()] rows.
#' @param snr SNR threshold.
#' @param populations Panels.
plot_items_needed <- function(gstudy, snr = 9, types = c("g_rel", "g_abs"), populations = NULL, ncol = 2) {
  g <- gstudy[eligible == TRUE & !is.na(var_model)]
  populations <- populations %||% unique(g$population)
  g <- g[population %in% populations]
  target <- snr / (1 + snr)
  d <- data.table::rbindlist(lapply(types, function(t) {
    data.table::data.table(benchmark = g$benchmark, population = g$population, n_items = g$n_items, type = t,
                           n_needed = items_needed(g, target, if (t == "g_rel") "relative" else "absolute"))
  }))
  d <- d[is.finite(n_needed)]
  d[, type := factor(type, levels = types)]
  pl <- stats::setNames(population_label(populations), populations)
  d[, pop_lab := factor(pl[population], levels = pl)]
  lim <- range(c(d$n_items, d$n_needed))
  ggplot2::ggplot(d, ggplot2::aes(n_items, n_needed, colour = type, shape = type)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey55", linewidth = 0.35, linetype = "22") +
    ggplot2::geom_point(size = 1.3, stroke = 0.5, alpha = 0.85) +
    scale_reliability_type(types, c("colour", "shape")) +
    ggplot2::scale_x_log10(labels = .k_labels) + ggplot2::scale_y_log10(labels = .k_labels) +
    ggplot2::coord_fixed(xlim = lim, ylim = lim) +
    ggplot2::facet_wrap(~pop_lab, ncol = ncol) +
    ggplot2::labs(x = "Items in the benchmark", y = bquote("Items needed for SNR" >= .(snr))) +
    theme_benchrel()
}

.k_labels <- function(x) ifelse(x >= 1e6, paste0(x / 1e6, "M"), ifelse(x >= 1e3, paste0(x / 1e3, "k"), as.character(x)))

# ---------------------------------------------------------------------------
# Saving
# ---------------------------------------------------------------------------

#' Save a figure as PDF at paper size with embedded Times
#'
#' Uses `cairo_pdf` (Unicode Greek letters render in Times) when available.
#' @param p ggplot.
#' @param path Output `.pdf`.
#' @param width,height Inches; width defaults to [PAPER_WIDTH].
save_figure <- function(p, path, width = PAPER_WIDTH, height = 4) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  dev <- if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf
  # Unicode labels (tau, Phi, >=) need a UTF-8 character locale at draw time.
  if (!isTRUE(l10n_info()[["UTF-8"]])) {
    old <- Sys.getlocale("LC_CTYPE")
    on.exit(Sys.setlocale("LC_CTYPE", old), add = TRUE)
    suppressWarnings(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))
  }
  ggplot2::ggsave(path, p, width = width, height = height, device = dev, units = "in")
  invisible(path)
}
