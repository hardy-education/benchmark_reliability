# Benchmark-level drivers: glue between the data, populations, item metrics,
# and curves. The analysis/ scripts call these once per benchmark and write
# the returned tables to results/.

#' Item selection scores for one benchmark and reference population
#'
#' @param long Long response table.
#' @param scores [model_scores()] of `long`.
#' @param bench Benchmark name.
#' @param reference Population string for the models that score the items.
#' @param methods Selection methods (see [METHOD_SETS]).
#' @param missing Missing-data policy for [handle_missing()].
#' @return `data.table(benchmark, reference, item, method, value)` with
#'   attribute `timing`: one row per method with seconds, model and item counts.
benchmark_item_metrics <- function(long, scores, bench, reference = "all",
                                   methods = METHOD_SETS$with_2pl, missing = "auto") {
  t0 <- proc.time()[["elapsed"]]
  mods <- population_models(scores, bench, reference)
  X <- handle_missing(response_matrix(long, bench, models = mods), missing)
  load_s <- proc.time()[["elapsed"]] - t0
  im <- item_metrics(X, methods)
  im[, `:=`(benchmark = bench, reference = reference)]
  data.table::setcolorder(im, c("benchmark", "reference", "item", "method", "value"))
  tm <- attr(im, "timing")
  attr(im, "timing") <- data.table::data.table(
    benchmark = bench, reference = reference, method = c("load", names(tm)),
    seconds = c(load_s, unname(tm)), n_models = nrow(X), n_items = ncol(X),
    n_constant = length(constant_items(X)),
    irt_converged = if (is.null(attr(im, "irt_converged"))) NA else attr(im, "irt_converged"))
  im
}

#' Empirical reliability curves for every method on one benchmark
#'
#' @param long,scores,bench As in [benchmark_item_metrics()].
#' @param metrics Output of [benchmark_item_metrics()] for `bench` (any
#'   reference population).
#' @param measured Population string for the models whose reliability is
#'   evaluated.
#' @param methods Methods to trace; `"random"` is expanded to `n_random` seeded
#'   draws.
#' @param n_random Number of random orders.
#' @param steps Prefix lengths (`NULL` = every n).
#' @param seed Base seed; draw d uses `seed + d`.
#' @return `data.table(benchmark, reference, measured, method, draw, n_items,
#'   var_model, var_item, var_residual, g_rel, g_abs, snr_rel, kendall)` with
#'   attribute `timing`.
benchmark_curves <- function(long, scores, bench, metrics, measured = "all",
                             methods = METHOD_SETS$with_2pl, n_random = 10L,
                             steps = NULL, missing = "auto", kendall = TRUE, seed = 2026L) {
  mods <- population_models(scores, bench, measured)
  X <- handle_missing(response_matrix(long, bench, models = mods), missing)
  reference <- metrics$reference[1]
  jobs <- c(lapply(setdiff(methods, "random"), function(m) list(method = m, draw = 0L)),
            if ("random" %in% methods) lapply(seq_len(n_random), function(d) list(method = "random", draw = d)))
  timing <- list()
  res <- data.table::rbindlist(lapply(jobs, function(j) {
    sc <- if (j$method == "random") NULL else {
      m <- metrics[method == j$method]; stats::setNames(m$value, m$item)
    }
    t0 <- proc.time()[["elapsed"]]
    ord <- selection_order(sc, colnames(X), j$method, seed = seed + j$draw)
    cv <- reliability_curve(X, ord, steps = steps, kendall = kendall)
    timing[[length(timing) + 1]] <<- data.table::data.table(method = j$method, draw = j$draw,
                                                           seconds = proc.time()[["elapsed"]] - t0)
    cv[, `:=`(method = j$method, draw = j$draw)]
  }))
  res[, `:=`(benchmark = bench, reference = reference, measured = measured)]
  data.table::setcolorder(res, c("benchmark", "reference", "measured", "method", "draw"))
  attr(res, "timing") <- data.table::rbindlist(timing)[, `:=`(benchmark = bench, reference = reference,
                                                              measured = measured, n_models = nrow(X), n_items = ncol(X))][]
  res
}

#' G-study and D-study projections by population (paper Fig. 2)
#'
#' @param populations Population strings to compare.
#' @param n_grid Test lengths for the D-study projection.
#' @return list(gstudy = one row per benchmark x population (see
#'   [gstudy_row()]; ineligible populations have NA components), dstudy =
#'   D-study curves for the eligible rows).
population_gstudies <- function(long, scores, benchmarks, populations = c("all", "top_half", "top_n:10"),
                                n_grid = unique(round(10^seq(0, 4, by = 0.05))), missing = "auto") {
  g <- data.table::rbindlist(lapply(benchmarks, function(b) data.table::rbindlist(lapply(populations, function(p) {
    gstudy_row(long, scores, b, p, missing)
  }), fill = TRUE)), fill = TRUE)
  ok <- g[eligible == TRUE & !is.na(var_model)]
  d <- dstudy_curve(ok[, .(benchmark, population, var_model, var_item, var_residual, n_models)], n_grid)
  list(gstudy = g, dstudy = d)
}

#' Write a table, creating its directory; `.csv.gz` paths are compressed
write_result <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(x, path)
  invisible(path)
}
