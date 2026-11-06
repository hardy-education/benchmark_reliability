# Cached, modular access to every computed stage.
#
# Each cached_*() function returns the same table the analysis/ scripts write,
# reading it from the results cache when present and computing (and saving)
# only what is missing. The cache layout is the scripts' layout, so the two
# can be mixed freely:
#
#   <dir>/item_metrics/<reference>/<benchmark>__<tag>.csv.gz   item scores
#   <dir>/curves/<reference>__<measured>[__<run_tag>]/<benchmark>.csv.gz
#   <dir>/gstudy/<name>.csv                                    G-studies
#
# Typical use:
#
#   long   <- load_responses(benchmarks = "main")
#   scores <- model_scores(long)
#   im <- cached_item_metrics(long, scores, "main", reference = "bottom_half")
#   cv <- cached_curves(long, scores, "main", "bottom_half", "top_half")
#   check_curves(cv)
#   plot_selection_curves(cv, outcome = "g_rel")
#
# Your own criterion: score items however you like, stack the scores as
# `data.table(benchmark, item, method, value)` (larger = select earlier) or
# turn explicit orders into that shape with order_to_metrics(), and pass them
# as `metrics` with a `run_tag` so they are cached separately.

#' Root directory of the results cache (env `BENCHREL_RESULTS`, default `results`)
results_dir <- function() Sys.getenv("BENCHREL_RESULTS", unset = "results")

# Benchmarks named by a set or given explicitly, restricted to those in `long`.
.benches <- function(benchmarks, long) {
  avail <- unique(long$benchmark)
  b <- if (is.null(benchmarks)) avail else if (length(benchmarks) == 1 && benchmarks %in% names(BENCHMARK_SETS))
    BENCHMARK_SETS[[benchmarks]] else benchmarks
  miss <- setdiff(b, avail)
  if (length(miss)) stop("Benchmark(s) not in the loaded data: ", paste(miss, collapse = ", "), call. = FALSE)
  b
}

#' Cached item-selection scores
#'
#' @param long,scores Response table and [model_scores()].
#' @param benchmarks Benchmark names or a [BENCHMARK_SETS] name.
#' @param reference Reference population string.
#' @param methods Methods to make sure are present (see [SELECTION_METHODS]).
#'   `"random"` needs no scores and is ignored here.
#' @param dir Cache root.
#' @param force Recompute even if cached.
#' @param workers Parallel workers over benchmarks (forked; 1 = sequential).
#' @param missing Missing-data policy for [handle_missing()].
#' @return `data.table(benchmark, reference, item, method, value)`.
cached_item_metrics <- function(long, scores, benchmarks = NULL, reference = "all",
                                methods = METHOD_SETS$with_2pl, dir = results_dir(), force = FALSE,
                                workers = 1L, missing = "auto") {
  methods <- setdiff(methods, "random")
  bs <- .benches(benchmarks, long)
  if (!length(methods)) return(.empty_metrics())
  one <- function(b) {
    d <- file.path(dir, "item_metrics", population_tag(reference))
    files <- list.files(d, pattern = paste0("^", .regex_escape(b), "(__.*)?\\.csv\\.gz$"), full.names = TRUE)
    have <- if (length(files) && !force) unique(data.table::rbindlist(lapply(files, data.table::fread,
                                              colClasses = list(character = "item"))), by = c("item", "method")) else NULL
    todo <- setdiff(methods, have$method)
    if (!population_eligible(scores, b, reference)) {
      if (length(todo)) message("item metrics: ", b, " / ", reference, " is not an eligible population; skipped")
      return(have %||% data.table::data.table())
    }
    for (grp in list(fast = setdiff(todo, "irt_a"), irt = intersect(todo, "irt_a"))) {
      if (!length(grp)) next
      tag <- if (identical(grp, "irt_a")) "irt" else "fast"
      im <- benchmark_item_metrics(long, scores, b, reference, grp, missing)
      path <- file.path(d, paste0(b, "__", tag, ".csv.gz"))
      old <- if (file.exists(path) && !force) data.table::fread(path, colClasses = list(character = "item")) else NULL
      new <- unique(data.table::rbindlist(list(im, old), use.names = TRUE), by = c("item", "method"))
      write_result(new, path)
      write_result(attr(im, "timing"), file.path(dir, "timing", "item_metrics", population_tag(reference),
                                                 paste0(b, "__", tag, ".csv")))
      have <- unique(data.table::rbindlist(list(new, have), use.names = TRUE), by = c("item", "method"))
    }
    have %||% data.table::data.table()
  }
  if (any(methods == "irt_a") && requireNamespace("mirt", quietly = TRUE)) loadNamespace("mirt")
  out <- par_map(bs, one, workers)
  .stop_on_errors(out, bs, "item metrics")
  res <- data.table::rbindlist(out, use.names = TRUE)
  if (!nrow(res)) .empty_metrics() else res
}

.empty_metrics <- function() data.table::data.table(benchmark = character(), reference = character(), item = character(),
                                                    method = character(), value = numeric())

#' Cached empirical reliability curves for one reference -> measured pair
#'
#' Reads `<dir>/curves/<ref>__<meas>[__<run_tag>]/<benchmark>.csv.gz`. Methods
#' missing from a cached file are computed and appended; benchmarks on which
#' the pair is not eligible ([population_eligible()]) are skipped.
#'
#' @param reference,measured Population strings (or `reference = "ref->meas"`).
#' @param methods Orderings to make sure are present; `"random"` gives
#'   `n_random` seeded draws.
#' @param metrics Optional item scores `data.table(benchmark, item, method,
#'   value)` to use instead of the cached reference-population scores. Give a
#'   `run_tag` so these curves are cached apart from the standard ones.
#' @param n_random,steps,missing,seed As in [benchmark_curves()].
#' @param run_tag Optional suffix for the cache directory.
#' @return The row-bound [benchmark_curves()] tables.
cached_curves <- function(long, scores, benchmarks = NULL, reference = "all", measured = NULL,
                          methods = METHOD_SETS$with_2pl, metrics = NULL, n_random = 10L, steps = NULL,
                          missing = "auto", seed = 2026L, run_tag = NULL, dir = results_dir(),
                          force = FALSE, workers = 1L) {
  if (is.null(measured)) { pr <- parse_pair(reference); reference <- pr$reference; measured <- pr$measured }
  bs <- .benches(benchmarks, long)
  ok <- population_eligible(scores, bs, paste0(reference, "->", measured))
  if (any(!ok)) message("curves: ", reference, " -> ", measured, " not eligible on ",
                        paste(bs[!ok], collapse = ", "), "; skipped")
  bs <- bs[ok]
  if (!length(bs)) return(data.table::data.table())
  if (is.null(metrics)) {
    metrics <- cached_item_metrics(long, scores, bs, reference, setdiff(methods, "random"), dir = dir, workers = workers,
                                   missing = missing)
  }
  tag <- paste0(pair_tag(reference, measured), if (!is.null(run_tag)) paste0("__", run_tag))
  one <- function(b) {
    path <- file.path(dir, "curves", tag, paste0(b, ".csv.gz"))
    old <- if (file.exists(path) && !force) data.table::fread(path) else NULL
    todo <- setdiff(methods, old$method)
    if (!length(todo)) return(old[method %in% methods])
    im <- metrics[benchmark == b]
    data.table::set(im, j = "reference", value = rep(reference, nrow(im)))
    avail <- intersect(todo, c(unique(im$method), "random"))
    if (length(setdiff(todo, avail))) message("curves: ", b, " / ", reference, " lacks ",
                                              paste(setdiff(todo, avail), collapse = ", "), "; skipped")
    if (!length(avail)) return(if (is.null(old)) data.table::data.table() else old[method %in% methods])
    cv <- benchmark_curves(long, scores, b, im, measured, avail, n_random, steps,
                           missing, seed = seed)
    all <- data.table::rbindlist(list(old, cv), use.names = TRUE, fill = TRUE)
    write_result(all, path)
    all[method %in% methods]
  }
  out <- par_map(bs, one, workers)
  .stop_on_errors(out, bs, "curves")
  data.table::rbindlist(out, use.names = TRUE, fill = TRUE)
}

#' Cached full-benchmark G-studies by population
#'
#' One row per benchmark x population, with the variance components, their
#' proportions of the total, the empirical coefficients at each benchmark's
#' own length, and `eligible` ([population_eligible()]; ineligible rows have
#' NA components). Cached at `<dir>/gstudy/<name>.csv`; populations missing
#' from the cached table are computed and appended.
#'
#' @param name Cache file name (e.g. `"appendix"`, `"complete"`).
#' @param populations Population strings.
#' @param missing Missing-data policy. `"none"` keeps incomplete matrices,
#'   which are then fitted to the observed cells by `lmer` (the right choice
#'   for the Complete set).
cached_gstudies <- function(long, scores, benchmarks = NULL, populations = "all", name = "gstudies",
                            missing = "auto", dir = results_dir(), force = FALSE, workers = 1L) {
  bs <- .benches(benchmarks, long)
  path <- file.path(dir, "gstudy", paste0(name, ".csv"))
  old <- if (file.exists(path) && !force) data.table::fread(path) else NULL
  todo <- data.table::CJ(benchmark = bs, population = populations, sorted = FALSE)
  if (!is.null(old)) todo <- todo[!old, on = c("benchmark", "population")]
  if (nrow(todo)) {
    if (requireNamespace("lme4", quietly = TRUE)) loadNamespace("lme4")
    rows <- par_map(seq_len(nrow(todo)), function(i) {
      gstudy_row(long, scores, todo$benchmark[i], todo$population[i], missing)
    }, workers)
    .stop_on_errors(rows, paste(todo$benchmark, todo$population), "G-studies")
    old <- data.table::rbindlist(c(list(old), rows), use.names = TRUE, fill = TRUE)
    write_result(old, path)
  }
  old[benchmark %in% bs & population %in% populations]
}

#' Full-benchmark G-study of one benchmark in one population
#'
#' @return One-row `data.table` (see [cached_gstudies()]).
gstudy_row <- function(long, scores, bench, population, missing = "auto") {
  elig <- population_eligible(scores, bench, population)
  base <- data.table::data.table(benchmark = bench, population = population, eligible = elig,
                                 n_benchmark_models = nrow(scores[benchmark == bench]))
  if (!elig) return(base)
  mods <- population_models(scores, bench, population)
  X <- handle_missing(response_matrix(long, bench, models = mods), missing)
  X <- X[rowSums(!is.na(X)) > 0, colSums(!is.na(X)) > 0, drop = FALSE]
  vc <- tryCatch(variance_components(X), error = function(e) {
    data.table::data.table(var_model = NA_real_, var_item = NA_real_, var_residual = NA_real_,
                           n_models = nrow(X), n_items = ncol(X), engine = "failed", status = conditionMessage(e))
  })
  out <- cbind(base, reliability_coefficients(vc))
  tot <- out$var_model + out$var_item + out$var_residual
  out[, `:=`(n_cells = sum(!is.na(X)), coverage = mean(!is.na(X)), mean_score = mean(X, na.rm = TRUE),
             prop_model = var_model / tot, prop_item = var_item / tot, prop_residual = var_residual / tot)]
  out[]
}

#' Turn explicit item orders into the item-score shape the curve functions use
#'
#' @param orders Named list: one character vector of items (first = selected
#'   first) per benchmark, or a list of such lists named by method.
#' @param method Method name to record (when `orders` is per benchmark only).
#' @return `data.table(benchmark, item, method, value)` with value = -rank.
#' @examples
#' order_to_metrics(list(gpqa = c("q3", "q1", "q2")), method = "my_order")
order_to_metrics <- function(orders, method = "custom") {
  is_nested <- all(vapply(orders, is.list, NA))
  if (is_nested) return(data.table::rbindlist(Map(order_to_metrics, orders, names(orders))))
  data.table::rbindlist(lapply(names(orders), function(b) {
    it <- as.character(orders[[b]])
    data.table::data.table(benchmark = b, item = it, method = method, value = -seq_along(it))
  }))
}

#' Check a set of reliability curves
#'
#' A per benchmark x pair x method summary to read before plotting: how many
#' prefixes there are, how many have no regular REML fit (NA; zero residual
#' or constant data), how many put s2_M on the zero boundary, the range of G,
#' and the share of points outside [0, 1] (should be zero). With `verify > 0`
#' and the data supplied, `verify` prefixes per curve are refitted by `lmer`
#' and the largest absolute difference in G is reported.
#'
#' @param curves Output of [cached_curves()] / [benchmark_curves()].
#' @param long,scores Optional data for the `lmer` check.
#' @param verify Number of prefixes per curve to refit with `lmer` (0 = none).
#' @param metrics Item scores used to build the orders (needed for `verify`).
#' @return `data.table` summary.
check_curves <- function(curves, long = NULL, scores = NULL, verify = 0L, metrics = NULL) {
  keys <- intersect(c("benchmark", "reference", "measured", "method"), names(curves))
  out <- curves[, .(n_points = .N, n_draws = data.table::uniqueN(draw),
                    pct_no_fit = round(100 * mean(is.na(g_rel)), 2),
                    pct_sigma_m_zero = round(100 * mean(status %in% c("boundary_model", "boundary_both")), 2),
                    g_rel_min = suppressWarnings(min(g_rel, na.rm = TRUE)),
                    g_rel_max = suppressWarnings(max(g_rel, na.rm = TRUE)),
                    pct_outside_01 = round(100 * mean(g_rel < 0 | g_rel > 1 | g_abs < 0 | g_abs > 1, na.rm = TRUE), 3),
                    kendall_final = kendall[which.max(n_items)]), by = keys]
  if (verify > 0 && !is.null(long) && !is.null(scores)) {
    out[, max_abs_diff_lmer := NA_real_]
    for (r in seq_len(nrow(out))) {
      k <- out[r]
      if (k$method != "random" && (is.null(metrics) || !nrow(metrics[benchmark == k$benchmark & method == k$method]))) next
      X <- handle_missing(response_matrix(long, k$benchmark, population_models(scores, k$benchmark, k$measured)), "auto")
      ord <- if (k$method == "random") selection_order(NULL, colnames(X), "random", seed = 2026L + 1L) else {
        m <- metrics[benchmark == k$benchmark & method == k$method]; selection_order(stats::setNames(m$value, m$item), colnames(X), k$method, seed = 2026L)
      }
      cv <- curves[benchmark == k$benchmark & measured == k$measured & method == k$method & draw == min(draw)]
      ns <- unique(round(exp(seq(log(2), log(max(cv$n_items)), length.out = verify))))
      d <- vapply(ns, function(n) {
        vc <- suppressMessages(variance_components(X[, ord[seq_len(n)], drop = FALSE], engine = "lmer"))
        g <- reliability_coefficients(vc)$g_rel
        abs(g - cv[n_items == n]$g_rel[1])
      }, 0)
      out[r, max_abs_diff_lmer := suppressWarnings(max(d, na.rm = TRUE))]
    }
  }
  out[]
}

.regex_escape <- function(x) gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", x)

.stop_on_errors <- function(res, labels, what) {
  # A worker that died returns NULL from mclapply; every job here returns a table.
  bad <- which(vapply(res, function(r) inherits(r, "try-error") || is.null(r), NA))
  if (length(bad)) stop(what, " failed for: ", paste(labels[bad], collapse = ", "), "\n",
                        as.character(res[[bad[1]]]), call. = FALSE)
}
