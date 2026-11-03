# Small helpers shared across modules. Nothing here touches data on load.

`%||%` <- function(x, y) if (is.null(x)) y else x

# data.table's non-standard evaluation trips R CMD check; declare the columns.
utils::globalVariables(c(
  "benchmark", "item", "model", "resp", "score", "n_items", "population",
  "metric", "value", "rank", "method", "draw", "g_rel", "g_abs", "kendall",
  "var_model", "var_item", "var_residual", "engine", "lo", "hi", ".N", ".SD",
  "n_eval", "expected_tau", "snr_rel", "snr_abs", "seconds", "label", "reference",
  "measured", "g_rel_lo", "g_rel_hi", "n_models", "lt", "type", "pop", "mean_score"
))

#' Directory that holds the input data
#'
#' Defaults to `data/` under the working directory; set the environment
#' variable `BENCHREL_DATA` to point elsewhere (e.g. a shared drive).
data_dir <- function() Sys.getenv("BENCHREL_DATA", unset = "data")

#' Path to a file inside [data_dir()]
data_path <- function(...) file.path(data_dir(), ...)

#' Is `x` a numeric 0/1 matrix (NA allowed)?
is_binary_matrix <- function(x) {
  v <- x[!is.na(x)]
  all(v == 0 | v == 1)
}

# Validate the response-matrix contract used throughout: models in rows,
# items in columns, numeric, with column names.
.check_matrix <- function(X, need_complete = FALSE) {
  if (!is.matrix(X) || !is.numeric(X)) stop("`X` must be a numeric matrix (models x items).", call. = FALSE)
  if (is.null(colnames(X))) stop("`X` must have item names as column names.", call. = FALSE)
  if (need_complete && anyNA(X)) stop("`X` must not contain missing values here; see `handle_missing()`.", call. = FALSE)
  invisible(X)
}

#' Parse `--key=value` command-line flags into a named list
#'
#' Used by the `analysis/` drivers so every stage can be re-run on a subset
#' without editing code: `Rscript analysis/01_item_metrics.R --benchmarks=gpqa`.
#' Flags without a value become `TRUE`. Comma-separated values are split.
parse_flags <- function(args = commandArgs(trailingOnly = TRUE), defaults = list()) {
  out <- defaults
  for (a in args) {
    if (!startsWith(a, "--")) next
    kv <- strsplit(sub("^--", "", a), "=", fixed = TRUE)[[1]]
    key <- gsub("-", "_", kv[1])
    val <- if (length(kv) < 2) TRUE else paste(kv[-1], collapse = "=")
    if (is.character(val) && grepl(",", val, fixed = TRUE)) val <- strsplit(val, ",", fixed = TRUE)[[1]]
    if (is.character(val) && length(val) == 1 && grepl("^-?[0-9.]+$", val)) val <- as.numeric(val)
    if (is.character(val) && length(val) == 1 && val %in% c("TRUE", "FALSE")) val <- as.logical(val)
    out[[key]] <- val
  }
  out
}

#' Time an expression, returning its value with the elapsed seconds attached
#'
#' `attr(result, "seconds")` holds wall-clock time. Used to record per-metric
#' and per-curve costs so the timing tables come from real runs.
timed <- function(expr) {
  t0 <- proc.time()[["elapsed"]]
  val <- force(expr)
  attr(val, "seconds") <- proc.time()[["elapsed"]] - t0
  val
}

#' Run `f` over `x` in parallel with forked workers where available
#'
#' Falls back to sequential `lapply()` on Windows or when `workers <= 1`.
#' Errors in one element are returned as `try-error` objects rather than
#' aborting the whole map, so a single failed benchmark does not lose the rest.
par_map <- function(x, f, workers = 1L) {
  if (workers <= 1L || .Platform$OS.type == "windows") return(lapply(x, function(e) try(f(e), silent = TRUE)))
  parallel::mclapply(x, function(e) try(f(e), silent = TRUE), mc.cores = workers, mc.preschedule = FALSE)
}
