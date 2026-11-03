# Shared setup for the analysis drivers. Run every script from the repository
# root, e.g. `Rscript analysis/01_item_metrics.R --benchmarks=core4`.

suppressPackageStartupMessages({
  if (requireNamespace("benchrel", quietly = TRUE) && !file.exists("DESCRIPTION")) {
    library(benchrel)
  } else {
    pkgload::load_all(".", quiet = TRUE, export_all = FALSE)
  }
  library(data.table)
})

RESULTS <- Sys.getenv("BENCHREL_RESULTS", unset = "results")
results_path <- function(...) file.path(RESULTS, ...)

# Resolve a --methods flag: a set name from METHOD_SETS or explicit names.
resolve_methods <- function(x) {
  if (length(x) == 1 && x %in% names(METHOD_SETS)) return(METHOD_SETS[[x]])
  bad <- setdiff(x, SELECTION_METHODS$method)
  if (length(bad)) stop("Unknown method(s): ", paste(bad, collapse = ", "))
  x
}

log_msg <- function(...) message(format(Sys.time(), "[%H:%M:%S] "), sprintf(...))
