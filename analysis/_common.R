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

# Run jobs in parallel and stop (non-zero exit) if any failed. A job counts as
# done only if it returns; an R error or a worker that died (mclapply then
# delivers NULL) counts as a failure. Packages with compiled code are loaded in
# the parent first, because loading a DLL inside a forked worker can segfault
# on macOS, which kills every job.
run_jobs <- function(n, f, workers, preload = character()) {
  for (p in preload) if (requireNamespace(p, quietly = TRUE)) loadNamespace(p)
  res <- par_map(seq_len(n), function(i) { f(i); TRUE }, workers = workers)
  bad <- which(!vapply(res, isTRUE, NA))
  for (i in bad) message("job ", i, " failed: ",
                         if (inherits(res[[i]], "try-error")) as.character(res[[i]]) else "worker died (no result)")
  if (length(bad)) stop(length(bad), " of ", n, " jobs failed.", call. = FALSE)
  invisible(res)
}
