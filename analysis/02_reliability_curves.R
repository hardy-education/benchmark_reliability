# Stage 2: empirical variance decomposition as each item is added, for every
# selection method, in a measured population.
#
#   Rscript analysis/02_reliability_curves.R --reference=all --measured=all
#   Rscript analysis/02_reliability_curves.R --reference=top_frac:0.1 --measured=top_frac:0.1,all --benchmarks=core4
#
# Needs stage 1 output for the reference population. Every prefix length
# n = 2..N is computed (pass --steps=log to use a 100-point log grid instead).
#
# Output:
#   results/curves/<reference>__<measured>/<benchmark>.csv.gz
#   results/timing/curves/<reference>__<measured>/<benchmark>.csv

source("analysis/_common.R")

flags <- parse_flags(defaults = list(benchmarks = "all18", reference = "all", measured = "all",
                                     methods = "with_2pl", n_random = 10, steps = "all",
                                     missing = "auto", workers = 1, force = FALSE))
methods <- resolve_methods(flags$methods)
long <- load_responses(benchmarks = flags$benchmarks)
scores <- model_scores(long)
benches <- sort(unique(long$benchmark))

read_metrics <- function(b, ref) {
  files <- list.files(results_path("item_metrics", population_tag(ref)),
                      pattern = paste0("^", b, "(__.*)?\\.csv\\.gz$"), full.names = TRUE)
  if (!length(files)) stop("No stage-1 output for ", b, " / ", ref, ". Run analysis/01_item_metrics.R first.")
  unique(rbindlist(lapply(files, fread)), by = c("item", "method"))
}

jobs <- CJ(benchmark = benches, reference = flags$reference, measured = flags$measured, sorted = FALSE)

run_job <- function(i) {
  b <- jobs$benchmark[i]; ref <- jobs$reference[i]; meas <- jobs$measured[i]
  tag <- paste0(population_tag(ref), "__", population_tag(meas))
  out <- results_path("curves", tag, paste0(b, ".csv.gz"))
  if (file.exists(out) && !isTRUE(flags$force)) return(invisible(NULL))
  im <- read_metrics(b, ref)
  avail <- intersect(methods, c(unique(im$method), "random"))
  if (length(setdiff(methods, avail))) log_msg("note: %s / %s lacks %s; skipping those", b, ref, paste(setdiff(methods, avail), collapse = ","))
  steps <- if (identical(flags$steps, "log")) unique(round(10^seq(log10(2), log10(nrow(unique(im, by = "item"))), length.out = 100))) else NULL
  log_msg("curves: %s | reference %s | measured %s", b, ref, meas)
  t0 <- proc.time()[["elapsed"]]
  cv <- benchmark_curves(long, scores, b, im, meas, avail, flags$n_random, steps, flags$missing)
  write_result(cv, out)
  elapsed <- proc.time()[["elapsed"]] - t0
  tm <- attr(cv, "timing")
  tm <- rbind(tm, tm[1][, `:=`(method = "job_total", draw = NA_integer_, seconds = elapsed)])
  write_result(tm, results_path("timing", "curves", tag, paste0(b, ".csv")))
  log_msg("done %s | %s: %.1fs", b, tag, elapsed)
}

invisible(par_map(seq_len(nrow(jobs)), run_job, workers = flags$workers))
