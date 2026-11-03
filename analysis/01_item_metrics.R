# Stage 1: score every item of every benchmark under each selection method,
# using the full item set of a reference population (no bootstrap).
#
#   Rscript analysis/01_item_metrics.R                       # all 18, reference = all, with 2PL
#   Rscript analysis/01_item_metrics.R --reference=top_frac:0.1 --benchmarks=core4 --methods=no_2pl
#   Rscript analysis/01_item_metrics.R --reference=all,top_frac:0.1 --workers=4
#
# Output (one file per benchmark x reference, skipped if present unless --force):
#   results/item_metrics/<reference>/<benchmark>.csv.gz   item, method, value
#   results/timing/item_metrics/<reference>/<benchmark>.csv  seconds per method
#
# Methods are computed in separate files per run, so adding 2PL later does not
# recompute the fast methods: pass --methods=irt_a --tag=irt to write to
# <benchmark>__irt.csv.gz; stage 2 reads every file for the benchmark.

source("analysis/_common.R")

flags <- parse_flags(defaults = list(benchmarks = "all18", reference = "all", methods = "with_2pl",
                                     missing = "auto", workers = 1, force = FALSE, tag = ""))
methods <- resolve_methods(flags$methods)
long <- load_responses(benchmarks = flags$benchmarks)
scores <- model_scores(long)
benches <- sort(unique(long$benchmark))

jobs <- CJ(benchmark = benches, reference = flags$reference, sorted = FALSE)
suffix <- if (nzchar(flags$tag)) paste0("__", flags$tag) else ""

run_job <- function(i) {
  b <- jobs$benchmark[i]; ref <- jobs$reference[i]
  out <- results_path("item_metrics", population_tag(ref), paste0(b, suffix, ".csv.gz"))
  tim <- results_path("timing", "item_metrics", population_tag(ref), paste0(b, suffix, ".csv"))
  if (file.exists(out) && !isTRUE(flags$force)) return(invisible(NULL))
  log_msg("item metrics: %s | reference %s | %s", b, ref, paste(methods, collapse = ","))
  im <- benchmark_item_metrics(long, scores, b, ref, methods, flags$missing)
  write_result(im, out)
  write_result(attr(im, "timing"), tim)
  log_msg("done %s | %s: %.1fs", b, ref, sum(attr(im, "timing")$seconds))
}

invisible(par_map(seq_len(nrow(jobs)), run_job, workers = flags$workers))
