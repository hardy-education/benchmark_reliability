# Stage 5: turn the per-job timings recorded by stages 1-2 into run-time
# estimates for the scenarios in docs/timing.md.
#
#   Rscript analysis/05_timing_report.R --workers=8
#
# Every number comes from a measured run on this machine. Sequential totals
# are sums of single-worker job times; parallel estimates schedule those same
# jobs (one benchmark per worker) longest-first onto `workers` cores, which is
# how stages 1-2 run with --workers. Fixed per-process overhead (starting R,
# reading aggdf.csv) is measured once and added to every scenario.

source("analysis/_common.R")
flags <- parse_flags(defaults = list(workers = 8, n_random = 10))

overhead <- attr(timed(load_responses()), "seconds")

# Longest-processing-time-first makespan on w identical workers.
makespan <- function(x, w) {
  load <- numeric(w)
  for (t in sort(x, decreasing = TRUE)) { k <- which.min(load); load[k] <- load[k] + t }
  max(load)
}

# ---- stage 1: item selection values -------------------------------------------
im <- rbindlist(lapply(list.files(results_path("timing", "item_metrics"), "\\.csv$", recursive = TRUE, full.names = TRUE), fread), fill = TRUE)
im <- im[, .(seconds = max(seconds), n_models = max(n_models), n_items = max(n_items),
             irt_converged = irt_converged[1]), by = .(benchmark, reference, method)]
write_result(dcast(im, benchmark + reference + n_models + n_items ~ method, value.var = "seconds"),
             results_path("timing", "item_metrics_by_benchmark.csv"))

sets <- list(all18 = sort(unique(im$benchmark)), core4 = BENCHMARK_SETS$core4)
scen1 <- CJ(reference = c("all", "top_frac:0.1"), benchmarks = names(sets), methods = names(METHOD_SETS), sorted = FALSE)
scen1 <- scen1[, {
  ms <- c("load", setdiff(METHOD_SETS[[methods]], "random"))
  per_bench <- im[reference == .BY$reference & benchmark %in% sets[[.BY$benchmarks]] & method %in% ms,
                  .(s = sum(seconds)), by = benchmark]$s
  .(sequential_s = overhead + sum(per_bench), parallel_s = overhead + makespan(per_bench, flags$workers),
    slowest_benchmark_s = max(per_bench))
}, by = .(reference, benchmarks, methods)]

# ---- stage 2: empirical variance decomposition curves ----------------------
# Keys are taken from the file path (curves/<reference>__<measured>/<benchmark>.csv).
cv <- rbindlist(lapply(list.files(results_path("timing", "curves"), "\\.csv$", recursive = TRUE, full.names = TRUE), function(f) {
  d <- fread(f)[, .(method, draw, seconds)]
  pops <- strsplit(basename(dirname(f)), "__", fixed = TRUE)[[1]]
  d[, `:=`(benchmark = sub("\\.csv$", "", basename(f)),
           reference = sub("-", ":", pops[1], fixed = TRUE), measured = sub("-", ":", pops[2], fixed = TRUE))]
}))
# Ordering cost does not depend on which criterion produced the order, so
# per-ordering time is the mean over all orderings in the job.
per_order <- cv[method != "job_total", .(order_s = mean(seconds)), by = .(benchmark, reference, measured)]
job <- cv[method == "job_total", .(job_s = seconds, n_orders = NA_integer_), by = .(benchmark, reference, measured)]
cvj <- merge(per_order, job, by = c("benchmark", "reference", "measured"))
cvj[, fixed_s := pmax(job_s - order_s * (4 + flags$n_random), 0)]   # matrix build + write, measured with 4 + n_random orders
cvj <- cvj[, .(order_s = mean(order_s), fixed_s = mean(fixed_s)), by = .(benchmark, measured)]

scen2 <- CJ(measured = c("all", "top_frac:0.1"), benchmarks = names(sets), methods = names(METHOD_SETS), sorted = FALSE)
scen2 <- scen2[, {
  n_orders <- length(setdiff(METHOD_SETS[[methods]], "random")) + flags$n_random
  per_bench <- cvj[measured == .BY$measured & benchmark %in% sets[[.BY$benchmarks]], fixed_s + n_orders * order_s]
  .(n_orderings = n_orders, sequential_s = overhead + sum(per_bench), parallel_s = overhead + makespan(per_bench, flags$workers))
}, by = .(measured, benchmarks, methods)]

write_result(scen1, results_path("timing", "scenarios_item_selection.csv"))
write_result(scen2, results_path("timing", "scenarios_variance_curves.csv"))
log_msg("per-process overhead (R start excluded): %.1fs", overhead)
print(scen1); print(scen2)
print(im[method == "irt_a", .(benchmark, reference, n_models, n_items, seconds, irt_converged)][order(reference, -seconds)])
