# Diagnostics for one reference -> measured run, to help read its curves.
#
#   Rscript analysis/07_run_diagnostics.R --reference=bottom_half --measured=top_half
#
# Per benchmark: population sizes; share of items constant in each population
# (constant-in-reference items get no score and are ranked last); 2PL
# convergence; and per method, G at 25 items and at 10% of the benchmark, the
# mean accuracy of the measured models on the first 25 selected items (a
# ceiling/floor check), and the share of prefixes with s2_M on the boundary or
# no regular REML fit.
#
# Writes results/diagnostics/<reference>__<measured>.csv

source("analysis/_common.R")
flags <- parse_flags(defaults = list(benchmarks = "all18", reference = "bottom_half", measured = "top_half", k = 25L))
long <- load_responses(benchmarks = flags$benchmarks)
scores <- model_scores(long)
tag <- paste0(population_tag(flags$reference), "__", population_tag(flags$measured))

res <- rbindlist(lapply(sort(unique(long$benchmark)), function(b) {
  Xr <- handle_missing(response_matrix(long, b, population_models(scores, b, flags$reference)), "auto")
  Xm <- handle_missing(response_matrix(long, b, population_models(scores, b, flags$measured)), "auto")
  cv <- fread(results_path("curves", tag, paste0(b, ".csv.gz")))
  im <- rbindlist(lapply(list.files(results_path("item_metrics", population_tag(flags$reference)),
                                    pattern = paste0("^", b, "__.*csv\\.gz$"), full.names = TRUE), fread))
  tm <- results_path("timing", "item_metrics", population_tag(flags$reference), paste0(b, "__irt.csv"))
  conv <- if (file.exists(tm)) { t <- fread(tm); if ("irt_converged" %in% names(t)) t$irt_converged[1] else NA } else NA
  N <- ncol(Xm); k <- min(flags$k, N); n10 <- max(2L, round(0.1 * N))
  per <- cv[, .(g_at_k = mean(g_rel[n_items == k], na.rm = TRUE),
                g_at_10pct = mean(g_rel[n_items == n10], na.rm = TRUE),
                pct_sigmaM_zero = 100 * mean(status %in% c("boundary_model", "boundary_both")),
                pct_no_fit = 100 * mean(status %in% c("degenerate", "constant"))), by = method]
  acc <- vapply(per$method, function(m) {
    if (m == "random") return(mean(Xm))
    s <- im[method == m]; ord <- selection_order(setNames(s$value, s$item), colnames(Xm), m)
    mean(Xm[, ord[seq_len(k)]])
  }, 0)
  per[, `:=`(acc_first_k = acc, benchmark = b, n_ref = nrow(Xr), n_meas = nrow(Xm), n_items = N,
             pct_const_ref = 100 * mean(apply(Xr, 2, function(x) all(x == x[1]))),
             pct_const_meas = 100 * mean(apply(Xm, 2, function(x) all(x == x[1]))),
             mean_acc_ref = mean(Xr), mean_acc_meas = mean(Xm), irt_converged = conv)]
}))
setcolorder(res, c("benchmark", "n_ref", "n_meas", "n_items", "mean_acc_ref", "mean_acc_meas", "pct_const_ref",
                   "pct_const_meas", "irt_converged", "method"))
write_result(res, results_path("diagnostics", paste0(tag, ".csv")))
log_msg("wrote %s", results_path("diagnostics", paste0(tag, ".csv")))
