# Stage 3: full-benchmark G-studies by model population and their D-study
# projections (paper Fig. 2 and the population table).
#
#   Rscript analysis/03_population_gstudies.R
#   Rscript analysis/03_population_gstudies.R --populations=all,top_half,top_frac:0.1,top_n:10
#
# Output: results/gstudy/population_gstudies.csv, results/gstudy/dstudy_curves.csv.gz

source("analysis/_common.R")

flags <- parse_flags(defaults = list(benchmarks = "all18", populations = c("all", "top_half", "top_n:10"),
                                     missing = "auto"))
long <- load_responses(benchmarks = flags$benchmarks)
scores <- model_scores(long)

res <- timed(population_gstudies(long, scores, sort(unique(long$benchmark)), flags$populations, missing = flags$missing))
write_result(res$gstudy, results_path("gstudy", "population_gstudies.csv"))
write_result(res$dstudy, results_path("gstudy", "dstudy_curves.csv.gz"))
log_msg("G-studies for %d benchmark x population cells in %.1fs", nrow(res$gstudy), attr(res, "seconds"))
print(res$gstudy[, .(benchmark, population, n_models, n_items, g_rel = round(g_rel, 3), g_abs = round(g_abs, 3))])
