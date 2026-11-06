# Stage 3: full-benchmark G-studies by model population and their D-study
# projections (paper Figs. 2-3 and the population table).
#
#   Rscript analysis/03_population_gstudies.R                                  # the 18 aggdf benchmarks
#   Rscript analysis/03_population_gstudies.R --populations=all,top_half,top_frac:0.1,top_n:10
#   Rscript analysis/03_population_gstudies.R --benchmarks=complete --workers=8  # the 100+ Complete set
#
# The Complete set needs data/complete_cells.csv.gz (analysis/00_build_complete.R).
# Its matrices can be incomplete; those are fitted to the observed cells by
# lmer (missing = "none"), the rest by the exact closed form. Populations that
# fail the eligibility rule (population_eligible()) get a row with NA components.
#
# Output (cached; only missing benchmark x population rows are computed):
#   results/gstudy/<name>.csv            name = --name, default the benchmark set
#   results/gstudy/population_gstudies.csv, dstudy_curves.csv.gz   (aggdf sets only; read by stage 4)

source("analysis/_common.R")

flags <- parse_flags(defaults = list(benchmarks = "appendix", populations = c("all", "top_half", "top_frac:0.1", "top_n:10"),
                                     missing = "auto", name = "", workers = 1, force = FALSE))
complete <- identical(flags$benchmarks, "complete")
long <- if (complete) complete_responses() else load_responses(benchmarks = flags$benchmarks)
scores <- model_scores(long, dates = upload_dates())
name <- if (nzchar(flags$name)) flags$name else paste(flags$benchmarks, collapse = "-")

g <- timed(cached_gstudies(long, scores, NULL, flags$populations, name = name,
                           missing = if (complete) "none" else flags$missing,
                           dir = RESULTS, force = isTRUE(flags$force), workers = flags$workers))
log_msg("G-studies: %d benchmark x population rows in %.1fs -> %s", nrow(g), attr(g, "seconds"),
        results_path("gstudy", paste0(name, ".csv")))
if (!complete) {
  write_result(g, results_path("gstudy", "population_gstudies.csv"))
  ok <- g[eligible == TRUE & !is.na(var_model)]
  write_result(dstudy_curve(ok[, .(benchmark, population, var_model, var_item, var_residual, n_models)],
                            unique(round(10^seq(0, 4, by = 0.05)))), results_path("gstudy", "dstudy_curves.csv.gz"))
}
print(g[, .(benchmark, population, eligible, n_models, n_items, status, g_rel = round(g_rel, 3), g_abs = round(g_abs, 3))], nrows = 600)
