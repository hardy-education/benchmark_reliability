# Stage 6: the item-selection figures for the paper.
#
#   Rscript analysis/06_paper_figures.R
#   Rscript analysis/06_paper_figures.R --y=kendall --panel-benchmarks=gpqa,mmlu_pro
#   Rscript analysis/06_paper_figures.R --smooth=false     # raw item-by-item curves; files get a _raw suffix
#
# Needs stage 2 output for two runs:
#   run 1: reference = measured = all models         (results/curves/all__all)
#   run 2: reference = measured = top 10% of models  (results/curves/top_frac-0.1__top_frac-0.1)
#
# Writes
#   figures/fig_selection_panel.pdf           main text: subset of benchmarks, run 1 top row, run 2 bottom row
#   figures/fig_selection_all_models.pdf      appendix: all 18 benchmarks, run 1
#   figures/fig_selection_top10.pdf           appendix: all 18 benchmarks, run 2

source("analysis/_common.R")
library(ggplot2)

flags <- parse_flags(defaults = list(y = "g_rel", panel_benchmarks = "core4", smooth = TRUE))
smooth <- isTRUE(flags$smooth)
fig_name <- function(f) if (smooth) f else sub("\\.pdf$", "_raw.pdf", f)
panel_b <- if (length(flags$panel_benchmarks) == 1 && flags$panel_benchmarks %in% names(BENCHMARK_SETS))
  BENCHMARK_SETS[[flags$panel_benchmarks]] else flags$panel_benchmarks

runs <- data.table(
  tag = c("all__all", "top_frac-0.1__top_frac-0.1"),
  run = c("(1) All LLMs: items scored and reliability measured on all models",
          "(2) Top 10%: items scored and reliability measured on the top 10% of models"),
  short = c("All LLMs", "Top 10% of LLMs"),
  file = c("fig_selection_all_models.pdf", "fig_selection_top10.pdf")
)

read_run <- function(tag) {
  files <- list.files(results_path("curves", tag), pattern = "\\.csv\\.gz$", full.names = TRUE)
  if (!length(files)) stop("No curves for ", tag, "; run analysis/02_reliability_curves.R first.")
  rbindlist(lapply(files, fread))
}
cv <- rbindlist(lapply(seq_len(nrow(runs)), function(i) read_run(runs$tag[i])[, run := runs$short[i]]))
cv[, run := factor(run, levels = runs$short)]

dir.create("figures", showWarnings = FALSE)
caption <- if (smooth) {
  "Items added in order of each selection criterion; geom_smooth fit (ggplot2 defaults) with its standard-error band. Random: mean over 10 random orders, smoothed."
} else "Lines: items added in order of each selection criterion. Random: mean and 95% band over 10 random orders."

# Main-text panel: rows = runs, columns = benchmarks.
p <- plot_reliability_curves(cv[benchmark %in% panel_b], y = flags$y, row_by = "run", smooth = smooth) +
  labs(caption = caption) + theme(strip.text.y = element_text(angle = 270))
ggsave(file.path("figures", fig_name("fig_selection_panel.pdf")), p, width = 3 * length(panel_b) + 1, height = 7)
log_msg("wrote figures/%s", fig_name("fig_selection_panel.pdf"))

# Appendix: one figure per run, all benchmarks.
for (i in seq_len(nrow(runs))) {
  p <- plot_reliability_curves(cv[run == runs$short[i]], y = flags$y, ncol = 5, smooth = smooth) +
    ggtitle(runs$run[i]) + labs(caption = caption)
  ggsave(file.path("figures", fig_name(runs$file[i])), p, width = 14, height = 11)
  log_msg("wrote figures/%s", fig_name(runs$file[i]))
}
