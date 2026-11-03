# Stage 4: figures from the saved results.
#
#   Rscript analysis/04_figures.R                                  # every curve set found
#   Rscript analysis/04_figures.R --benchmarks=core4 --y=g_rel,kendall
#
# Writes figures/reliability_<y>_<reference>__<measured>_<benchmarks>.pdf and,
# if stage 3 has run, figures/dstudy_populations.pdf.

source("analysis/_common.R")
library(ggplot2)

flags <- parse_flags(defaults = list(benchmarks = "all18", y = c("g_rel", "g_abs", "kendall"), width = 14, height = 10))
benches <- if (identical(flags$benchmarks, "all18")) NULL else
  if (length(flags$benchmarks) == 1 && flags$benchmarks %in% names(BENCHMARK_SETS)) BENCHMARK_SETS[[flags$benchmarks]] else flags$benchmarks
set_tag <- if (is.null(benches)) "all18" else paste(flags$benchmarks, collapse = "-")
dir.create("figures", showWarnings = FALSE)

for (tag in list.dirs(results_path("curves"), full.names = FALSE, recursive = FALSE)) {
  files <- list.files(results_path("curves", tag), pattern = "\\.csv\\.gz$", full.names = TRUE)
  if (!is.null(benches)) files <- files[sub("\\.csv\\.gz$", "", basename(files)) %in% benches]
  if (!length(files)) next
  cv <- rbindlist(lapply(files, fread))
  for (y in flags$y) {
    p <- plot_reliability_curves(cv, y = y, ncol = if (length(files) <= 4) 2 else 5) +
      ggtitle(sprintf("Items scored on: %s   |   reliability measured on: %s",
                      population_label(cv$reference[1]), population_label(cv$measured[1])))
    f <- file.path("figures", sprintf("reliability_%s_%s_%s.pdf", y, tag, set_tag))
    ggsave(f, p, width = if (length(files) <= 4) 9 else flags$width, height = if (length(files) <= 4) 7 else flags$height)
    log_msg("wrote %s", f)
  }
}

ds <- results_path("gstudy", "dstudy_curves.csv.gz")
if (file.exists(ds)) {
  d <- fread(ds)
  if (!is.null(benches)) d <- d[benchmark %in% benches]
  ggsave(file.path("figures", sprintf("dstudy_populations_%s.pdf", set_tag)), plot_dstudy_populations(d), width = 12, height = 9)
}
