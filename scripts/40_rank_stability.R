#!/usr/bin/env Rscript
# ==============================================================================
# Stage 40 -- empirical rank stability: Kendall's tau against the full-benchmark
#             ranking as items accumulate in criterion order.
#
# This is the observable counterpart to the variance-component reliability of
# stage 30. Theory (Eq. 17) says the two should agree; Fig. 1B checks it, and
# Fig. 3C reports the tau curves per selection criterion.
#
# Three model populations are scored: the lower- and higher-performing halves
# (Eqs. 9-10) and a random half of the same size, which is the control that
# makes the population effect interpretable rather than a cohort-size artefact.
#
# Output: data/results/boots/rank_stability_<date>.rds
# ==============================================================================

source(here::here("R", "load_all.R"))

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (!length(hit)) default else sub(paste0("^--", name, "="), "", hit[1])
}

CFG <- list(
  max_items = as.integer(opt("max-items", "500")),
  adjusted  = opt("adjusted", NA),
  workers   = as.integer(opt("workers", NA)),
  seed      = as.integer(opt("seed", "12345"))
)

br_init_dirs()
br_plan(workers = if (is.na(CFG$workers)) NULL else CFG$workers)
set.seed(CFG$seed)

# --- Inputs -------------------------------------------------------------------
adjusted_file <- if (!is.na(CFG$adjusted)) CFG$adjusted else {
  candidates <- list.files(BR_BOOTS, pattern = "^item_criteria_.*\\.rds$",
                           full.names = TRUE)
  if (!length(candidates)) stop("Run scripts/20_adjust_metrics.R first.")
  sort(candidates, decreasing = TRUE)[1]
}
item_metrics <- readRDS(adjusted_file)

responses <- load_responses(benchmarks = unique(item_metrics$benchmark))
splits    <- make_hilo_splits(responses)

criteria <- prepare_item_criteria(
  item_metrics,
  metrics = c(setdiff(names(item_metrics), c("benchmark", "item")), RANDOM_METRICS)
) |>
  dplyr::rename(estimate = est)

#' Attach the reference ranking each reduced ranking is compared against
#'
#' `average` is the model's mean score over the *whole* benchmark, so tau
#' measures how well a k-item subset recovers the full-benchmark ordering.
with_reference_ranking <- function(responses, population_table) {
  responses |>
    dplyr::inner_join(
      population_table |>
        dplyr::select(benchmark, model, resp, split) |>
        dplyr::rename(average = resp, samp = split) |>
        dplyr::distinct(),
      by = c("benchmark", "model")
    ) |>
    tidyr::drop_na()
}

tic("Stage 40 total")

# --- The two performance halves ----------------------------------------------
benched <- with_reference_ranking(responses, splits)
tic("performance-split populations")
rank_hilo <- cumulative_kendall(benched, criteria, max_items = CFG$max_items)
toc()

# --- The random-half control --------------------------------------------------
# Drawn once with a fixed seed so the control is identical across reruns; the
# original drew it inside an ungated top-level block, so every rerun produced a
# different baseline.
random_half <- make_random_split(splits, prop = 0.5)
benched_rand <- responses |>
  dplyr::inner_join(
    random_half |>
      dplyr::select(benchmark, model, resp) |>
      dplyr::rename(average = resp) |>
      dplyr::distinct(),
    by = c("benchmark", "model")
  ) |>
  tidyr::drop_na() |>
  dplyr::mutate(samp = "rand")

tic("random-half control")
rank_rand <- cumulative_kendall(benched_rand, criteria, max_items = CFG$max_items)
toc()

rank_stability <- data.table::rbindlist(list(rank_hilo, rank_rand),
                                        use.names = TRUE, fill = TRUE)

out_file <- file.path(BR_BOOTS, glue("rank_stability_{Sys.Date()}.rds"))
saveRDS(rank_stability, out_file)
message(glue("Wrote {out_file} ({nrow(rank_stability)} rows)."))

toc()
message("Stage 40 complete. Next: scripts/60_figures.R")
