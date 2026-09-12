#!/usr/bin/env Rscript
# ==============================================================================
# Stage 30 -- G/D studies over item budgets and selection criteria.
#
# For each item budget k and each selection criterion, take that criterion's top
# k items and fit the crossed random-effects model of Eq. (1) to them, for both
# model populations. The resulting variance components give the generalizability
# coefficients of Fig. 3A and the SNRs of Fig. 3B.
#
# Output: one RDS per budget under data/results/boots/, named
# `gdstudy_n<k>_<date>.rds`, which is the naming `read_dstudy_results()` parses.
#
# Usage:
#   Rscript scripts/30_gd_studies.R
#   Rscript scripts/30_gd_studies.R --budgets=10,25,50,100 --metrics=L6,a,diff,rand
# ==============================================================================

source(here::here("R", "load_all.R"))

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (!length(hit)) default else sub(paste0("^--", name, "="), "", hit[1])
}

# The budget grid from the paper: dense at the low end, where selection matters
# most, and sparse above 100 where the curves have converged.
DEFAULT_BUDGETS <- c(1:100, seq(110, 240, by = 10), 300, 350, 400, 450, 500)

# The criteria carried into the main figures. The full battery lives in the
# stage-20 output; this is the subset the paper reports.
DEFAULT_METRICS <- c("L6", "MS", "CA", "Hi", "Zi", "a", "snr_alpha",
                     "ctt_disc", "G2", "itcor", "cralpha_rv", "diff")

CFG <- list(
  budgets = {
    v <- as.integer(strsplit(opt("budgets", ""), ",")[[1]])
    if (!length(v) || all(is.na(v))) DEFAULT_BUDGETS else v
  },
  metrics = {
    v <- strsplit(opt("metrics", ""), ",")[[1]]
    if (!length(v)) DEFAULT_METRICS else v
  },
  adjusted = opt("adjusted", NA),
  workers  = as.integer(opt("workers", NA))
)

br_init_dirs()
br_plan(workers = if (is.na(CFG$workers)) NULL else CFG$workers)

# --- Inputs -------------------------------------------------------------------
adjusted_file <- if (!is.na(CFG$adjusted)) CFG$adjusted else {
  candidates <- list.files(BR_BOOTS, pattern = "^item_criteria_.*\\.rds$",
                           full.names = TRUE)
  if (!length(candidates)) {
    stop("No adjusted item metrics found. Run scripts/20_adjust_metrics.R first.")
  }
  # Most recent by name, which sorts by the embedded date.
  sort(candidates, decreasing = TRUE)[1]
}
message(glue("Using adjusted metrics: {basename(adjusted_file)}"))

item_metrics <- readRDS(adjusted_file)
criteria <- prepare_item_criteria(
  item_metrics,
  metrics = c(CFG$metrics, RANDOM_METRICS)
)

responses <- load_responses(benchmarks = unique(item_metrics$benchmark))
splits    <- make_hilo_splits(responses)

# The two model populations of Eqs. (9)-(10). Both are scored at every budget,
# which is what makes the population dependence in Table 1 measurable.
populations <- c(higher = "test", lower = "train")

# --- Sweep --------------------------------------------------------------------
tic("Stage 30 total")

for (k in CFG$budgets) {
  out_file <- file.path(BR_BOOTS, glue("gdstudy_n{k}_{Sys.Date()}.rds"))
  if (file.exists(out_file)) {
    message(glue("Budget {k}: {basename(out_file)} exists, skipping."))
    next
  }

  message(glue("--- budget {k} items ---"))
  tic(glue("budget {k}"))

  selected <- select_top_items(criteria, n_items = k)

  per_population <- purrr::imap(populations, function(split_label, name) {
    keep <- splits |>
      dplyr::filter(split == split_label) |>
      dplyr::select(benchmark, model) |>
      dplyr::distinct()

    resp <- responses |>
      dplyr::semi_join(selected, by = c("benchmark", "item")) |>
      dplyr::semi_join(keep, by = c("benchmark", "model")) |>
      tidyr::drop_na()

    gstudy_by_criterion(resp, selected) |>
      dplyr::mutate(samp = split_label, population = name)
  })

  saveRDS(purrr::list_rbind(per_population), out_file)
  message(glue("Wrote {basename(out_file)}"))
  toc()
  gc()
}

toc()
message("Stage 30 complete. Next: scripts/40_rank_stability.R")
