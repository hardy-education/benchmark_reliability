#!/usr/bin/env Rscript
# ==============================================================================
# Stage 10 -- per-item reliability metrics, bootstrapped over item samples.
#
# For each benchmark, item budget, and bootstrap replicate: draw a response
# matrix from the lower-performing model population, then compute every
# candidate item-selection criterion on it -- lambda*_6 (Sec. 3.3), 2PL
# discrimination, Mokken H_i / Z_i, Molenaar-Sijtsma, alpha-drop statistics,
# and the inter-item correlation summary.
#
# Output: one RDS per (benchmark, budget) under data/results/boots/.
#
# This is the expensive stage -- hours to days depending on the budget grid.
# The paper's own timing note (App. D.4) is 3,579 s for a single AirBench
# lambda*_6 estimation at 3,758 items x 21 models.
#
# Usage:
#   Rscript scripts/10_item_metrics.R                    # full grid
#   Rscript scripts/10_item_metrics.R --benchmarks=musr,gpqa --budgets=25,50
#   Rscript scripts/10_item_metrics.R --boots=2 --no-irt
# ==============================================================================

source(here::here("R", "load_all.R"))

# --- Configuration ------------------------------------------------------------
# Exposed as command-line flags rather than edited in place. The original
# guarded each stage behind an `if (rerunbootstraps)` flag that had to be
# toggled by hand, and was reassigned at two different points in the file, so
# which stages actually ran depended on how much of the script you had
# evaluated.
args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^--", name, "="), "", hit[1])
}
flag <- function(name) any(args == paste0("--", name))

CFG <- list(
  benchmarks = strsplit(opt("benchmarks", ""), ",")[[1]],
  budgets    = as.integer(strsplit(opt("budgets", ""), ",")[[1]]),
  n_boots    = as.integer(opt("boots", "10")),
  population = opt("population", "train"),
  workers    = as.integer(opt("workers", NA)),
  run_irt    = !flag("no-irt"),
  seed       = as.integer(opt("seed", "20260101"))
)
# Default grid: every integer budget from 1 to 200, as in the original.
if (!length(CFG$budgets) || all(is.na(CFG$budgets))) CFG$budgets <- 1:200

br_init_dirs()
br_plan(workers = if (is.na(CFG$workers)) NULL else CFG$workers)
set.seed(CFG$seed)

# --- Data ---------------------------------------------------------------------
responses <- load_responses(
  benchmarks = if (length(CFG$benchmarks)) CFG$benchmarks else NULL
)
splits       <- make_hilo_splits(responses)
item_counts  <- benchmark_summary(responses)
benchmarks   <- unique(responses$benchmark)

message(glue(
  "Stage 10: {length(benchmarks)} benchmark(s) x {length(CFG$budgets)} budget(s) ",
  "x {CFG$n_boots} replicate(s), population '{CFG$population}'."
))

#' Every item-level metric for one sampled response matrix
#'
#' Each metric is joined by `item`, and each is individually fault-tolerant: a
#' benchmark where (say) the omega fit will not converge should still contribute
#' its other metrics rather than losing the whole draw.
item_metrics_for_sample <- function(wide, metadata, run_irt = TRUE) {
  out <- metadata

  safe_join <- function(acc, expr, label) {
    res <- tryCatch(expr, error = function(e) {
      warning(glue("{label} failed: {conditionMessage(e)}"))
      NULL
    })
    if (is.null(res)) acc else dplyr::left_join(acc, res, by = "item")
  }

  if (run_irt) {
    irt <- tryCatch(
      fit_irt(wide, metadata = metadata, item_types = "2PL", plots = FALSE),
      error = function(e) {
        warning(glue("IRT fit failed: {conditionMessage(e)}"))
        NULL
      }
    )
    if (!is.null(irt) && nrow(irt$items)) {
      out <- dplyr::left_join(
        out,
        irt$items |> dplyr::filter(irt == "2PL") |>
          dplyr::select(-dplyr::any_of(names(metadata)[names(metadata) != "item"])),
        by = "item"
      )
    }
  }

  tic("item metrics")
  out <- safe_join(out, item_mokken_hz(wide), "Mokken H_i / Z_i")
  out <- safe_join(out, lambda_star_components(wide, n_workers = 1L), "lambda*_6")
  out <- safe_join(out, item_correlation_summary(item_correlations_long(wide)),
                   "inter-item correlations")
  out <- safe_join(out, item_alpha(wide), "Cronbach alpha")
  # `item_similarity_enhanced()` is an optional external helper providing the
  # "bonus" binary-similarity criteria (Jaccard, F1, Forbes, ...). It is not
  # part of this repository; when it is on the search path its columns are
  # added, and otherwise the pipeline proceeds without them. The original
  # depended on the same `exists()` check, which is why several result files
  # carry those columns and others do not.
  if (exists("item_similarity_enhanced", mode = "function")) {
    out <- safe_join(out,
                     item_similarity_enhanced(item_block(wide)),
                     "binary similarity criteria")
  }
  toc()
  out
}

# --- Bootstrap loop -----------------------------------------------------------
tic("Stage 10 total")

for (b in benchmarks) {
  n_available <- item_counts$n_items[item_counts$benchmark == b]

  for (ni in CFG$budgets) {
    if (ni > n_available) {
      message(glue("Skipping {b} at {ni} items (only {n_available} available)."))
      next
    }

    draws <- vector("list", CFG$n_boots)

    for (i in seq_len(CFG$n_boots)) {
      message(glue("{b}: budget {ni}, replicate {i}/{CFG$n_boots}"))
      tic(glue("{b} n{ni} boot{i}"))

      wide <- sample_response_matrix(
        splits, responses,
        benchmark  = b,
        n_items    = ni,
        population = CFG$population
      ) |> as.data.frame()

      metadata <- sample_metadata(wide, benchmark = b, iter = i,
                                  population = CFG$population) |>
        dplyr::mutate(boot = i, budget = ni)

      draws[[i]] <- item_metrics_for_sample(wide, metadata,
                                            run_irt = CFG$run_irt)
      toc()
      gc()
    }

    # One file per (benchmark, budget). The original re-serialised the entire
    # accumulated result list on every budget, so writing was quadratic in the
    # number of draws and a crash late in the grid lost nothing but cost hours
    # of redundant I/O.
    out_file <- file.path(
      BR_BOOTS,
      glue("item_metrics_{b}_n{ni}_{CFG$population}_{Sys.Date()}.rds")
    )
    saveRDS(purrr::list_rbind(draws), out_file)
    message(glue("Wrote {out_file}"))
  }
}

toc()
message("Stage 10 complete. Next: scripts/20_adjust_metrics.R")
