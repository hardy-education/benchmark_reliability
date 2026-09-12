#!/usr/bin/env Rscript
# ==============================================================================
# Stage 20 -- remove draw-level variation from the item metrics, then aggregate.
#
# Stage 10 computes each item's metrics many times, on samples that differ in
# size and composition. A raw average across those draws would confound the
# item's own properties with the draws it happened to appear in: a metric
# estimated on a 10-item sample is on a different scale from the same metric on
# a 200-item sample.
#
# So each metric is residualised on a (replicate x bootstrap x benchmark) fixed
# effect before averaging, leaving the item-specific deviation. `fixest`'s
# multi-outcome syntax does this for every metric in one pass.
#
# Output: data/results/boots/item_criteria_<date>.rds -- one row per
# (benchmark, item), one column per criterion, ready for item selection.
# ==============================================================================

source(here::here("R", "load_all.R"))
library(fixest)

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (!length(hit)) default else sub(paste0("^--", name, "="), "", hit[1])
}

CFG <- list(
  # Matches stage-10 output only. Stage 20's own output is named
  # `item_criteria_*`, so a rerun cannot read its own results back in as if
  # they were raw draws.
  pattern     = opt("pattern", "^item_metrics_[^ ]*\\.rds$"),
  min_valid   = as.integer(opt("min-valid", "2")),
  random_seed = as.integer(opt("seed", "12345"))
)

br_init_dirs()

# --- Collect the stage-10 output ---------------------------------------------
files <- list.files(BR_BOOTS, pattern = CFG$pattern, full.names = TRUE)
if (!length(files)) {
  stop(glue("No stage-10 output matching '{CFG$pattern}' in {BR_BOOTS}. ",
            "Run scripts/10_item_metrics.R first."))
}
message(glue("Reading {length(files)} stage-10 result file(s)."))

raw <- purrr::map(files, readRDS) |> purrr::list_rbind()

# --- Derived metrics ----------------------------------------------------------
# Names follow the results tables. `L6` is lambda*_6, not classical lambda_6.
#
# Which columns exist depends on what stage 10 could compute: the IRT
# discrimination `a` needs a converged 2PL, and the binary-similarity family
# (`cralpha`, `forbes_adj`, ...) needs the optional `item_similarity_enhanced()`
# helper, which is not part of this repository. So every derived metric is
# guarded on the columns it actually needs, via `col()`, rather than assuming a
# fixed schema. The original assumed the full schema and would fail outright on
# a run without the optional helper.
col <- function(df, name) {
  if (name %in% names(df)) df[[name]] else rep(NA_real_, nrow(df))
}
has <- function(df, ...) all(c(...) %in% names(df))

adjusted_input <- raw

# Corrected-attenuation discrimination: squared corrected item-total
# correlation over alpha-with-item-dropped.
adjusted_input$ctt_disc <- col(adjusted_input, "r_rest")
adjusted_input$itcor    <- col(adjusted_input, "raw_r")
adjusted_input$obsr2    <- col(adjusted_input, "meanobsr2")
adjusted_input$CA       <- adjusted_input$ctt_disc^2 / col(adjusted_input, "alpha_rest")

# Uniqueness: the share of an item's variance the shared signal does not
# explain, and its product with L6 -- high only for items that are both
# well-predicted and carry independent variance.
adjusted_input$uniq_L6 <- 1 - col(adjusted_input, "L6")
adjusted_input$L6_qual <- adjusted_input$uniq_L6 * col(adjusted_input, "L6")

adjusted_input <- adjusted_input |>
  dplyr::mutate(
    # Pseudo-lambda_6 transform: rescale each R-squared variant by the item's
    # own variance, so a highly predictable but nearly-constant item is not
    # rewarded for having no variance to explain.
    dplyr::across(
      dplyr::any_of(c("cor2", "ar2", "pr2", "apr2", "wr2", "war2", "wpr2", "wapr2")),
      \(x) 1 - (1 - x) / i_var,
      .names = "{.col}_pgl6"
    )
  )

adjusted_input$L6_war_qual <- if (has(adjusted_input, "war2_pgl6")) {
  col(adjusted_input, "L6") * (1 - adjusted_input$war2_pgl6)
} else NA_real_

adjusted_input$alph_qual <- if (has(adjusted_input, "snr_alpha", "cralpha")) {
  adjusted_input$snr_alpha * (1 - adjusted_input$cralpha)
} else NA_real_

# An item is "valid" for a draw if the regression produced an estimate at all.
adjusted_input$valid_item <- !is.na(dplyr::coalesce(
  col(adjusted_input, "cor2"), col(adjusted_input, "L6")
))

adjusted_input <- adjusted_input |>
  dplyr::group_by(item) |>
  dplyr::mutate(n_valid_draws = sum(valid_item, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  # An item seen in only one or two draws has no stable estimate to residualise.
  dplyr::filter(n_valid_draws > CFG$min_valid) |>
  collapse::replace_inf() |>
  # Columns that are all-NA (an unavailable metric) or constant carry no
  # information and would make the multi-outcome regression singular.
  dplyr::select(dplyr::where(\(x) !all(is.na(x)))) |>
  data.table::as.data.table() |>
  janitor::remove_constant()

if (nrow(adjusted_input) == 0L) {
  stop("No items survived the min-valid-draws filter (--min-valid=",
       CFG$min_valid, "). Run stage 10 with more replicates.")
}

# --- Which columns are criteria ----------------------------------------------
# Everything numeric that is not an identifier or a sample descriptor.
ID_COLS <- c("benchmark", "item", "model", "iter", "iter_grp", "boot", "budget",
             "split", "samp", "irt", "n_iobs", "nmods", "ni", "nobs", "valid_n",
             "n_valmods", "n_models", "n_items", "valid_item", "n_valid_draws",
             "held_out")

candidate_cols <- setdiff(
  names(adjusted_input)[vapply(adjusted_input, is.numeric, logical(1))],
  ID_COLS
)

# A criterion can only be residualised on the draw fixed effect if it has
# enough non-missing observations and some variation left within the fixed
# effect. A column failing either test makes the multi-outcome regression
# singular and aborts the whole fit, so they are dropped up front and reported
# rather than being discovered as a `reshape_env` error from inside `fixest`.
fe_key <- interaction(adjusted_input$iter, adjusted_input$boot,
                      adjusted_input$benchmark, drop = TRUE)

usable <- vapply(candidate_cols, function(nm) {
  x <- adjusted_input[[nm]]
  if (sum(!is.na(x)) < 3L) return(FALSE)
  within_sd <- tapply(x, fe_key, function(v) stats::sd(v, na.rm = TRUE))
  any(is.finite(within_sd) & within_sd > 0)
}, logical(1))

metric_cols <- candidate_cols[usable]
dropped     <- candidate_cols[!usable]

if (length(dropped)) {
  message(glue(
    "Dropping {length(dropped)} criteria with no within-draw variation: ",
    "{paste(dropped, collapse = ', ')}"
  ))
}
if (!length(metric_cols)) {
  stop("No criteria have within-draw variation; nothing to residualise.")
}
message(glue("Residualising {length(metric_cols)} criteria."))

# --- Residualise on the draw fixed effect -------------------------------------
# `c(a, b, c) ~ 1 | iter^boot^benchmark` fits one model per outcome sharing the
# same fixed-effect structure, which is far cheaper than looping.
fe_formula <- stats::as.formula(glue(
  "c({paste(metric_cols, collapse = ',')}) ~ 1 | iter^boot^benchmark"
))

tic("fixed-effect adjustment")
fits <- fixest::feols(fe_formula, data = adjusted_input, panel.id = ~ item + iter)
resid_mat <- as.data.frame(fixest:::residuals.fixest_multi(fits, na.rm = FALSE))
names(resid_mat) <- metric_cols
toc()

# --- Aggregate to one row per item -------------------------------------------
item_metrics <- adjusted_input |>
  dplyr::select(-dplyr::any_of(metric_cols)) |>
  dplyr::bind_cols(resid_mat) |>
  tidyr::pivot_longer(dplyr::any_of(metric_cols),
                      names_to = "metric", values_to = "est") |>
  dplyr::group_by(benchmark, item, metric) |>
  dplyr::summarise(est = mean(est, na.rm = TRUE), .groups = "drop") |>
  tidyr::pivot_wider(names_from = metric, values_from = est)

# --- Raw item difficulty, and the random baselines ---------------------------
# Difficulty is deliberately *not* residualised: it is a property of the item,
# not of the draw, and it doubles as a selection criterion in its own right
# (the "Raw difficulty" series in Fig. 3).
responses <- load_responses(benchmarks = unique(item_metrics$benchmark))
splits    <- make_hilo_splits(responses)

difficulty <- responses |>
  dplyr::semi_join(
    splits |> dplyr::filter(split == "train") |> dplyr::select(model, benchmark),
    by = c("model", "benchmark")
  ) |>
  fgroup_by(benchmark, item) |>
  fsummarise(diff = 1 - fmean(resp)) |>
  fungroup()

item_metrics <- item_metrics |>
  dplyr::left_join(difficulty, by = c("benchmark", "item")) |>
  add_random_baselines(n_draws = 6L, seed = CFG$random_seed)

# --- Composite criteria -------------------------------------------------------
# Rank-averaged combinations, included in the comparison to test whether
# blending criteria beats any single one.
# Each composite is a mean of dense ranks, so it is only defined when all of
# its constituents were computed.
rank_mean <- function(df, cols, signs = rep(1, length(cols))) {
  if (!all(cols %in% names(df))) return(rep(NA_real_, nrow(df)))
  ranks <- purrr::map2(cols, signs, \(cc, sgn) dplyr::dense_rank(sgn * df[[cc]]))
  Reduce(`+`, ranks) / (length(cols) * nrow(df))
}

item_metrics <- item_metrics |>
  dplyr::group_by(benchmark) |>
  dplyr::group_modify(function(g, ...) {
    g$dalf <- rank_mean(g, c("diff", "L6", "a", "forbes_adj"))
    g$comp <- rank_mean(g, c("L6", "Hi", "a", "cralpha"), c(1, 1, 1, -1))
    g
  }) |>
  dplyr::ungroup()

missing_composites <- c("dalf", "comp")[
  vapply(c("dalf", "comp"), \(nm) all(is.na(item_metrics[[nm]])), logical(1))
]
if (length(missing_composites)) {
  message(glue(
    "Composite criteria not computable from this run: ",
    "{paste(missing_composites, collapse = ', ')} ",
    "(their inputs need the 2PL fit and item_similarity_enhanced())."
  ))
}

out_file <- file.path(BR_BOOTS, glue("item_criteria_{Sys.Date()}.rds"))
saveRDS(item_metrics, out_file)
message(glue("Wrote {out_file} ({nrow(item_metrics)} items, ",
             "{ncol(item_metrics)} columns)."))
message("Stage 20 complete. Next: scripts/30_gd_studies.R")
