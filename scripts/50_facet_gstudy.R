#!/usr/bin/env Rscript
# ==============================================================================
# Stage 50 -- facet G-study on Open LLM Leaderboard metadata.
#
# A different decomposition from the item-level one: instead of partitioning
# score variance into model and item effects, this partitions it across *model
# attributes* -- benchmark (B), architecture/generation (A), contributor (C),
# and deployment/tuning configuration (D) -- plus every interaction. It answers
# which properties of a model, rather than which items, account for score
# variance, and it is the multi-facet generalisation the paper notes the
# framework admits (Sec. 3.1: "can be generalized to richer designs").
#
# Three nested specifications are fitted:
#   base    -- intercept-only random effects for B, A, C, D and interactions
#   slopes  -- adds uncorrelated random slopes on log parameter count (`||`)
#   corr    -- the same with correlated slopes (`|`); the most demanding fit
#
# Output: data/results/facet_gstudy_<spec>_<date>.rds, each holding the fitted
# model and its variance-component table.
#
# Usage:
#   Rscript scripts/50_facet_gstudy.R                 # all three
#   Rscript scripts/50_facet_gstudy.R --specs=base
# ==============================================================================

source(here::here("R", "load_all.R"))
suppressPackageStartupMessages({
  library(lme4)
  library(forcats)
})

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (!length(hit)) default else sub(paste0("^--", name, "="), "", hit[1])
}
CFG <- list(
  specs = {
    v <- strsplit(opt("specs", ""), ",")[[1]]
    if (!length(v)) c("base", "slopes", "corr") else v
  }
)

br_init_dirs()

# --- Data ---------------------------------------------------------------------
metadata <- load_model_metadata()

scores <- metadata |>
  tidyr::pivot_longer(dplyr::all_of(OPENLLM_BENCH_COLS),
                      names_to = "bench", values_to = "val") |>
  dplyr::mutate(not_avail = as.numeric(!available_on_the_hub))

message(glue(
  "Facet G-study: {n_unique(scores$model)} models x ",
  "{length(OPENLLM_BENCH_COLS)} benchmarks = {nrow(scores)} scores."
))

# --- Facet definitions --------------------------------------------------------
# The four facets, written once. The original repeated each 14-term random-effects
# formula in full, three times per specification, which is how the `m1` and
# `mbase` specifications came to differ (one grouped on `base` + `author`, the
# other on `architecture:generation` + `author:removed:not_avail`) without that
# difference being documented anywhere.
FACETS <- c(
  B = "bench",
  A = "architecture:generation",
  C = "author:removed:not_avail",
  D = "type:chat_template:mo_e:merged:precision"
)

#' Build the random-effects terms for all main effects and interactions
#'
#' @param facets Named vector of grouping expressions.
#' @param slope `NULL` for intercept-only terms, or a variable name for random
#'   slopes. `correlated = FALSE` uses `||`, which drops the
#'   intercept-slope covariances and is far cheaper to fit.
#' @param max_order Highest interaction order to include.
facet_terms <- function(facets, slope = NULL, correlated = TRUE, max_order = 3L) {
  groups <- unlist(lapply(seq_len(max_order), function(k) {
    combos <- utils::combn(facets, k, simplify = FALSE)
    vapply(combos, paste, character(1), collapse = ":")
  }))
  lhs <- if (is.null(slope)) "1" else slope
  bar <- if (is.null(slope) || correlated) "|" else "||"
  paste0("(", lhs, " ", bar, " ", groups, ")")
}

#' Assemble the model formula for one specification
facet_formula <- function(spec) {
  switch(spec,
    base = stats::as.formula(paste(
      "val ~ 1 +", paste(facet_terms(FACETS), collapse = " + ")
    )),
    slopes = stats::as.formula(paste(
      "val ~ 1 + log_nbpars +",
      paste(facet_terms(FACETS, slope = "log_nbpars", correlated = FALSE),
            collapse = " + ")
    )),
    corr = stats::as.formula(paste(
      "val ~ 1 + log_nbpars +",
      paste(facet_terms(FACETS, slope = "log_nbpars", correlated = TRUE),
            collapse = " + ")
    )),
    stop("Unknown specification: ", spec)
  )
}

#' Variance-component table with facet attribution
#'
#' Each component is tagged by which facets appear in its grouping, so variance
#' can be aggregated by facet afterwards.
#'
#' The original built these `bench` ... `base_type_bench` indicator columns for
#' two of the three specifications and then applied
#' `across(bench:base_type_bench, ...)` to the third, where the columns did not
#' exist -- so that summary errored. Tagging happens here for every
#' specification, from the same code.
facet_variance_table <- function(fit) {
  lme4::VarCorr(fit) |>
    as.data.frame() |>
    janitor::clean_names() |>
    dplyr::filter(is.na(var2)) |>
    dplyr::mutate(
      pct        = round(vcov / sum(vcov) * 100, 2),
      has_bench  = stringr::str_detect(grp, "bench"),
      has_arch   = stringr::str_detect(grp, "architecture|base|arch"),
      has_author = stringr::str_detect(grp, "author|contrib"),
      has_type   = stringr::str_detect(grp, "type|tune"),
      is_resid   = grp == "Residual",
      order      = stringr::str_count(grp, ":") + 1L
    ) |>
    dplyr::arrange(dplyr::desc(vcov))
}

# --- Fit ----------------------------------------------------------------------
for (spec in CFG$specs) {
  message(glue("--- specification: {spec} ---"))
  tic(glue("facet G-study ({spec})"))

  fit <- lme4::lmer(
    formula   = facet_formula(spec),
    data      = scores,
    na.action = stats::na.exclude,
    control   = lme4::lmerControl(optimizer = "bobyqa", calc.derivs = FALSE)
  )

  var_table <- facet_variance_table(fit)
  print(utils::head(var_table, 15))

  out_file <- file.path(BR_RESULTS, glue("facet_gstudy_{spec}_{Sys.Date()}.rds"))
  # `sum_tab` is always this fit's own table. In the original, the correlated
  # specification was saved alongside the *base* specification's summary, so the
  # saved object silently disagreed with its own model.
  saveRDS(list(spec = spec, formula = facet_formula(spec),
               mod = fit, sum_tab = var_table), out_file)
  message(glue("Wrote {basename(out_file)}"))
  toc()
  gc()
}

message("Stage 50 complete.")
