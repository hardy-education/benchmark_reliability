# ==============================================================================
# 01_data.R -- loading the response matrices, reshaping, and defining the
#              model populations used to study distribution shift.
#
# Paper: Sec. 3.2 (population splits), App. H (the 18 benchmarks).
# ==============================================================================

# ------------------------------------------------------------------------------
# Loading
# ------------------------------------------------------------------------------

#' Load the pooled model-item response matrix
#'
#' `data/aggdf.csv` is stored wide: `item`, `benchmark`, then one column per
#' model (585 of them). Every analysis wants it long, so the pivot happens once
#' here rather than being repeated at the top of each script.
#'
#' @param path Path to the CSV. Defaults to `data/aggdf.csv`.
#' @param benchmarks Optional character vector to subset benchmarks; loading all
#'   18 costs about 25 million rows, so restricting early is worthwhile when
#'   developing.
#' @param drop_missing Drop model-item cells with no recorded response. The
#'   crossed random-effects fits tolerate missingness, but the item-metric
#'   functions need complete columns.
#' @return Long data.table with `benchmark`, `item`, `model`, `resp`.
load_responses <- function(path = file.path(BR_DATA, "aggdf.csv"),
                           benchmarks = NULL,
                           drop_missing = FALSE) {
  if (!file.exists(path)) {
    stop(glue(
      "Response matrix not found at {path}.\n",
      "See data/README.md for the download link and checksum."
    ))
  }
  tic("load responses")
  dt <- data.table::fread(path)
  if (!is.null(benchmarks)) dt <- dt[benchmark %in% benchmarks]

  long <- data.table::melt(
    dt,
    id.vars         = c("benchmark", "item"),
    variable.name   = "model",
    value.name      = "resp",
    variable.factor = FALSE
  )
  if (drop_missing) long <- long[!is.na(resp)]
  toc()
  long[]
}

#' Load the Open LLM Leaderboard model metadata
#'
#' Used only by the facet G-study (scripts/50_facet_gstudy.R), which decomposes
#' benchmark scores across model *attributes* (architecture, contributor, tuning
#' type) rather than items.
#'
#' The derived columns below are the ones the facet models actually reference.
#' The original `full_gstudy_openllmbenches.R` built roughly forty derived
#' columns, most of which no model used; the unused ones are dropped here and
#' the survivors documented.
#'
#' @param path Path to `openllm_all_model_info_full.csv`.
load_model_metadata <- function(path = file.path(BR_DATA, "openllm_all_model_info_full.csv")) {
  if (!file.exists(path)) {
    stop(glue("Model metadata not found at {path}. See data/README.md."))
  }
  readr::read_csv(path, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::mutate(
      model = janitor::make_clean_names(fullname),

      # log10 parameter count; 0-parameter rows are unknowns, not 0B models.
      log_nbpars = dplyr::if_else(number_params_b > 0, log10(number_params_b), NA_real_),

      # Facet (A) Architecture / generation: the architecture string carries a
      # "...For<Task>" or "...Model" suffix that is task rather than family, so
      # it is stripped to recover the base family.
      base = stringr::str_split_i(architecture, "For|Model", i = 1),
      base = dplyr::if_else(is.na(base) | base %in% c("?", "Unknown"), "unk", base),
      base = forcats::as_factor(janitor::make_clean_names(base, allow_dupes = TRUE)),

      # Facet (C) Contributor: the org prefix of the HF model id, plus whether
      # the weights were withdrawn or are unavailable.
      author      = janitor::make_clean_names(stringr::str_split_i(fullname, "/", 1),
                                              allow_dupes = TRUE),
      base_author = janitor::make_clean_names(stringr::str_split_i(base_model, "/", 1),
                                              allow_dupes = TRUE),
      removed   = as.numeric(base_author == "removed"),
      not_avail = as.numeric(available_on_the_hub == 0),

      # Facet (D) Deployment / tuning.
      dplyr::across(
        c(merged, mo_e, chat_template, official_providers, available_on_the_hub, flagged),
        as.numeric
      ),
      adapted_weights = as.numeric(weight_type == "Adapter"),
      type = forcats::as_factor(type)
    ) |>
    dplyr::select(model, dplyr::everything())
}

# The six Open LLM Leaderboard benchmark score columns, as named after
# `clean_names()`. Pivoted long by the facet G-study.
OPENLLM_BENCH_COLS <- c("if_eval", "bbh", "mmlu_pro", "math_lvl_5", "gpqa", "musr")

# ------------------------------------------------------------------------------
# Reshaping
# ------------------------------------------------------------------------------

#' Pivot a long response frame to a wide response matrix
#'
#' @param df Long frame with `item`, `model`, `resp`.
#' @param drop_cols Columns to discard before pivoting.
#' @return Wide data frame: `model` plus one column per item, with constant
#'   columns removed (an item every model gets right carries no information and
#'   breaks correlation-based metrics).
long_to_wide <- function(df, drop_cols = c("benchmark", "split", "med", "iter", "samp")) {
  # Numeric item ids become invalid column names, so they are prefixed.
  prefix <- if (is.numeric(df$item[1])) "i" else NULL
  df |>
    dplyr::select(-dplyr::any_of(drop_cols)) |>
    tidyr::pivot_wider(names_from = item, values_from = resp, names_prefix = prefix %||% "") |>
    janitor::remove_constant()
}

#' Coerce either a long frame or an already-wide matrix to wide form
#'
#' Guard used at the top of each item-metric function so they accept both.
as_response_matrix <- function(df) {
  if (!is.data.frame(df)) stop("Input must be a data frame.")
  if (ncol(df) == 3L && "item" %in% names(df)) return(long_to_wide(df))
  df
}

#' Drop the identifier column from a response matrix
#'
#' Every metric function needs the numeric item block alone. This was written
#' out as `select(-any_of(c("model", "id")))` in fourteen separate places.
item_block <- function(wide) {
  wide |> dplyr::select(-dplyr::any_of(c("model", "id", "subject", "benchmark")))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# ------------------------------------------------------------------------------
# Model populations: the distribution-shift splits (paper Eqs. 9-10)
# ------------------------------------------------------------------------------

#' Split models into lower- and higher-performing halves
#'
#' Implements Eqs. (9)-(10): within each benchmark, compute each model's mean
#' score across items and split at the median.
#'
#'   M_lower  = {m : S_m <= S_med}   labelled "train"
#'   M_higher = {m : S_m >  S_med}   labelled "test"
#'
#' The `train`/`test` labels are inherited from the original code and kept for
#' compatibility with the saved result files. They are *not* a fitting split:
#' "train" is the weaker cohort and "test" the stronger one, which is what
#' Fig. 1 and Table 1 report. `population` gives the unambiguous name.
#'
#' @param df Long response frame.
#' @return One row per (model, benchmark): mean `resp`, the benchmark median
#'   `med`, `split`, and `population`.
make_hilo_splits <- function(df) {
  df |>
    dplyr::group_by(model, benchmark) |>
    dplyr::summarise(resp = mean(resp, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(benchmark) |>
    dplyr::mutate(
      med   = stats::median(resp, na.rm = TRUE),
      split = dplyr::if_else(resp <= med, "train", "test"),
      population = dplyr::if_else(resp <= med, "lower-performing", "higher-performing")
    ) |>
    dplyr::ungroup() |>
    tidyr::drop_na() |>
    dplyr::distinct()
}

#' Split models by upload date rather than by score
#'
#' A temporal analogue of the performance split: models released before the
#' median upload date versus after. Useful as a check that the reliability
#' change in Fig. 1 tracks capability rather than some artefact of splitting on
#' the outcome itself.
#'
#' Requires an `upload_to_hub_date` column, so it applies to the Open LLM
#' Leaderboard benchmarks only.
make_date_splits <- function(df) {
  stopifnot("upload_to_hub_date" %in% names(df))
  df |>
    dplyr::group_by(model, benchmark, upload_to_hub_date) |>
    dplyr::summarise(resp = mean(resp, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(benchmark) |>
    dplyr::mutate(
      med   = stats::median(upload_to_hub_date, na.rm = TRUE),
      split = dplyr::if_else(upload_to_hub_date <= med, "train", "test"),
      population = dplyr::if_else(upload_to_hub_date <= med, "earlier", "later")
    ) |>
    dplyr::ungroup() |>
    tidyr::drop_na() |>
    dplyr::distinct()
}

#' Draw a random half of the models, as the comparison population
#'
#' The "Random Model Sample" series in Fig. 1C. Sampling half the models keeps
#' the population size comparable to each performance half, so the reliability
#' difference cannot be attributed to cohort size.
#'
#' @param splits Output of `make_hilo_splits()`.
#' @param prop Fraction of models to draw per benchmark.
make_random_split <- function(splits, prop = 0.5) {
  splits |>
    dplyr::slice_sample(prop = prop, by = "benchmark") |>
    dplyr::mutate(split = "rand", population = "random") |>
    dplyr::distinct()
}

# ------------------------------------------------------------------------------
# Item sampling
# ------------------------------------------------------------------------------

#' Build a wide response matrix for one benchmark and model population
#'
#' @param splits Output of `make_hilo_splits()`.
#' @param responses Long response frame.
#' @param benchmark Benchmark to draw from.
#' @param n_items Number of items to sample.
#' @param population Which `split` level to keep ("train", "test", or "rand").
#' @param items Optional explicit item ids. Required when `population = "test"`
#'   and you intend to score the same items as a training draw.
#' @param padding Leave this many items unused, so `remove_constant()` cannot
#'   take the matrix below the requested width.
#' @return Wide response matrix.
sample_response_matrix <- function(splits, responses,
                                   benchmark,
                                   n_items = 100,
                                   population = "train",
                                   items = NULL,
                                   padding = 10) {
  keep_models <- splits |>
    dplyr::filter(benchmark == !!benchmark, split == !!population) |>
    dplyr::select(model) |>
    dplyr::distinct()

  long <- responses |>
    dplyr::filter(benchmark == !!benchmark) |>
    dplyr::semi_join(keep_models, by = "model") |>
    dplyr::select(-benchmark)

  available <- n_unique(long$item)
  if (n_items >= available - padding) n_items <- available - padding
  if (n_items < 1L) {
    stop(glue("Benchmark {benchmark} has too few items ({available}) for padding {padding}."))
  }

  if (is.null(items)) {
    # The original guarded this with `&& !(samp == "test")`, so asking for a
    # test-population matrix without passing `items` left `items` NULL and the
    # subsequent `item %in% NULL` filter returned zero rows with no error. Item
    # selection is now independent of which model population was requested;
    # pass `items` explicitly when the two draws must match.
    items <- long |>
      dplyr::distinct(item) |>
      dplyr::slice_sample(n = n_items) |>
      dplyr::pull(item)
  }

  long |>
    dplyr::filter(item %in% items) |>
    long_to_wide() |>
    tidyr::drop_na() |>
    janitor::remove_constant()
}

#' Per-benchmark item and model counts
#'
#' Reproduces the summary of paper Table 4.
benchmark_summary <- function(responses) {
  responses |>
    tidyr::drop_na() |>
    dplyr::group_by(benchmark) |>
    dplyr::summarise(
      n_items    = dplyr::n_distinct(item),
      n_models   = dplyr::n_distinct(model),
      mean_score = mean(resp, na.rm = TRUE),
      .groups    = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(n_items))
}

#' Descriptive metadata for one sampled response matrix
#'
#' Item-level means, SDs and variances plus the sample's identity, carried
#' alongside every bootstrap draw so results can be traced to their draw.
#'
#' @param wide Wide response matrix.
#' @param benchmark Benchmark label. Inferred from the item column names when
#'   `NULL`, which only works if items are named `<benchmark>_<n>`.
#' @param iter Draw index.
#' @param population Model population label.
sample_metadata <- function(wide, benchmark = NULL, iter = 1L, population = "train") {
  if (is.null(benchmark)) {
    benchmark <- names(wide)[3] |> stringr::str_remove("_q?[0-9]+$")
  }
  wide |>
    tidyr::pivot_longer(-model, names_to = "item", values_to = "resp") |>
    dplyr::group_by(item) |>
    dplyr::summarise(
      n_iobs = dplyr::n(),
      i_mean = mean(resp, na.rm = TRUE),
      i_sd   = stats::sd(resp, na.rm = TRUE),
      i_var  = stats::var(resp, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      benchmark = benchmark,
      iter      = iter,
      nmods     = n_unique(wide$model),
      ni        = ncol(wide) - 1L,
      split     = population
    )
}
