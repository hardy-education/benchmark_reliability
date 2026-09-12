# ==============================================================================
# 06_rank_stability.R -- item selection and empirical rank stability.
#
# Paper: Sec. 3.3 ("Role in reliability optimization") and Sec. 4 ("Efficient
# item selection improves reliability at fixed evaluation budgets").
#
# The experiment is: rank items by a candidate criterion, take the top k, score
# every model on just those k items, and measure Kendall's tau between that
# reduced ranking and the full-benchmark ranking. Doing this for k = 1..K gives
# the reliability-versus-budget curves of Figs. 1C and 3C, and comparing
# criteria against the `rand` baseline is what "better than random subsampling"
# means.
# ==============================================================================

# ------------------------------------------------------------------------------
# Cumulative rank concordance
# ------------------------------------------------------------------------------

#' Kendall's tau against the full-benchmark ranking, as items accumulate
#'
#' For each (benchmark, criterion, population), orders items by the criterion,
#' forms the running mean score per model over the first `n` items, and
#' correlates that running mean with each model's full-benchmark average.
#'
#' `pcaPP::cor.fk` is a Fast Kendall implementation, O(n log n) rather than
#' O(n^2); with 429 models and 500 item budgets the naive version dominates the
#' runtime.
#'
#' @param responses Long response frame with `benchmark`, `item`, `model`,
#'   `resp`, `samp` (population label), and `average` (each model's
#'   full-benchmark mean score, the reference ranking).
#' @param item_scores Long frame with `benchmark`, `item`, `metric`, `estimate`
#'   -- one criterion value per item per criterion.
#' @param max_items Truncate each curve at this many items.
#' @return data.table with `benchmark`, `metric`, `samp`, `n_items`, `kcor`.
cumulative_kendall <- function(responses, item_scores, max_items = 500L) {
  required <- c("benchmark", "item", "model", "resp", "samp", "average")
  missing_cols <- setdiff(required, names(responses))
  if (length(missing_cols)) {
    stop("`responses` is missing: ", paste(missing_cols, collapse = ", "))
  }

  responses   <- data.table::as.data.table(responses)[, ..required]
  item_scores <- data.table::as.data.table(item_scores)[
    , .(benchmark, item, metric, estimate)
  ]

  # One task per criterion. Splitting on `metric` keeps each worker's join
  # small; the original also did this, but additionally passed `benched`
  # through `future.globals` while the closure already captured it, so the
  # whole response table was serialised to every worker twice.
  metric_groups <- split(item_scores, by = "metric", keep.by = TRUE)

  results <- future.apply::future_lapply(
    metric_groups,
    function(scores_one_metric) {
      dt <- responses[scores_one_metric, on = .(benchmark, item), nomatch = 0L]

      # Best items first.
      data.table::setorder(dt, benchmark, -estimate)

      if (!is.null(max_items) && max_items > 0L) {
        dt <- dt[, utils::head(.SD, max_items), by = .(benchmark, model, samp)]
      }

      # Running mean score per model as items are added in criterion order.
      dt[, n_items  := seq_len(.N),               by = .(benchmark, model, samp)]
      dt[, cum_mean := cumsum(resp) / seq_len(.N), by = .(benchmark, model, samp)]

      dt[, .(kcor = suppressWarnings(pcaPP::cor.fk(cum_mean, average))),
         by = .(benchmark, metric, samp, n_items)]
    },
    future.seed     = TRUE,
    future.packages = c("data.table", "pcaPP")
  )

  data.table::rbindlist(results, use.names = TRUE)
}

# ------------------------------------------------------------------------------
# Item selection
# ------------------------------------------------------------------------------

#' Criteria whose sign must be reversed before selection
#'
#' Some criteria are "lower is better" -- misfit statistics, the share of
#' negative inter-item correlations, standard errors, uniqueness measures. To
#' compare every criterion under a single "take the highest" rule they are
#' mapped to `1 - x` and suffixed `_rv`, so a reversed criterion is always
#' visible in the output as such.
REVERSE_CODED_METRICS <- c(
  "outfit", "infit", "z.infit",       # IRT misfit: large means the item misfits
  "clow", "cneg", "average_r",        # inter-item correlation pathologies
  "cse", "cvar",                      # instability of an item's correlations
  "std_error", "obsr2", "war2_pgl6",  # lambda*_6 precision / uniqueness
  "pr_t", "t_value", "b",             # regression diagnostics, item difficulty
  "cralpha", "del_alpha",             # alpha contribution
  "f1", "jac", "star_kap", "Star_f1", "Star_j", "hardy_f1", "star_P4",
  "hardy_j", "star_taub", "hardy_tau", "sorgen_star", "pmi", "pmi2",
  "Zh"
)

#' Criteria taken in absolute value before selection
#'
#' Infit and outfit are centred on 1, so both tails indicate misfit.
ABS_CODED_METRICS <- c("outfit", "infit")

#' Reshape a wide item-metric table into long criterion form, normalising sign
#'
#' @param item_metrics One row per (benchmark, item), one column per criterion.
#' @param metrics Criterion column names to keep.
#' @return Long frame with `benchmark`, `item`, `metric`, `est`, where higher
#'   `est` always means "select this item sooner".
prepare_item_criteria <- function(item_metrics, metrics) {
  item_metrics |>
    tidyr::pivot_longer(
      dplyr::any_of(metrics),
      names_to = "metric", values_to = "est"
    ) |>
    dplyr::mutate(
      est    = dplyr::if_else(metric %in% ABS_CODED_METRICS, abs(est), est),
      est    = dplyr::if_else(metric %in% REVERSE_CODED_METRICS, 1 - est, est),
      metric = dplyr::if_else(metric %in% REVERSE_CODED_METRICS,
                              paste0(metric, "_rv"), metric)
    )
}

#' Add random-selection baselines
#'
#' The comparison the paper's claim rests on: an item-selection criterion has to
#' beat random subsampling at the same budget. Several independent draws are
#' added so the baseline carries its own sampling variability rather than being
#' a single lucky or unlucky subset.
#'
#' @param item_metrics One row per (benchmark, item).
#' @param n_draws Number of independent random criteria to add.
#' @param seed Seed, so the baseline is reproducible.
add_random_baselines <- function(item_metrics, n_draws = 6L, seed = 12345L) {
  withr_seed(seed)
  for (k in seq_len(n_draws)) {
    nm <- if (k == 1L) "rand" else paste0("rand", k)
    item_metrics[[nm]] <- stats::runif(nrow(item_metrics))
  }
  item_metrics
}

RANDOM_METRICS <- c("rand", paste0("rand", 2:10))

# Small helper so the seed is set locally without depending on withr.
withr_seed <- function(seed) {
  if (!is.null(seed)) set.seed(seed)
  invisible(NULL)
}

#' Select the top `n_items` items per benchmark under each criterion
#'
#' @param criteria Long criterion frame from `prepare_item_criteria()`.
#' @param n_items Budget.
#' @param metrics Criteria to include.
#' @return `benchmark`, `item`, `metric` for the selected items.
#'
#' Benchmarks with fewer than `n_items` items contribute all of their items
#' rather than being dropped, which is why some curves in Fig. 2 stop earlier
#' than others.
select_top_items <- function(criteria, n_items, metrics = NULL) {
  if (!is.null(metrics)) criteria <- dplyr::filter(criteria, metric %in% metrics)
  criteria |>
    dplyr::slice_max(order_by = est, by = c(benchmark, metric), n = n_items,
                     with_ties = FALSE) |>
    dplyr::select(benchmark, item, metric) |>
    dplyr::distinct()
}

#' Collapse the several random baselines into one series
#'
#' `rand`, `rand2`, ... are independent draws of the same baseline, so for
#' plotting they are pooled under the single label `rand`.
pool_random_metrics <- function(df, col = "metric") {
  df[[col]] <- dplyr::if_else(df[[col]] %in% RANDOM_METRICS, "rand", df[[col]])
  df
}

# ------------------------------------------------------------------------------
# G/D studies over selected item subsets
# ------------------------------------------------------------------------------

#' Run a G-study for every (benchmark, criterion) item subset
#'
#' For a fixed budget, fits the crossed random-effects model of Eq. (1) to the
#' items each criterion selected, so the resulting reliability is attributable
#' to the selection rule.
#'
#' @param responses Long response frame restricted to the relevant models.
#' @param selected Output of `select_top_items()`.
#' @param verbose Passed to `fit_gstudy()`.
#' @return One row per (benchmark, criterion) of variance components and
#'   coefficients.
gstudy_by_criterion <- function(responses, selected, verbose = FALSE) {
  tic("G-studies over selected item subsets")

  out <- purrr::map(unique(selected$benchmark), function(b) {
    sel_b <- selected |>
      dplyr::filter(benchmark == b) |>
      dplyr::select(item, metric) |>
      dplyr::distinct()
    resp_b <- responses |> dplyr::semi_join(sel_b, by = "item")

    message(glue("G-studies for benchmark {b}"))

    furrr::future_map(
      unique(sel_b$metric),
      function(m) {
        dat <- resp_b |>
          dplyr::inner_join(dplyr::filter(sel_b, metric == m), by = "item")
        fit_gstudy(dat, verbose = verbose) |> dplyr::mutate(metric = m)
      },
      # Progress bars are useful at the console and pure noise in a log file.
      .progress = interactive(),
      .options  = furrr::furrr_options(
        seed = TRUE,
        conditions = structure("condition", exclude = "message")
      )
    ) |>
      purrr::list_rbind()
  }) |>
    purrr::list_rbind()

  toc()
  out
}
