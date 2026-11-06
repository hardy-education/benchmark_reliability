# Loading and reshaping the pooled model x item response table.
#
# Input format (data/aggdf.csv): one row per (benchmark, item), one column per
# model, cells in {0, 1, NA}. NA means the model was not evaluated on the item.
# Everything downstream works on either
#   * the long table   -- data.table(benchmark, item, model, resp), NA rows dropped
#   * a response matrix -- numeric matrix, models in rows, items in columns

#' Benchmark sets used in the paper
#'
#' * `main` (alias `core4`): the four benchmarks of the main-text figures.
#' * `legacy_main`: `main` plus MuSR (the earlier draft's main set).
#' * `appendix` (alias `all18`): the 18 item-selection benchmarks in `aggdf.csv`.
#' * `temporal`: the seven Open LLM Leaderboard benchmarks with model upload
#'   dates (paper App. H; populations `early_half` / `late_half`).
#'
#' The Complete set (100+ benchmarks from several sources) is not a subset of
#' `aggdf.csv`; load it with [complete_responses()].
BENCHMARK_SETS <- list(
  main = c("arc_challenge", "bbh", "gpqa", "mmlu_pro"),
  core4 = c("arc_challenge", "bbh", "gpqa", "mmlu_pro"),
  legacy_main = c("arc_challenge", "bbh", "gpqa", "mmlu_pro", "musr"),
  appendix = c("air_bench_2024", "arc_challenge", "bbh", "bbq", "boolq", "commonsense", "gpqa", "ifeval",
               "legalbench", "med_qa", "mmlu_pro", "mmluclinic", "mmlumath", "mmlupromed", "musr",
               "openllm_math", "thai_exam", "wikifact"),
  temporal = c("arc_challenge", "bbh", "gpqa", "ifeval", "mmlu_pro", "musr", "openllm_math")
)

#' Display names for benchmarks (falls back to the raw name)
BENCHMARK_LABELS <- c(
  air_bench_2024 = "AIR-Bench 2024", arc_challenge = "ARC-Challenge", bbh = "BBH", bbq = "BBQ",
  boolq = "BoolQ", commonsense = "CommonSense", gpqa = "GPQA", ifeval = "IFEval", legalbench = "LegalBench",
  med_qa = "MedQA", mmlu_pro = "MMLU-Pro", mmluclinic = "MMLU Clinical", mmlumath = "MMLU Math",
  mmlupromed = "MMLU Prof. Med.", musr = "MuSR", openllm_math = "MATH Lvl 5", thai_exam = "Thai Exam",
  wikifact = "WikiFact"
)

#' Display name of a benchmark
benchmark_label <- function(x) {
  out <- unname(BENCHMARK_LABELS[x])
  ifelse(is.na(out), x, out)
}

#' Human-readable name of a benchmark set
BENCHMARK_SET_LABELS <- c(main = "Main", core4 = "Main", legacy_main = "Legacy Main",
                          appendix = "Appendix", all18 = "Appendix", temporal = "Temporal", complete = "Complete")

#' Load the pooled response table in long format
#'
#' @param path CSV in the `aggdf.csv` layout.
#' @param benchmarks Optional character vector, or the name of an entry in
#'   [BENCHMARK_SETS] (`"core4"`), or `"all18"`/`NULL` for everything.
#' @return `data.table(benchmark, item, model, resp)` keyed by benchmark, with
#'   unobserved cells removed.
load_responses <- function(path = data_path("aggdf.csv"), benchmarks = NULL) {
  if (!file.exists(path)) stop("Response file not found: ", path, "\nSee data/README.md for how to obtain it.", call. = FALSE)
  wide <- data.table::fread(path, showProgress = FALSE)
  benchmarks <- resolve_benchmarks(benchmarks, unique(wide$benchmark))
  wide <- wide[benchmark %in% benchmarks]
  long <- data.table::melt(wide, id.vars = c("benchmark", "item"), variable.name = "model",
                           value.name = "resp", variable.factor = FALSE, na.rm = TRUE)
  long[, item := as.character(item)]
  data.table::setkeyv(long, c("benchmark", "model", "item"))
  long[]
}

#' Expand a benchmark selector into benchmark names
#'
#' @param benchmarks `NULL`, `"all18"`, a [BENCHMARK_SETS] name, or names.
#' @param available Benchmarks present in the data.
resolve_benchmarks <- function(benchmarks, available) {
  if (is.null(benchmarks) || identical(benchmarks, "all18") || identical(benchmarks, "all") ||
      identical(benchmarks, "complete")) return(sort(available))
  if (length(benchmarks) == 1 && benchmarks %in% names(BENCHMARK_SETS)) benchmarks <- BENCHMARK_SETS[[benchmarks]]
  bad <- setdiff(benchmarks, available)
  if (length(bad)) stop("Unknown benchmark(s): ", paste(bad, collapse = ", "), call. = FALSE)
  benchmarks
}

#' Build a models x items response matrix for one benchmark
#'
#' Uses direct index assignment rather than a pivot, so a 429 x 12,032
#' benchmark takes well under a second.
#'
#' @param long Long table from [load_responses()].
#' @param bench Benchmark name.
#' @param models Optional subset of models (rows), e.g. from [population_models()].
#' @param items Optional subset of items (columns).
#' @return Numeric matrix with dimnames `list(model, item)`; NA where unobserved.
response_matrix <- function(long, bench, models = NULL, items = NULL) {
  sub <- long[list(bench), nomatch = 0L]
  if (!is.null(models)) sub <- sub[model %in% models]
  if (!is.null(items)) sub <- sub[item %in% items]
  mods <- models %||% sort(unique(sub$model))
  its <- items %||% unique(sub$item)
  mods <- mods[mods %in% sub$model]
  its <- its[its %in% sub$item]
  X <- matrix(NA_real_, length(mods), length(its), dimnames = list(mods, its))
  X[cbind(match(sub$model, mods), match(sub$item, its))] <- sub$resp
  X
}

#' Resolve missing responses so the exact estimators apply
#'
#' The closed-form estimators in this package need a complete matrix. Only 3 of
#' the 18 benchmarks have unobserved cells (boolq, legalbench, wikifact). The
#' options are:
#'
#' * `"auto"` (default): drop whichever of {incomplete models, incomplete items}
#'   removes fewer cells. For the paper data this drops 110 of 3,316 boolq items,
#'   2 of 91 legalbench models, and 1 of 67 wikifact models.
#' * `"drop_items"`, `"drop_models"`: force one rule.
#' * `"none"`: return `X` unchanged (estimators that support NA, such as the
#'   `lmer` engine of [variance_components()], can then be used).
#'
#' @param X Response matrix.
#' @param policy One of `"auto"`, `"drop_items"`, `"drop_models"`, `"none"`.
#' @return The filtered matrix, with attribute `dropped` describing what went.
handle_missing <- function(X, policy = c("auto", "drop_items", "drop_models", "none")) {
  policy <- match.arg(policy)
  if (policy == "none" || !anyNA(X)) return(structure(X, dropped = list(models = character(), items = character())))
  na <- is.na(X)
  bad_items <- colnames(X)[colSums(na) > 0]
  bad_models <- rownames(X)[rowSums(na) > 0]
  if (policy == "auto") {
    cells_items <- length(bad_items) * nrow(X)
    cells_models <- length(bad_models) * ncol(X)
    policy <- if (cells_items <= cells_models) "drop_items" else "drop_models"
  }
  out <- if (policy == "drop_items") X[, !colnames(X) %in% bad_items, drop = FALSE] else X[!rownames(X) %in% bad_models, , drop = FALSE]
  # Dropping models can, in principle, leave residual NA; finish by dropping items.
  if (anyNA(out)) out <- out[, colSums(is.na(out)) == 0, drop = FALSE]
  attr(out, "dropped") <- list(
    models = setdiff(rownames(X), rownames(out)),
    items = setdiff(colnames(X), colnames(out))
  )
  out
}

#' Columns (items) with zero variance in `X`
#'
#' An item every model passes (or fails) carries no information about model
#' differences in that population; item metrics are undefined for it.
constant_items <- function(X) {
  colnames(X)[matrixStats::colVars(X, na.rm = TRUE) == 0 | colSums(!is.na(X)) < 2]
}

#' Per-benchmark counts of items, models, and coverage
#'
#' Reproduces the benchmark summary table (paper App. H).
benchmark_summary <- function(long) {
  long[, .(
    n_items = data.table::uniqueN(item),
    n_models = data.table::uniqueN(model),
    mean_score = mean(resp),
    coverage = .N / (data.table::uniqueN(item) * data.table::uniqueN(model))
  ), by = benchmark][order(benchmark)]
}
