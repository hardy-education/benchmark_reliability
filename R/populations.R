# Model populations.
#
# Reliability is a property of a benchmark *and* the population of models it is
# used to compare (paper Sec. 3.4). Two populations matter in an item-selection
# study, and they can differ:
#
#   reference population  -- the models whose responses are used to score items
#                            (and therefore to choose them);
#   measured population   -- the models whose reliability is then evaluated on
#                            the chosen items.
#
# A population is written as a short string so it can be passed on the command
# line and recorded in result file names:
#
#   "all"                   every model evaluated on the benchmark
#   "top_half"              models scoring strictly above the benchmark median
#                           (the paper's higher-performing 50%; legacy label "test")
#   "bottom_half"           models at or below the median (legacy label "train")
#   "middle_half"           the middle 50% by rank (inter-quartile models);
#                           "middle_frac:p" keeps the central share p
#   "top_frac:0.1"          the top floor(0.1 * M) models by benchmark score
#   "bottom_frac:0.5"       the bottom floor(0.5 * M) models
#   "top_n:10"              the ten highest-scoring models
#   "not_top_frac:0.1"      every model NOT in top_frac:0.1 (the bottom 90%)
#   "not_top_n:10"          every model NOT in top_n:10 (the bottom N - 10)
#   "random_frac:0.5:12345" a seeded random half (a control for selection on score)
#   "random_comp:0.5:12345" the models NOT in random_frac:0.5:12345 (the other
#                           half; an out-of-sample, no-shift control)
#   "early_half", "late_half"  models uploaded on or before / after the median
#                           upload date; needs a `date` column in the scores
#                           (see model_upload_dates()). Only benchmarks with a
#                           date for at least 90% of their models are eligible.
#
# The complements make positive-shift pairs that partition the models:
# "not_top_frac:0.1->top_frac:0.1" (bottom 90% -> top 10%) and
# "not_top_n:10->top_n:10" (bottom N-10 -> top 10).
#
# A pair is written "reference->measured"; a single string means both.
# Ties in score are broken by model name so populations are deterministic.

#' Mean score of every model on every benchmark
#'
#' @param long Long table from [load_responses()].
#' @param dates Optional `data.table(model, date)` (e.g. [model_upload_dates()])
#'   for the temporal populations.
#' @return `data.table(benchmark, model, score, n_items[, date])`.
model_scores <- function(long, dates = NULL) {
  s <- long[, .(score = mean(resp), n_items = .N), by = .(benchmark, model)]
  if (!is.null(dates)) s <- merge(s, unique(dates[, .(model, date)], by = "model"), by = "model", all.x = TRUE)
  data.table::setcolorder(s, c("benchmark", "model"))
  s[]
}

POPULATION_TYPES <- c("all", "top_half", "bottom_half", "middle_half", "middle_frac", "top_frac",
                      "bottom_frac", "top_n", "not_top_frac", "not_top_n", "random_frac", "random_comp",
                      "early_half", "late_half")

#' Parse a population string into its parts
#'
#' @param population e.g. `"top_frac:0.1"`.
#' @return list(type, value, seed).
parse_population <- function(population) {
  parts <- strsplit(population, ":", fixed = TRUE)[[1]]
  type <- parts[1]
  if (!type %in% POPULATION_TYPES) stop("Unknown population '", population, "'. Use one of: ",
                                        paste(POPULATION_TYPES, collapse = ", "), call. = FALSE)
  value <- if (length(parts) > 1) as.numeric(parts[2]) else if (type == "middle_half") 0.5 else NA_real_
  list(type = type, value = value, seed = if (length(parts) > 2) as.integer(parts[3]) else 12345L)
}

#' Split a population pair `"ref->meas"` into its two populations
#'
#' A string without `->` is a population used as both reference and measured.
#' @return list(reference, measured).
parse_pair <- function(pair) {
  p <- strsplit(pair, "->", fixed = TRUE)[[1]]
  list(reference = trimws(p[1]), measured = trimws(if (length(p) > 1) p[2] else p[1]))
}

#' Result-directory tag for a pair: `"bottom_half->top_half"` -> `"bottom_half__top_half"`
pair_tag <- function(reference, measured = reference) {
  paste0(population_tag(reference), "__", population_tag(measured))
}

#' Models belonging to a population on one benchmark
#'
#' @param scores Output of [model_scores()].
#' @param bench Benchmark name.
#' @param population Population string (see file header).
#' @return Character vector of model names.
population_models <- function(scores, bench, population = "all") {
  p <- parse_population(population)
  s <- scores[benchmark == bench]
  s <- s[order(-score, model)]            # rank 1 = best; ties broken by name
  M <- nrow(s)
  k <- switch(p$type,
    top_frac = , not_top_frac = , bottom_frac = , random_frac = , random_comp = floor(p$value * M),
    top_n = , not_top_n = min(p$value, M),
    NA_integer_)
  drop <- if (p$type %in% c("middle_half", "middle_frac")) floor((1 - p$value) / 2 * M) else 0L
  out <- switch(p$type,
    all = s$model,
    top_half = s[score > median(score)]$model,
    bottom_half = s[score <= median(score)]$model,
    middle_half = , middle_frac = s$model[seq.int(drop + 1L, M - drop)],
    top_frac = , top_n = head(s$model, k),
    not_top_frac = , not_top_n = utils::tail(s$model, M - k),
    bottom_frac = utils::tail(s$model, k),
    random_frac = .random_half(s$model, k, p$seed),
    random_comp = setdiff(s$model, .random_half(s$model, k, p$seed)),
    early_half = , late_half = {
      if (!"date" %in% names(s)) stop("Population '", population, "' needs a `date` column: ",
                                      "model_scores(long, dates = model_upload_dates()).", call. = FALSE)
      s <- s[!is.na(date)]
      med <- stats::median(s$date)
      if (p$type == "early_half") s[date <= med]$model else s[date > med]$model
    })
  if (length(out) < 2) warning(sprintf("Population '%s' on %s has %d model(s); reliability is undefined.", population, bench, length(out)), call. = FALSE)
  sort(out)
}

# A seeded random subset that does not disturb the caller's RNG stream.
.random_half <- function(models, k, seed) {
  old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv())
  on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()))
  set.seed(seed)
  sample(sort(models), k)
}

#' Is a population usable on a benchmark?
#'
#' Rule for the "top" subsets (top_frac, top_n): the subset must be at most
#' half of the benchmark's models and contain more than `min_models` models.
#' A top 10% therefore needs at least 60 models and a top n = 10 at least 20.
#' The other score-ranked subsets (halves, bottom_frac and the not_top_*
#' complements) need more than `min_models` models. The temporal halves need
#' more than `min_models` models and an upload date for at least
#' `min_dated` of the benchmark's models. Every other population needs at
#' least 2 models. A pair is eligible when both of its populations are.
#'
#' @param scores [model_scores()].
#' @param bench Benchmark name(s).
#' @param population Population string or pair `"ref->meas"`.
#' @param min_models Size must exceed this (default 5).
#' @param max_share Largest allowed share of the benchmark's models (default 0.5).
#' @param min_dated Smallest share of models with an upload date for the
#'   temporal halves (default 0.9).
#' @return Logical, one per benchmark.
population_eligible <- function(scores, bench, population, min_models = 5, max_share = 0.5, min_dated = 0.9) {
  vapply(bench, function(b) {
    pr <- parse_pair(population)
    all(vapply(unique(c(pr$reference, pr$measured)), function(p) {
      sb <- scores[benchmark == b]
      M <- nrow(sb)
      type <- parse_population(p)$type
      if (type %in% c("early_half", "late_half") &&
          (!"date" %in% names(sb) || mean(!is.na(sb$date)) < min_dated)) return(FALSE)
      n <- length(suppressWarnings(population_models(scores, b, p)))
      if (type %in% c("top_frac", "top_n")) n > min_models && n <= max_share * M + 1e-9
      else if (type %in% c("top_half", "bottom_half", "bottom_frac", "not_top_frac", "not_top_n",
                           "early_half", "late_half")) n > min_models
      else n >= 2
    }, NA))
  }, NA, USE.NAMES = FALSE)
}

#' Human-readable label for a population string
population_label <- function(population) {
  vapply(population, function(pop) {
    if (grepl("->", pop, fixed = TRUE)) {
      pr <- parse_pair(pop)
      return(paste(population_label(pr$reference), "->", population_label(pr$measured)))
    }
    p <- parse_population(pop)
    switch(p$type,
      all = "All LLMs",
      top_half = "Top 50%",
      bottom_half = "Bottom 50%",
      middle_half = "Middle 50%",
      middle_frac = sprintf("Middle %g%%", 100 * p$value),
      top_frac = sprintf("Top %g%%", 100 * p$value),
      bottom_frac = sprintf("Bottom %g%%", 100 * p$value),
      top_n = sprintf("Top n=%g", p$value),
      not_top_frac = sprintf("Bottom %g%%", 100 * (1 - p$value)),
      not_top_n = sprintf("Bottom N-%g", p$value),
      random_frac = sprintf("Random %g%%", 100 * p$value),
      random_comp = sprintf("Random %g%% (other half)", 100 * (1 - p$value)),
      early_half = "Earlier 50%",
      late_half = "Later 50%")
  }, "", USE.NAMES = FALSE)
}

#' File-name-safe tag for a population string (`"top_frac:0.1"` -> `"top_frac-0.1"`)
population_tag <- function(population) gsub(":", "-", population, fixed = TRUE)

#' Population sizes and eligibility for every benchmark
#'
#' Useful before a run: some populations are tiny (top 10% of the 40-41 HELM
#' models on air_bench_2024 and thai_exam is 4 models).
population_sizes <- function(scores, populations) {
  data.table::rbindlist(lapply(unique(scores$benchmark), function(b) {
    data.table::data.table(
      benchmark = b, population = populations, n_benchmark_models = nrow(scores[benchmark == b]),
      n_models = vapply(populations, function(p) length(suppressWarnings(population_models(scores, b, p))), 1L),
      eligible = vapply(populations, function(p) population_eligible(scores, b, p), NA))
  }))
}

#' Hugging Face upload dates of the Open LLM Leaderboard models
#'
#' Reads `openllm_all_model_info_full.csv` (see data/README.md) and maps each
#' model's full name to the cleaned names used in `aggdf.csv` (lower case,
#' runs of non-alphanumerics replaced by `_`).
#'
#' @param path CSV with columns `fullname` and `Upload To Hub Date` (m/d/yy).
#' @param official_only Keep only "official providers" models (the paper's subset).
#' @return `data.table(model, date)`.
model_upload_dates <- function(path = data_path("openllm_all_model_info_full.csv"), official_only = FALSE) {
  if (!file.exists(path)) stop("Model metadata not found: ", path, call. = FALSE)
  mi <- data.table::fread(path, select = c("fullname", "Upload To Hub Date", "Official Providers"), showProgress = FALSE)
  data.table::setnames(mi, c("fullname", "date", "official"))
  if (official_only) mi <- mi[official == TRUE]
  mi[, model := clean_model_name(fullname)]
  mi[, date := as.Date(date, format = "%m/%d/%y")]
  unique(mi[!is.na(date), .(model, date)], by = "model")
}

#' Clean a model name the way `janitor::make_clean_names()` did for aggdf.csv
#'
#' Splits camelCase as janitor does (`"OLMo"` -> `"ol_mo"`, `"DeepSeek"` ->
#' `"deep_seek"`), lower-cases, and turns runs of other characters into `_`.
clean_model_name <- function(x) {
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  x <- gsub("([A-Z]+)([A-Z][a-z])", "\\1_\\2", x)
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  sub("^([0-9])", "x\\1", x)          # janitor prefixes a leading digit with x
}
