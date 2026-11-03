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
#   "all"             every model evaluated on the benchmark
#   "top_half"        models scoring strictly above the benchmark median
#                     (the paper's higher-performing 50%; legacy label "test")
#   "bottom_half"     models at or below the median (legacy label "train")
#   "top_frac:0.1"    the top floor(0.1 * M) models by benchmark score
#   "bottom_frac:0.5" the bottom floor(0.5 * M) models
#   "top_n:10"        the ten highest-scoring models
#   "random_frac:0.5:12345"  a seeded random half (a control for selection on score)
#
# Ties in score are broken by model name so populations are deterministic.

#' Mean score of every model on every benchmark
#'
#' @param long Long table from [load_responses()].
#' @return `data.table(benchmark, model, score, n_items)`.
model_scores <- function(long) {
  long[, .(score = mean(resp), n_items = .N), by = .(benchmark, model)]
}

#' Parse a population string into its parts
#'
#' @param population e.g. `"top_frac:0.1"`.
#' @return list(type, value, seed).
parse_population <- function(population) {
  parts <- strsplit(population, ":", fixed = TRUE)[[1]]
  type <- parts[1]
  known <- c("all", "top_half", "bottom_half", "top_frac", "bottom_frac", "top_n", "random_frac")
  if (!type %in% known) stop("Unknown population '", population, "'. Use one of: ", paste(known, collapse = ", "), call. = FALSE)
  list(type = type,
       value = if (length(parts) > 1) as.numeric(parts[2]) else NA_real_,
       seed = if (length(parts) > 2) as.integer(parts[3]) else 12345L)
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
    top_frac = , bottom_frac = , random_frac = floor(p$value * M),
    top_n = min(p$value, M),
    NA_integer_)
  out <- switch(p$type,
    all = s$model,
    top_half = s[score > median(score)]$model,
    bottom_half = s[score <= median(score)]$model,
    top_frac = , top_n = head(s$model, k),
    bottom_frac = utils::tail(s$model, k),
    random_frac = { set.seed(p$seed); sample(s$model, k) })
  if (length(out) < 2) warning(sprintf("Population '%s' on %s has %d model(s); reliability is undefined.", population, bench, length(out)), call. = FALSE)
  sort(out)
}

#' Human-readable label for a population string
population_label <- function(population) {
  p <- parse_population(population)
  switch(p$type,
    all = "All LLMs",
    top_half = "Top 50%",
    bottom_half = "Bottom 50%",
    top_frac = sprintf("Top %g%%", 100 * p$value),
    bottom_frac = sprintf("Bottom %g%%", 100 * p$value),
    top_n = sprintf("Top n=%g", p$value),
    random_frac = sprintf("Random %g%%", 100 * p$value))
}

#' File-name-safe tag for a population string (`"top_frac:0.1"` -> `"top_frac-0.1"`)
population_tag <- function(population) gsub(":", "-", population, fixed = TRUE)

#' Population sizes for every benchmark
#'
#' Useful before a run: some populations are tiny (top 10% of the 40-41 HELM
#' models on air_bench_2024 and thai_exam is 4 models).
population_sizes <- function(scores, populations) {
  data.table::rbindlist(lapply(unique(scores$benchmark), function(b) {
    data.table::data.table(benchmark = b, population = populations,
                           n_models = vapply(populations, function(p) length(suppressWarnings(population_models(scores, b, p))), 1L))
  }))
}
