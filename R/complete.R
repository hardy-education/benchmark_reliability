# The "Complete" benchmark set: every benchmark with item-level scores that the
# paper's reliability landscape covers (Fig. 3, App. Figs. 24-25).
#
# Five sources, each rebuilt by the scripts in data_collection/:
#
#   aggdf     the 18 HELM / Open LLM Leaderboard benchmarks (data/aggdf.csv)
#   wabm      WABM item scores            (wabm_item_scores/{scores,metrics,items}.csv)
#   openeval  OpenEval response scores    (openeval_response_scores/{scores,metrics,responses}.csv)
#   eee       Every Eval Ever item level  (eee_item_level_filtered.csv)
#   livebench LiveBench                   (livebench.parquet; needs the arrow package)
#
# Each source's primary metric is used. Replicates, sampled runs and prompt
# setups are averaged within each model x item cell, so the table has one score
# per cell: the unit a benchmark score averages over. (The legacy
# gstudy_agg.csv instead fitted lmer to every replicate, so its residual also
# contains run-to-run noise.) Matrices can be incomplete; variance_components()
# then fits the mixed model to the observed cells.

#' Paths to the raw Complete-set sources
#'
#' @param dir Directory holding the source files (default [data_dir()]).
#' @return Named list of paths; a source whose files are missing is skipped by
#'   [build_complete_cells()] with a message.
complete_sources <- function(dir = data_dir()) {
  list(
    wabm = file.path(dir, "wabm_item_scores"),
    openeval = file.path(dir, "openeval_response_scores"),
    eee = file.path(dir, "eee_item_level_filtered.csv"),
    livebench = file.path(dir, "livebench.parquet")
  )
}

#' Build the cell-level table of the non-aggdf Complete benchmarks
#'
#' Reads each source, keeps its primary metric, averages within model x item
#' cells, and writes `out`. Benchmarks whose cells are all identical (nothing
#' to decompose) are dropped. Benchmark names that occur in more than one
#' source get the source as a suffix (`xstest_wabm`).
#'
#' @param sources Output of [complete_sources()].
#' @param out Path of the `.csv.gz` to write (default `data/complete_cells.csv.gz`).
#' @return The table, invisibly: `data.table(dataset, benchmark, item, model, resp, n_obs)`.
build_complete_cells <- function(sources = complete_sources(), out = data_path("complete_cells.csv.gz")) {
  have <- vapply(sources, file.exists, NA)
  for (s in names(sources)[!have]) message("complete: source '", s, "' not found at ", sources[[s]], "; skipped")
  parts <- list()
  if (have[["wabm"]]) {
    d <- sources$wabm
    met <- data.table::fread(file.path(d, "metrics.csv"), showProgress = FALSE)[is_primary == 1, .(metric_uid)]
    its <- unique(data.table::fread(file.path(d, "items.csv"), select = c("item_uid", "benchmark"), showProgress = FALSE))
    sc <- data.table::fread(file.path(d, "scores.csv"), showProgress = FALSE)[metric_uid %in% met$metric_uid]
    sc <- merge(sc, its, by = "item_uid")
    parts$wabm <- sc[, .(resp = mean(value), n_obs = .N),
                     by = .(benchmark, item = as.character(item_uid), model = paste0("wabm_", model_uid))]
  }
  if (have[["openeval"]]) {
    d <- sources$openeval
    met <- data.table::fread(file.path(d, "metrics.csv"), showProgress = FALSE)[is_primary == 1, .(metric_uid, benchmark)]
    sc <- data.table::fread(file.path(d, "scores.csv"), showProgress = FALSE)[metric_uid %in% met$metric_uid]
    sc <- merge(sc, met, by = "metric_uid")
    rs <- data.table::fread(file.path(d, "responses.csv"), select = c("response_uid", "item_uid", "model_uid"), showProgress = FALSE)
    sc <- merge(sc, rs, by = "response_uid")
    # MoralBench mixes two scales (0-1 and a human-rating scale above 1); the
    # legacy analysis split them into two benchmarks.
    sc[benchmark == "moralbench" & value > 1, benchmark := "moralbench_hum"]
    parts$openeval <- sc[, .(resp = mean(value), n_obs = .N),
                         by = .(benchmark, item = as.character(item_uid), model = paste0("oe_", model_uid))]
  }
  if (have[["eee"]]) {
    sc <- data.table::fread(sources$eee, select = c("eee_benchmark", "model_id", "sample_id", "is_correct"), showProgress = FALSE)
    parts$eee <- sc[, .(resp = mean(as.numeric(is_correct)), n_obs = .N),
                    by = .(benchmark = eee_benchmark, item = as.character(sample_id), model = model_id)]
  }
  if (have[["livebench"]]) {
    if (!requireNamespace("arrow", quietly = TRUE)) stop("Reading livebench.parquet needs the 'arrow' package.", call. = FALSE)
    sc <- data.table::as.data.table(arrow::read_parquet(sources$livebench, col_select = c("question_id", "model", "score")))
    parts$livebench <- sc[, .(resp = mean(score), n_obs = .N), by = .(item = question_id, model)][, benchmark := "livebench"][]
  }
  cells <- data.table::rbindlist(parts, idcol = "dataset", use.names = TRUE)
  cells <- cells[!is.na(resp)]
  # Disambiguate names shared across sources, and against the 18 aggdf benchmarks.
  agg_names <- c("air_bench_2024", "arc_challenge", "bbh", "bbq", "boolq", "commonsense", "gpqa", "ifeval",
                 "legalbench", "med_qa", "mmlu_pro", "mmluclinic", "mmlumath", "mmlupromed", "musr",
                 "openllm_math", "thai_exam", "wikifact")
  dup <- cells[, .(n = data.table::uniqueN(dataset)), by = benchmark][n > 1 | benchmark %in% agg_names]$benchmark
  cells[benchmark %in% dup, benchmark := paste0(benchmark, "_", dataset)]
  # Some EEE item ids are the full prompt text (with newlines and quotes). Give
  # those benchmarks short, deterministic ids so the table round-trips as CSV.
  bad_ids <- cells[, .(bad = any(nchar(item) > 64 | grepl("[\r\n\",]", item))), by = benchmark][bad == TRUE]$benchmark
  cells[benchmark %in% bad_ids, item := paste0("q", match(item, sort(unique(item)))), by = benchmark]
  const <- cells[, .(v = stats::var(resp)), by = benchmark][is.na(v) | v == 0]$benchmark
  if (length(const)) message("complete: dropping constant benchmark(s): ", paste(const, collapse = ", "))
  cells <- cells[!benchmark %in% const]
  if (!is.null(out)) write_result(cells, out)
  invisible(cells[])
}

#' Load the Complete benchmark set in the long format of [load_responses()]
#'
#' Row-binds the 18 aggdf benchmarks and the cells built by
#' [build_complete_cells()]. A `dataset` column records each benchmark's source.
#'
#' @param cells Path written by [build_complete_cells()].
#' @param aggdf Path of `aggdf.csv` (or `NULL` to leave those benchmarks out).
complete_responses <- function(cells = data_path("complete_cells.csv.gz"), aggdf = data_path("aggdf.csv")) {
  if (!file.exists(cells)) stop("Complete-set cells not found: ", cells,
                                "\nRun analysis/00_build_complete.R (see data/README.md).", call. = FALSE)
  # Declare every type: ids can look numeric in one source and not another.
  x <- data.table::fread(cells, showProgress = FALSE,
                         colClasses = list(character = c("dataset", "benchmark", "item", "model"), numeric = c("resp", "n_obs")))
  x[, n_obs := NULL]
  if (!is.null(aggdf)) x <- data.table::rbindlist(list(load_responses(aggdf)[, dataset := "aggdf"], x), use.names = TRUE)
  data.table::setcolorder(x, c("benchmark", "item", "model", "resp", "dataset"))
  data.table::setkeyv(x, c("benchmark", "model", "item"))
  x[]
}
