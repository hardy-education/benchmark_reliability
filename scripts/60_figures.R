#!/usr/bin/env Rscript
# ==============================================================================
# Stage 60 -- build the paper's figures.
#
#   Fig. 1  properties of reliability under distribution shift (3 panels)
#   Fig. 2  estimated model-ranking reliability vs. number of items
#   Fig. 3  item selection at fixed evaluation budget (3 panels)
#   Fig. 4  lambda*_6 against classical Guttman lambda_6
#   Tab. 1  generalizability coefficients across model populations
#
# Each figure is composed from the panel builders in R/07_plots.R and written to
# figures/ as both PDF and PNG.
#
# Usage:
#   Rscript scripts/60_figures.R
#   Rscript scripts/60_figures.R --figures=1,3
# ==============================================================================

source(here::here("R", "load_all.R"))
suppressPackageStartupMessages(library(cowplot))

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (!length(hit)) default else sub(paste0("^--", name, "="), "", hit[1])
}
WANTED <- {
  v <- strsplit(opt("figures", ""), ",")[[1]]
  if (!length(v)) c("1", "2", "3", "4", "table1") else v
}
want <- function(x) x %in% WANTED

br_init_dirs()

# --- Inputs -------------------------------------------------------------------
latest <- function(pattern, dir = BR_BOOTS) {
  f <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (!length(f)) stop(glue("No file matching '{pattern}' in {dir}."))
  sort(f, decreasing = TRUE)[1]
}

responses <- load_responses()
splits    <- make_hilo_splits(responses)

model_counts <- splits |>
  dplyr::group_by(benchmark) |>
  dplyr::summarise(
    ntrain = sum(split == "train"),
    ntest  = sum(split == "test"),
    .groups = "drop"
  )

rank_stability <- readRDS(latest("^rank_stability_.*\\.rds$"))
dstudy         <- read_dstudy_results("^gdstudy_n[0-9]+_.*\\.rds$")

frame <- build_reliability_frame(dstudy, rank_stability, model_counts)

# --- Figure 1 -----------------------------------------------------------------
if (want("1")) {
  message("Figure 1: reliability under distribution shift")
  fig1 <- cowplot::plot_grid(
    plot_score_distributions(splits) + theme(legend.position = "none"),
    plot_tau2_vs_snr(frame) + theme(legend.position = "none"),
    plot_rank_reliability_by_population(rank_stability),
    nrow = 3, rel_heights = c(1, 1, 1.3),
    labels = c("A", "B", "C"),
    label_fontfamily = BR_FONT, label_size = 16
  )
  br_save_figure(fig1, "fig1_distribution_shift", width = 14, height = 11)
}

# --- Figure 2 -----------------------------------------------------------------
if (want("2")) {
  message("Figure 2: reliability vs. number of items")
  # Full-benchmark variance components, then the D-study curve of Eq. (3). The
  # curve is truncated per benchmark at its own item count, which is why some
  # lines stop earlier than others.
  components <- purrr::map(unique(responses$benchmark), function(b) {
    fit_gstudy(responses |> dplyr::filter(benchmark == b) |> tidyr::drop_na())
  }) |>
    purrr::list_rbind() |>
    dplyr::left_join(
      # Renamed on the way in: `dstudy_project()` creates its own `n_items`
      # column for the projected design size, and letting the two share a name
      # leaves the truncation below depending on a `.x`/`.y` join suffix.
      benchmark_summary(responses) |>
        dplyr::select(benchmark, items_available = n_items),
      by = "benchmark"
    )

  grid <- c(1:20, 25, 30, 40, 50, 75, 100, 125, 250, 500, 750, 1000,
            2500, 5000, 12000)
  # A benchmark's curve stops at its own item count -- you cannot project a
  # design onto more items than the benchmark has.
  curve <- dstudy_curve(components, grid) |>
    dplyr::filter(is.na(items_available) | n_items <= items_available)

  br_save_figure(plot_reliability_vs_items(curve),
                 "fig2_reliability_vs_items", width = 10, height = 6)
}

# --- Figure 3 -----------------------------------------------------------------
if (want("3")) {
  message("Figure 3: item selection at fixed budget")
  fig3 <- cowplot::plot_grid(
    plot_selection_panel(frame, "m_dep_D", expression(E * rho^2)) +
      theme(legend.position = "none"),
    plot_selection_panel(frame, "m_dep_D_snr", expression(SNR(Phi))) +
      theme(legend.position = "none"),
    plot_selection_panel(frame, "kcor", expression(paste("Kendall's ", tau))),
    nrow = 3, rel_heights = c(1, 1, 1.3),
    labels = c("A", "B", "C"),
    label_fontfamily = BR_FONT, label_size = 16
  )
  br_save_figure(fig3, "fig3_item_selection", width = 14, height = 11)
}

# --- Figure 4 -----------------------------------------------------------------
if (want("4")) {
  message("Figure 4: lambda*_6 vs classical lambda_6")
  # lambda*_6 comes from the stage-20 aggregate; classical lambda_6 is computed
  # here on the same item sets, so the comparison is like for like.
  item_metrics <- readRDS(latest("^item_criteria_.*\\.rds$"))

  comparison <- purrr::map(unique(item_metrics$benchmark), function(b) {
    wide <- sample_response_matrix(splits, responses, benchmark = b,
                                  n_items = 200, population = "train")
    classical <- suppressWarnings(guttman_lambda6(wide))
    tibble::tibble(
      benchmark   = b,
      lambda_star = mean(item_metrics$L6[item_metrics$benchmark == b], na.rm = TRUE),
      lambda6     = classical$lambda6,
      n_items     = ncol(wide) - 1L,
      n_models    = nrow(wide)
    )
  }) |>
    purrr::list_rbind()

  readr::write_csv(comparison, file.path(BR_RESULTS, "lambda_comparison.csv"))
  if (requireNamespace("ggrepel", quietly = TRUE)) {
    br_save_figure(plot_lambda_comparison(comparison),
                   "fig4_lambda_comparison", width = 9, height = 7)
  } else {
    message("ggrepel not installed; wrote lambda_comparison.csv only.")
  }
}

# --- Table 1 ------------------------------------------------------------------
if (want("table1")) {
  message("Table 1: G-coefficients across model populations")
  # Reliability estimated separately on the lower-performing half, the
  # higher-performing half, and a random half of the same size -- the
  # population-dependence result of Sec. 3.2.
  populations <- list(train = "train", test = "test")

  per_population <- purrr::imap(populations, function(split_label, name) {
    keep <- splits |> dplyr::filter(split == split_label) |>
      dplyr::select(benchmark, model)
    purrr::map(unique(responses$benchmark), function(b) {
      dat <- responses |>
        dplyr::filter(benchmark == b) |>
        dplyr::semi_join(keep, by = c("benchmark", "model")) |>
        tidyr::drop_na()
      fit_gstudy(dat) |> dplyr::mutate(sample = name)
    }) |> purrr::list_rbind()
  }) |> purrr::list_rbind()

  rand_half <- make_random_split(splits, prop = 0.5)
  per_random <- purrr::map(unique(responses$benchmark), function(b) {
    dat <- responses |>
      dplyr::filter(benchmark == b) |>
      dplyr::semi_join(rand_half |> dplyr::select(benchmark, model),
                       by = c("benchmark", "model")) |>
      tidyr::drop_na()
    fit_gstudy(dat) |> dplyr::mutate(sample = "rand")
  }) |> purrr::list_rbind()

  table1 <- dplyr::bind_rows(per_population, per_random) |>
    dplyr::select(benchmark, sample, m_rel, m_dep, m_snr) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, 3)))

  out <- file.path(BR_RESULTS, "table1_population_gcoefficients.csv")
  readr::write_csv(table1, out)
  message(glue("Wrote {out}"))
  print(table1 |> tidyr::pivot_wider(names_from = sample, values_from = m_rel,
                                     id_cols = benchmark))
}

message("Stage 60 complete. Figures in ", BR_FIGURES)
