# ==============================================================================
# 00_setup.R -- packages, paths, options, and the shared plotting theme.
#
# Every script in scripts/ begins by sourcing R/load_all.R, which sources this
# file first. Nothing here is specific to a single analysis: if a setting only
# matters to one stage of the pipeline, it belongs in that stage's script.
# ==============================================================================

# --- Packages -----------------------------------------------------------------
# Declared once, here. The original scripts each carried their own 30-40 line
# library() preamble, with several packages loaded two to four times and some
# used functions (tictoc, collapse verbs, mokken) never loaded at all. The
# authoritative dependency list is DESCRIPTION; this block loads the subset
# needed by nearly every stage.
suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(readr)
  library(ggplot2)
  library(glue)
  library(here)
  library(janitor)
  library(collapse)   # fmean / fgroup_by / fsummarise, used in the hot paths
  library(tictoc)
})

# `collapse` masks a few dplyr generics. Resolve the clashes explicitly rather
# than relying on load order, which is what made the original scripts sensitive
# to the sequence of their library() calls.
fsubset   <- collapse::fsubset
fmutate   <- collapse::fmutate
fsummarise <- collapse::fsummarise
fgroup_by <- collapse::fgroup_by
fungroup  <- collapse::fungroup
n_unique  <- collapse::fnunique

# --- Paths --------------------------------------------------------------------
# All paths are derived from the project root, so the pipeline runs from any
# working directory and on any machine. The original code mixed
# here("benchmark_reliability/data/...") with bare here("data/...") and with
# absolute paths under /Users/mhardy/, which is why several scripts only ran on
# one laptop from one working directory.
BR_ROOT    <- here::here()
BR_DATA    <- file.path(BR_ROOT, "data")
BR_RESULTS <- file.path(BR_DATA, "results")
BR_BOOTS   <- file.path(BR_RESULTS, "boots")
BR_FIGURES <- file.path(BR_ROOT, "figures")

br_path <- function(...) file.path(BR_ROOT, ...)

#' Ensure the output directories exist
#'
#' Called by every script that writes results, so a fresh clone can run the
#' pipeline without anyone creating directories by hand.
br_init_dirs <- function() {
  for (d in c(BR_DATA, BR_RESULTS, BR_BOOTS, BR_FIGURES)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(TRUE)
}

# --- Options ------------------------------------------------------------------
options(
  dplyr.summarise.inform = FALSE,
  datatable.verbose      = FALSE,
  readr.show_col_types   = FALSE
)

#' Configure the parallel backend
#'
#' The original scripts called `future::plan()` at the top level more than once
#' with conflicting strategies (`multisession` at load time, then `multicore`
#' a few hundred lines later), so the backend actually in force depended on how
#' far down the file you had evaluated. Here it is one explicit call.
#'
#' `multicore` (forked workers) is faster and shares memory, but is unsafe in
#' RStudio and unavailable on Windows, so we fall back to `multisession`.
#'
#' @param workers Number of parallel workers. Defaults to one fewer than the
#'   available cores, leaving a core for the OS.
#' @param strategy "auto", "multicore", "multisession", or "sequential".
br_plan <- function(workers = NULL, strategy = "auto") {
  if (is.null(workers)) workers <- max(1L, future::availableCores(omit = 1L))
  if (identical(strategy, "auto")) {
    strategy <- if (future::supportsMulticore() && !interactive()) {
      "multicore"
    } else {
      "multisession"
    }
  }
  if (workers <= 1L) strategy <- "sequential"
  # A sequential plan has exactly one worker regardless of what was requested,
  # so report that rather than the requested count.
  if (identical(strategy, "sequential")) workers <- 1L
  message(glue("Parallel backend: {strategy} with {workers} worker(s)."))
  switch(strategy,
    multicore    = future::plan(future::multicore, workers = workers),
    multisession = future::plan(future::multisession, workers = workers),
    sequential   = future::plan(future::sequential),
    stop("Unknown strategy: ", strategy)
  )
  invisible(future::plan())
}

# --- Plotting -----------------------------------------------------------------
# The paper's figures use a serif face at ~14pt. This was repeated as an inline
# `theme(text = element_text(family = "times", size = 14), ...)` block in nearly
# every plot in plot_all_code.R, along with a stack of four mutually overriding
# ggthemes scale_*_ calls per plot (only the last of which took effect).

# "Times" is not present on every system; fall back to the generic serif family
# rather than emitting a font warning per panel, as the original code did.
BR_FONT <- if ("Times" %in% names(grDevices::postscriptFonts())) "Times" else "serif"

#' Shared theme for every figure in the paper
#'
#' @param base_size Base font size in points.
#' @param legend Legend position passed to `theme()`.
theme_br <- function(base_size = 14, legend = "right") {
  theme_minimal(base_size = base_size) +
    theme(
      text           = element_text(family = BR_FONT),
      legend.position = legend,
      legend.title   = element_blank(),
      legend.text    = element_text(size = base_size - 2),
      legend.spacing.x = unit(0.001, "cm"),
      legend.spacing.y = unit(0.001, "cm"),
      legend.margin  = margin(0.001, 0.001, 0.001, 0.001, unit = "cm"),
      panel.grid.minor = element_blank(),
      strip.text     = element_text(face = "bold")
    )
}

#' Colourblind-safe discrete scales
#'
#' One definition, used for both colour and fill, replacing the stacked
#' `scale_color_wsj()` / `scale_color_economist()` / `scale_color_colorblind()`
#' calls that appeared together in the original plots.
scale_br_colour <- function(...) ggthemes::scale_colour_colorblind(...)
scale_br_fill   <- function(...) ggthemes::scale_fill_colorblind(...)

#' Save a figure at the size used in the paper
#'
#' @param plot A ggplot (or cowplot) object.
#' @param name File name stem; written to figures/ as both PDF and PNG.
#' @param width,height Inches.
br_save_figure <- function(plot, name, width = 12, height = 7) {
  br_init_dirs()
  for (dev in c("pdf", "png")) {
    ggplot2::ggsave(
      filename = file.path(BR_FIGURES, paste0(name, ".", dev)),
      plot = plot, width = width, height = height, dpi = 300
    )
  }
  invisible(file.path(BR_FIGURES, name))
}
