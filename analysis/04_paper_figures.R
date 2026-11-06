# Stage 4: the paper's figures, with their LaTeX captions.
#
#   Rscript analysis/04_paper_figures.R                      # every figure -> paper_figures/
#   Rscript analysis/04_paper_figures.R --only=panel,shift   # some families (see FAMILIES)
#   Rscript analysis/04_paper_figures.R --lines=raw          # raw item-by-item curves (files get _raw)
#   Rscript analysis/04_paper_figures.R --out=/path/to/dir --thresholds=9,4 --workers=4
#
# Inputs are cached in results/ and computed on first use if missing, which
# can be slow; `make paper-inputs` runs stages 1-3 for exactly these:
#   results/curves/<reference>__<measured>/  the three positive shifts (every criterion) and
#                                           all, top_half, top_n:10 in-sample (random order)
#   results/gstudy/appendix.csv              populations all, top_half, top_frac:0.1, top_n:10
#   results/gstudy/complete.csv              the Complete set (needs data/complete_cells.csv.gz)
#
# Output (git-ignored): <out>/<figure>.pdf; <out>/figures.tex, one captioned
# figure environment per figure, by paper section; <out>/manifest.csv; and
# <out>/preview.tex, a wrapper at the paper's text width for checking.
# Captions are written here; docs/FIGURES.md lists the figures.

source("analysis/_common.R")
suppressPackageStartupMessages(library(ggplot2))

flags <- parse_flags(defaults = list(out = "paper_figures", lines = "smooth", workers = 1, only = "all",
                                     thresholds = c(9, 4)))
OUT <- flags$out
THR <- as.numeric(flags$thresholds)
LINES <- flags$lines
SUFFIX <- if (LINES == "smooth") "" else paste0("_", LINES)
FAMILIES <- c("panel", "shift", "1a", "1c", "2", "3", "4", "5")
only <- if (identical(flags$only, "all")) FAMILIES else as.character(flags$only)
if (length(bad <- setdiff(only, FAMILIES))) stop("Unknown --only: ", paste(bad, collapse = ", "), call. = FALSE)
# The landscape needs the Complete set; without it, draw everything else.
if ("3" %in% only && !file.exists(results_path("gstudy", "complete.csv")) && !file.exists(data_path("complete_cells.csv.gz"))) {
  message("Skipping the reliability landscape (family 3): no Complete-set data; run `make complete-data` (data/README.md).")
  only <- setdiff(only, "3")
}
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# Benchmarks, populations and caption text
# ---------------------------------------------------------------------------

SETS <- list(
  main = list(name = "Main", b = BENCHMARK_SETS$main, ncol = 2, h = 4.7, h_dot = 2.4),
  appendix = list(name = "Appendix", b = BENCHMARK_SETS$appendix, ncol = 4, h = 7.9, h_sq = 8.1, h_dot = 6.6)
)

# In-sample populations (A), (B), (F).
POPS_ABF <- c(A = "all", B = "top_half", F = "top_n:10")
POP_TEXT <- c(all = "all models", top_half = "the top 50\\% of models", "top_n:10" = "the 10 highest-scoring models")
pops_text <- paste(sprintf("(%s) %s", names(POPS_ABF), POP_TEXT[POPS_ABF]), collapse = "; ")

# Positive capability shifts: items scored on lower-scoring models, outcomes
# measured on higher-scoring ones. Captions name them in words.
SHIFTS <- data.table(
  id = c("CtoB", "JtoE", "KtoF"),
  pair = c("bottom_half->top_half", "not_top_frac:0.1->top_frac:0.1", "not_top_n:10->top_n:10"),
  title = c("bottom 50\\% $\\to$ top 50\\%", "bottom 90\\% $\\to$ top 10\\%", "all but the top ten $\\to$ top ten"),
  ref = c("the bottom 50\\% of models", "the bottom 90\\% of models (all but the top 10\\%)",
          "all models except the 10 highest-scoring"),
  meas = c("the top 50\\%", "the top 10\\%", "those 10")
)
PANEL_SHIFTS <- c("CtoB", "KtoF")
PANEL_GROUPS <- list(full = METHOD_SETS$with_2pl, sel4 = c("lambda_star", "difficulty", "irt_a", "random"))
CRITERIA_TEXT <- "$\\lambda^{\\star}_6$, raw difficulty, 2PL discrimination, the corrected inter-item correlation, Loevinger's $H_i$ and random order"
OUTCOME_TEXT <- c(g_rel = "relative reliability $G(n)$", kendall = "Kendall's $\\tau$ with the full-benchmark ranking")

fmt3 <- function(x) sub("^0", "", sprintf("%.3f", x))
LT_TEXT <- c("solid", "dotted", "dashed", "dot-dashed")
thr_text <- function(scale = "g", lead = "Grey reference lines") {
  v <- snr_threshold_value(THR, scale)
  what <- if (scale == "tau") "$\\tau^{\\ast}=\\tfrac{2}{\\pi}\\arcsin\\sqrt{G^{\\ast}}$, the Kendall $\\tau$ an $n$-item subset reaches with the full benchmark when $G(n)=G^{\\ast}$" else "$G^{\\ast}=\\mathrm{SNR}/(1+\\mathrm{SNR})$"
  sprintf("%s: %s at %s.", lead, paste(sprintf("$\\mathrm{SNR}=%g$ (%s, %s)", THR, LT_TEXT[seq_along(THR)], fmt3(v)), collapse = "; "), what)
}
lines_text <- function() switch(LINES,
  smooth = "Lines are \\texttt{geom\\_smooth} fits at ggplot2's defaults (LOESS for curves under 1{,}000 points, otherwise a GAM) on the $\\log_{10} n$ scale, with standard-error bands.",
  raw = "Lines show the value at every $n$; Random also has a 95\\% band over its 10 orders.",
  both = "Thin lines show the value at every $n$; thick lines are \\texttt{geom\\_smooth} fits at ggplot2's defaults on the $\\log_{10} n$ scale, with standard-error bands.")
paste_and <- function(x) if (length(x) <= 1) x else paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)])
# Sentence naming the benchmarks where a population (or a pair's measured
# population) is too small to use; "" when it is used everywhere.
omitted_text <- function(pop, benches, what) {
  bad <- benches[!population_eligible(scores, benches, pop)]
  if (!length(bad)) return("")
  sprintf(" On %s, %s has 5 or fewer models and is not shown.", paste_and(latex_escape(benchmark_label(bad))), what)
}
# Figure height when only some of a set's benchmarks have panels.
fit_height <- function(S, nb) {
  rows_full <- ceiling(length(S$b) / S$ncol); rows <- ceiling(nb / S$ncol)
  if (rows >= rows_full) S$h else 1.25 + (S$h - 1.25) * rows / rows_full
}

# ---------------------------------------------------------------------------
# Data (cached in results/)
# ---------------------------------------------------------------------------

long <- load_responses(benchmarks = "appendix")
scores <- model_scores(long, dates = upload_dates())
.curves <- new.env()
curves_for <- function(pair, methods = METHOD_SETS$with_2pl) {
  key <- paste(pair, paste(methods, collapse = "+"))
  if (!exists(key, envir = .curves)) {
    pr <- parse_pair(pair)
    assign(key, suppressMessages(cached_curves(long, scores, "appendix", pr$reference, pr$measured,
                                               methods = methods, dir = RESULTS)), envir = .curves)
  }
  get(key, envir = .curves)
}
random_on <- function(pop) curves_for(paste0(pop, "->", pop), "random")
.gst <- NULL
gst_app <- function() {
  if (is.null(.gst)) .gst <<- cached_gstudies(long, scores, "appendix", c("all", "top_half", "top_frac:0.1", "top_n:10"),
                                              name = "appendix", dir = RESULTS)
  .gst
}
.complete <- NULL
complete_landscape <- function() {
  if (is.null(.complete)) {
    pops <- c("all", "top_half", "top_frac:0.1", "top_n:10")
    path <- results_path("gstudy", "complete.csv")
    g <- if (file.exists(path)) fread(path) else {
      cl <- complete_responses()
      cached_gstudies(cl, model_scores(cl), NULL, pops, name = "complete", missing = "none", dir = RESULTS,
                      workers = flags$workers)
    }
    .complete <<- list(g = g[population %in% pops], pops = pops)
  }
  .complete
}

# ---------------------------------------------------------------------------
# Figures: each returns list(p, height, caption)
# ---------------------------------------------------------------------------

FIGS <- list()
fig <- function(id, family, section, group, make) {
  FIGS[[length(FIGS) + 1]] <<- list(id = id, family = family, section = section, group = group, make = make)
}

# Main text, positive shift: Main benchmarks as columns, two shifts as rows.
# Vertical space is scarce, so the legend sits on the right in one column.
panel_theme <- function() list(
  ggplot2::scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")),
  ggplot2::labs(y = "Relative\nreliability G(n)"),
  ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 270, size = 8, lineheight = 0.85),
                 strip.text.x = ggplot2::element_text(size = 8.5),
                 axis.title = ggplot2::element_text(size = 9, lineheight = 0.9),
                 legend.position = "right", legend.title.position = "top",
                 legend.title = ggplot2::element_text(size = 8.5),
                 legend.text = ggplot2::element_text(size = 8, lineheight = 0.85),
                 legend.key.width = ggplot2::unit(1.3, "lines"), legend.key.height = ggplot2::unit(0.75, "lines"),
                 legend.key.spacing.y = ggplot2::unit(0.5, "pt"), legend.box.spacing = ggplot2::unit(3, "pt"),
                 panel.spacing = ggplot2::unit(3, "pt"), plot.margin = ggplot2::margin(2, 3, 1, 2)),
  ggplot2::guides(colour = ggplot2::guide_legend(ncol = 1), fill = ggplot2::guide_legend(ncol = 1),
                  linetype = ggplot2::guide_legend(ncol = 1)))
for (g in names(PANEL_GROUPS)) local({
  g <- g
  fig(sprintf("fig1bshift_main_panel_%s_grel", g), "panel", "body", "(1b) Positive shift: main-text panel", function() {
    sh <- SHIFTS[match(PANEL_SHIFTS, id)]
    pairs <- vapply(sh$pair, function(pp) { x <- parse_pair(pp); paste0(x$reference, "->", x$measured) }, "", USE.NAMES = FALSE)
    cv <- rbindlist(lapply(sh$pair, function(pp) curves_for(pp)[benchmark %in% SETS$main$b]), fill = TRUE)
    p <- plot_selection_curves(cv, "g_rel", PANEL_GROUPS[[g]], THR, LINES, pair_rows = TRUE, pairs = pairs,
                               mark_frac = 0.1, legend_wrap = TRUE) + panel_theme()
    list(p = p, height = 1.92, caption = sprintf(
      "\\textbf{Item selection under positive capability shift (Main benchmarks).} Relative reliability $G(n)$ of the first $n$ items chosen by each criterion on lower-scoring models, measured on higher-scoring ones: bottom 50\\%% $\\to$ top 50\\%% (top row) and all but the top ten $\\to$ top ten (bottom row). Random: mean of 10 orders. Dotted vertical lines: 10\\%% of items (Table~\\ref{tab:shift-selection}); grey horizontal lines: %s. Lines smoothed, with standard-error bands.",
      paste(sprintf("$\\mathrm{SNR}=%g$ (%s)", THR, LT_TEXT[seq_along(THR)]), collapse = " and ")))
  })
})

# Appendix, positive shift: every criterion on the 18 Appendix benchmarks, one figure per shift and outcome.
for (k in seq_len(nrow(SHIFTS))) for (y in c("g_rel", "kendall")) local({
  k <- k; y <- y
  sh <- SHIFTS[k]
  fig(sprintf("fig1bshift_appendix_%s_full_%s", sh$id, gsub("_", "", y)), "shift", "appendix",
      "(1b) Positive shift: every criterion, one shift", function() {
    S <- SETS$appendix
    cv <- curves_for(sh$pair)[benchmark %in% S$b]
    p <- plot_selection_curves(cv, y, PANEL_GROUPS$full, THR, LINES, ncol = S$ncol)
    how <- if (y == "g_rel")
      sprintf("Items are scored on %s, and $G(n)$ is measured on %s: every point re-estimates the variance components of the selected items in the measured models.", sh$ref, sh$meas)
    else sprintf("Items are scored on %s, and $\\tau$ is computed on %s, between their scores on the selected items and on the full benchmark.", sh$ref, sh$meas)
    list(p = p, height = fit_height(S, uniqueN(cv$benchmark)), caption = sprintf(
      "\\textbf{Item selection under positive capability shift, %s (Appendix benchmarks): %s as items are added.} %s Each criterion (%s) adds items in its own order; Random is the mean of 10 random orders at each $n$. %s %s%s",
      sh$title, OUTCOME_TEXT[[y]], how, CRITERIA_TEXT, lines_text(), thr_text(if (y == "kendall") "tau" else "g"),
      omitted_text(sh$pair, S$b, sh$meas)))
  })
})

# Appendix: relative and absolute reliability under random item order, populations (A), (B), (F) together.
fig("fig1a_appendix_popsABF", "1a", "appendix", "(1a) Reliability contrasts", function() {
  S <- SETS$appendix
  cv <- rbindlist(lapply(POPS_ABF, random_on), fill = TRUE)[benchmark %in% S$b]
  p <- plot_reliability_contrast(cv, unname(POPS_ABF), "random", c("g_rel", "g_abs"), THR, LINES, ncol = S$ncol)
  list(p = p, height = S$h, caption = sprintf(
    "\\textbf{Relative and absolute reliability vs.\\ number of items by population, Appendix benchmarks.} Empirical relative ($G(n)$, solid) and absolute ($\\Phi(n)$, dashed) reliability of the first $n$ items of a random order, re-estimated at every $n$ within each population (colours) and averaged over 10 orders: %s. %s %s%s",
    pops_text, lines_text(), thr_text("g"),
    paste(vapply(POPS_ABF, function(p) omitted_text(p, S$b, POP_TEXT[[p]]), ""), collapse = "")))
})

# Main text: G-theory projection for random item draws.
fig("fig1c_main_popsABF", "1c", "body", "(1c) Theoretical random draws", function() {
  g <- gst_app()[benchmark %in% SETS$main$b & population %in% POPS_ABF]
  p <- plot_dstudy(g, "population", thresholds = THR, ncol = SETS$main$ncol)
  list(p = p, height = SETS$main$h, caption = sprintf(
    "\\textbf{G-theory projection for random item draws, Main benchmarks.} Relative ($G(n)$, solid) and absolute ($\\Phi(n)$, dashed) reliability of $n$ items drawn at random from the benchmark's task universe, from each population's full-benchmark variance components: %s. %s",
    pops_text, thr_text("g")))
})

# Appendix: SNR-R2_tau path by population.
fig("fig2_appendix_popsABF", "2", "appendix", "(2) SNR--$R^2_\\tau$ paths by population", function() {
  S <- SETS$appendix
  cv <- rbindlist(lapply(POPS_ABF, random_on), fill = TRUE)[benchmark %in% S$b]
  p <- plot_snr_r2_path(cv, "measured", "random", ncol = S$ncol, populations = unname(POPS_ABF))
  list(p = p, height = S$h_sq, caption = sprintf(
    "\\textbf{SNR--$R^2_\\tau$ mapping under random item draws, Appendix benchmarks: populations (A), (B), (F).} Each point is one number of items $n$ (more opaque as $n$ grows): $x$ is the empirical relative reliability $G(n)$ of the first $n$ items of a random order, and $y$ is $R^2_\\tau=\\sin^2(\\pi\\tau/2)$, with $\\tau$ Kendall's $\\tau$ between the models' $n$-item scores and their full-benchmark scores (Greiner's identity). Both are means over 10 orders, measured within each population (%s). The dashed line is $x=y$, the theoretical relationship (App.~C). Because the subset is part of the full benchmark, the expected value for a benchmark of $N$ items is about $R^2_\\tau=G(n)/G(N)$, slightly above the line when $G(N)<1$. Larger departures diagnose rank instability that the variance decomposition does not predict.%s",
    pops_text, paste(vapply(POPS_ABF, function(p) omitted_text(p, S$b, POP_TEXT[[p]]), ""), collapse = "")))
})

# Reliability landscape of the Complete set: SNR 9 in the main text, SNR 100 in the supplement.
for (s in c(9, 100)) local({
  s <- s
  fig(sprintf("fig3_complete_snr%g", s), "3", if (s == 9) "body" else "supplementary", "(3) Reliability landscape", function() {
    cl <- complete_landscape()
    p <- plot_reliability_landscape(cl$g, s, c(1, 10, 1000), cl$pops)
    counts <- cl$g[eligible == TRUE & is.finite(prop_model), .N, by = population]
    list(p = p, height = 7.6, caption = sprintf(
      "\\textbf{Reliability landscape of the Complete benchmark set at $\\mathrm{SNR}\\geq %g$.} Each point is one benchmark, for the population in its row: %s. The $x$ axis is the share of score variance that is residual ($\\sigma^2_R/V$) and the $y$ axis the share due to models ($\\sigma^2_M/V$), both on square-root scales; the item share is the remainder. Columns give the number of new, comparable tasks $n$ a claim covers. Green triangles: the benchmark meets $\\mathrm{SNR}\\geq %g$ for absolute reliability at $n$ tasks, $y\\geq %g/(n+%g)$ (solid line), and therefore also for relative reliability; blue diamonds: relative only, $y\\geq %g\\,x/n$ (dashed line); red open triangles: neither. Variance components are exact REML for complete matrices and \\texttt{lmer} on the observed cells otherwise; replicates and prompt variants are averaged within each model--item cell. Populations with 5 or fewer models, or above half of the benchmark's models, are omitted.",
      s, paste(sprintf("%s (%d benchmarks)", latex_escape(population_label(counts$population)), counts$N), collapse = ", "),
      s, s, s, s))
  })
})

# Full-benchmark relative vs absolute reliability by population: Main (main text) and Appendix benchmarks.
for (set in c("main", "appendix")) local({
  set <- set
  fig(sprintf("fig4_%s_popsABF", set), "4", if (set == "main") "body" else "appendix",
      "(4) Relative--absolute population contrasts", function() {
    S <- SETS[[set]]
    p <- plot_population_dots(gst_app()[benchmark %in% S$b], unname(POPS_ABF), thresholds = THR)
    list(p = p, height = S$h_dot, caption = sprintf(
      "\\textbf{Full-benchmark reliability by model population, %s benchmarks.} Relative ($G$, blue, solid) and absolute ($\\Phi$, orange, dashed) reliability of each benchmark at its own length, estimated separately in each population (shapes): %s. Segments span the populations for each reliability type. %s%s",
      S$name, pops_text, thr_text("g", "Grey vertical lines"),
      paste(vapply(POPS_ABF, function(p) omitted_text(p, S$b, POP_TEXT[[p]]), ""), collapse = "")))
  })
})

# Main text: the score distributions behind the bottom 50% -> top 50% shift.
fig("fig5_main_CtoB", "5", "body", "(5) Population distributions", function() {
  p <- plot_population_histograms(scores, SETS$main$b, "bottom_half->top_half", ncol = SETS$main$ncol)
  list(p = p, height = 4.2, caption = "\\textbf{Distribution of model scores by population, Main benchmarks.} Mean score of each model on each benchmark, coloured by membership: reference = the bottom 50\\% of models; measured = the top 50\\% of models.")
})

ALL_IDS <- vapply(FIGS, `[[`, "", "id")
FIGS <- Filter(function(f) f$family %in% only, FIGS)

# ---------------------------------------------------------------------------
# Render: PDFs, manifest, LaTeX
# ---------------------------------------------------------------------------

# Load shared inputs in the parent so forked workers inherit them.
fams <- unique(vapply(FIGS, `[[`, "", "family"))
if (any(c("panel", "shift") %in% fams)) for (pp in SHIFTS$pair) curves_for(pp)
if (any(c("1a", "2") %in% fams)) for (pp in POPS_ABF) random_on(pp)
if (any(c("1c", "4") %in% fams)) invisible(gst_app())
if ("3" %in% fams) invisible(complete_landscape())
invisible(loadNamespace("mgcv"))

render <- function(f) {
  r <- f$make()
  file <- paste0(f$id, SUFFIX, ".pdf")
  save_figure(r$p, file.path(OUT, file), height = r$height)
  data.table(id = f$id, family = f$family, section = f$section, group = f$group, file = file,
             label = paste0("fig:", gsub("_", "-", f$id), SUFFIX), width_in = PAPER_WIDTH, height_in = r$height,
             caption = r$caption)
}
log_msg("rendering %d figures with %d worker(s)", length(FIGS), flags$workers)
t0 <- proc.time()[["elapsed"]]
rows <- par_map(FIGS, function(f) suppressMessages(render(f)), workers = flags$workers)
bad <- which(vapply(rows, function(r) inherits(r, "try-error") || is.null(r), NA))
for (i in bad) message("failed: ", FIGS[[i]]$id, ": ", as.character(rows[[i]]))
man <- rbindlist(rows[setdiff(seq_along(rows), bad)])
log_msg("rendered %d figures in %.0fs", nrow(man), proc.time()[["elapsed"]] - t0)

# With --only, keep the other figures already in the manifest. Figures keep
# the order above within each section.
mpath <- file.path(OUT, paste0("manifest", SUFFIX, ".csv"))
if (!identical(flags$only, "all") && file.exists(mpath)) {
  old <- fread(mpath)
  man <- rbind(man, old[!id %in% man$id & id %in% ALL_IDS], fill = TRUE)
}
man <- man[order(match(section, c("body", "appendix", "supplementary")), match(id, ALL_IDS))]
fwrite(man, mpath)
tex <- file.path(OUT, paste0("figures", SUFFIX, ".tex"))
write_latex_figures(man, tex, header = c(
  sprintf("Generated by analysis/04_paper_figures.R on %s (%d figures). Edit captions in that script.", format(Sys.Date()), nrow(man)),
  "Needs \\usepackage{graphicx,amsmath}; graphics paths are relative to this file (set \\graphicspath if it lives elsewhere).",
  "manifest.csv lists each figure's section, size and caption."))
writeLines(c("\\documentclass[10pt]{article}",
             "\\usepackage[letterpaper,textwidth=5.5in,textheight=9in]{geometry}",
             "\\usepackage{graphicx,amsmath,amssymb,mathptmx}",
             "\\begin{document}", sprintf("\\input{figures%s.tex}", SUFFIX), "\\end{document}"),
           file.path(OUT, paste0("preview", SUFFIX, ".tex")))
log_msg("wrote %s and %s", tex, mpath)
if (length(bad)) stop(length(bad), " figure(s) failed.", call. = FALSE)
