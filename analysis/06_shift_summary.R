# Stage 6: how each item-selection criterion performs under positive capability
# shift, at a budget of 10% of each benchmark's items.
#
#   Rscript analysis/06_shift_summary.R
#   Rscript analysis/06_shift_summary.R --frac=0.05 --out=paper_figures/tables
#
# Shifts (reference -> measured; items are scored on the reference models and
# every outcome is measured on the measured models):
#   bottom 50%   -> top 50%
#   bottom 90%   -> top 10%     (benchmarks with > 5 models in the top 10%)
#   bottom N-10  -> top n = 10
#
# For each benchmark x shift x outcome (G, Phi, Kendall tau with the full
# benchmark) the value of the first n10 = round(frac * N) selected items is
# compared with random order (mean of the 10 seeded orders) at the same n10.
# A criterion whose first n10 items are answered identically by every measured
# model has no regular REML fit ("no fit"); it counts as not beating random and
# as a material loss.
#
# Needs stage 2 curves for the three pairs (make paper-inputs).
# Writes <out>/shift_summary_cells.csv, shift_summary_pooled.csv,
# shift_summary_by_shift.csv and LaTeX tables shift_table_main.tex,
# shift_table_by_shift.tex, shift_table_levels.tex.

source("analysis/_common.R")
flags <- parse_flags(defaults = list(frac = 0.1, loss = 0.1, out = "paper_figures/tables"))
dir.create(flags$out, recursive = TRUE, showWarnings = FALSE)

SHIFTS <- data.table(code = c("C-B", "J-E", "K-F"),
                     pair = c("bottom_half->top_half", "not_top_frac:0.1->top_frac:0.1", "not_top_n:10->top_n:10"),
                     tex = c("Bottom 50\\% $\\to$ top 50\\%", "Bottom 90\\% $\\to$ top 10\\%", "All but top ten $\\to$ top ten"),
                     words = c("bottom 50\\% $\\to$ top 50\\%", "bottom 90\\% $\\to$ top 10\\%", "all but the top ten $\\to$ top ten"))
CRIT <- c("lambda_star", "difficulty", "irt_a", "ciic", "loevinger_hi")
CRIT_TEX <- c(lambda_star = "$\\lambda^{\\star}_6$", difficulty = "Raw difficulty", irt_a = "2PL discrimination",
              ciic = "Inter-item corr.", loevinger_hi = "Loevinger's $H_i$", random = "Random")
OUT_TEX <- c(g_rel = "$G$", g_abs = "$\\Phi$", kendall = "$\\tau$")

long <- load_responses(benchmarks = "appendix")
scores <- model_scores(long)

cells <- rbindlist(lapply(seq_len(nrow(SHIFTS)), function(i) {
  pr <- parse_pair(SHIFTS$pair[i])
  cv <- suppressMessages(cached_curves(long, scores, "appendix", pr$reference, pr$measured,
                                       methods = METHOD_SETS$with_2pl, dir = RESULTS))
  rbindlist(lapply(unique(cv$benchmark), function(b) {
    x <- cv[benchmark == b]
    N <- max(x$n_items); n10 <- max(2L, as.integer(round(flags$frac * N)))
    x[n_items == n10, .(g_rel = mean(g_rel, na.rm = TRUE), g_abs = mean(g_abs, na.rm = TRUE),
                        kendall = mean(kendall, na.rm = TRUE), status = status[1]), by = method][
      , `:=`(shift = SHIFTS$code[i], benchmark = b, N = N, n10 = n10)]
  }))
}))
for (v in c("g_rel", "g_abs", "kendall")) cells[is.nan(get(v)), (v) := NA_real_]
m <- melt(cells, id.vars = c("shift", "benchmark", "method", "N", "n10", "status"),
          measure.vars = c("g_rel", "g_abs", "kendall"), variable.name = "outcome", variable.factor = FALSE)
rnd <- m[method == "random", .(shift, benchmark, outcome, random = value)]
d <- merge(m[method != "random"], rnd, by = c("shift", "benchmark", "outcome"))
d[, `:=`(delta = value - random, no_fit = is.na(value))]
d[, beats := !no_fit & !is.na(delta) & delta > 0]
d[, loss := no_fit | (!is.na(delta) & delta < -flags$loss)]
fwrite(d, file.path(flags$out, "shift_summary_cells.csv"))

# Per benchmark x shift: gains on all three outcomes; no material loss on any.
bs <- d[, .(all3 = all(beats), noloss = !any(loss)), by = .(method, shift, benchmark)]
n_bs <- uniqueN(bs[, .(shift, benchmark)])

pooled <- d[, .(beats = sum(beats), n = .N, med_delta = median(delta, na.rm = TRUE),
                q10_delta = stats::quantile(delta, 0.1, na.rm = TRUE, names = FALSE),
                no_fit = sum(no_fit)), by = .(method, outcome)]
pooled_bs <- bs[, .(all3 = sum(all3), noloss = sum(noloss), n_bs = .N), by = method]
pooled_any <- d[, .(no_fit_cells = sum(no_fit), n_cells = .N, sd_delta = stats::sd(delta, na.rm = TRUE),
                    q10_all = stats::quantile(delta, 0.1, na.rm = TRUE, names = FALSE)), by = method]
fwrite(merge(merge(dcast(pooled, method ~ outcome, value.var = c("beats", "med_delta", "q10_delta")), pooled_bs, by = "method"),
             pooled_any, by = "method"), file.path(flags$out, "shift_summary_pooled.csv"))
by_shift <- d[, .(beats = sum(beats), n = .N, med_delta = median(delta, na.rm = TRUE), no_fit = sum(no_fit)),
              by = .(method, shift, outcome)]
fwrite(by_shift, file.path(flags$out, "shift_summary_by_shift.csv"))
levels <- m[, .(median = median(value, na.rm = TRUE)), by = .(method, shift, outcome)]
fwrite(levels, file.path(flags$out, "shift_summary_levels.csv"))

# ---- LaTeX tables -----------------------------------------------------------
f2 <- function(x) ifelse(is.na(x), "--", sub("^(-?)0\\.", "\\1.", sprintf("%.2f", x)))
sgn <- function(x) ifelse(is.na(x), "--", ifelse(round(x, 2) == 0, ".00",
                          ifelse(x > 0, paste0("$+$", f2(abs(x))), paste0("$-$", f2(abs(x))))))
bold_if <- function(txt, cond) ifelse(cond, paste0("\\textbf{", txt, "}"), txt)

g <- function(meth, out, col) pooled[method == meth & outcome == out][[col]]
rows <- vapply(CRIT, function(cr) {
  b <- vapply(c("g_rel", "g_abs", "kendall"), function(o) g(cr, o, "beats"), 0)
  md <- vapply(c("g_rel", "g_abs", "kendall"), function(o) g(cr, o, "med_delta"), 0)
  pb <- pooled_bs[method == cr]
  nf <- pooled_any[method == cr]$no_fit_cells
  paste(c(CRIT_TEX[[cr]], b, pb$all3, pb$noloss, sgn(md), nf), collapse = " & ")
}, "")
# Bold the best entry of each column (max for counts and gains, min for no-fit).
best <- function(v, f = max) v == f(v)
mat <- do.call(rbind, strsplit(rows, " & ", fixed = TRUE))
num <- function(j) suppressWarnings(as.numeric(gsub("[^0-9.-]", "", gsub("\\$-\\$", "-", gsub("\\$\\+\\$", "", mat[, j])))))
for (j in 2:6) mat[, j] <- bold_if(mat[, j], best(num(j)))
for (j in 7:9) mat[, j] <- bold_if(mat[, j], best(num(j)))
mat[, 10] <- bold_if(mat[, 10], best(num(10), min))
main_rows <- apply(mat, 1, paste, collapse = " & ")
n_cells <- nrow(d) / length(CRIT) / 3
tab_main <- c(
  "\\begin{table}[t]",
  "\\centering",
  sprintf("\\caption{\\textbf{Item selection under positive shift at %g\\%% of items.} %d benchmark--shift cells: Appendix benchmarks under bottom 50\\%% $\\to$ top 50\\%%, bottom 90\\%% $\\to$ top 10\\%% and all but the top ten $\\to$ top ten. Counts: cells where the selected items beat random order on $G$, $\\Phi$ or Kendall's $\\tau$, on all three, and with no outcome more than %.2f below random (\\emph{no loss}). \\emph{No fit}: outcome cells (of %d) where all measured models answer the selected items identically. Bold: best per column.}",
          100 * flags$frac, n_bs, flags$loss, 3L * n_bs),
  "\\label{tab:shift-selection}",
  "\\resizebox{\\linewidth}{!}{%",
  "\\begin{tabular}{lccccccccc}",
  "\\toprule",
  " & \\multicolumn{5}{c}{Benchmark--shift cells (of %N)} & \\multicolumn{3}{c}{Median gain over random} & \\\\",
  "\\cmidrule(lr){2-6}\\cmidrule(lr){7-9}",
  "Criterion & $G$ & $\\Phi$ & $\\tau$ & All three & No loss & $\\Delta G$ & $\\Delta\\Phi$ & $\\Delta\\tau$ & No fit \\\\",
  "\\midrule",
  paste0(main_rows, " \\\\"),
  "\\bottomrule",
  "\\end{tabular}}",
  "\\end{table}")
tab_main <- gsub("%N", n_bs, tab_main, fixed = TRUE)
writeLines(tab_main, file.path(flags$out, "shift_table_main.tex"))

# By shift: "beats / n (median delta)" per outcome.
hdr_sh <- paste(sprintf("\\multicolumn{3}{c}{%s}", SHIFTS$tex), collapse = " & ")
cm <- paste(sprintf("\\cmidrule(lr){%d-%d}", c(2, 5, 8), c(4, 7, 10)), collapse = "")
sub_hdr <- paste(rep(OUT_TEX, 3), collapse = " & ")
nn <- by_shift[method == CRIT[1] & outcome == "g_rel", .(shift, n)]
row_sh <- function(cr) {
  cells_txt <- unlist(lapply(SHIFTS$code, function(s) vapply(c("g_rel", "g_abs", "kendall"), function(o) {
    r <- by_shift[method == cr & shift == s & outcome == o]
    sprintf("%d (%s)", r$beats, sgn(r$med_delta))
  }, "")))
  paste(c(CRIT_TEX[[cr]], cells_txt), collapse = " & ")
}
tab_sh <- c(
  "\\begin{table}[ht]",
  "\\centering",
  sprintf("\\caption{\\textbf{Item selection by shift at %g\\%% of items.} Each entry: number of benchmarks in which the criterion's selected items beat random order on the outcome, and in parentheses the median gain over random across benchmarks (no-fit cells excluded from the median, counted as not beating random). Benchmarks per shift: %s.}",
          100 * flags$frac, {
            x <- sprintf("%d (%s)", nn$n[match(SHIFTS$code, nn$shift)], SHIFTS$words)
            paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)])
          }),
  "\\label{tab:shift-by-shift}",
  "\\resizebox{\\linewidth}{!}{%",
  "\\begin{tabular}{lccccccccc}",
  "\\toprule",
  paste0(" & ", hdr_sh, " \\\\"), cm,
  paste0("Criterion & ", sub_hdr, " \\\\"),
  "\\midrule",
  paste0(vapply(CRIT, row_sh, ""), " \\\\"),
  "\\bottomrule",
  "\\end{tabular}}",
  "\\end{table}")
writeLines(tab_sh, file.path(flags$out, "shift_table_by_shift.tex"))

# Levels: median value at 10% of items, with Random for reference.
row_lv <- function(cr) paste(c(CRIT_TEX[[cr]], unlist(lapply(SHIFTS$code, function(s) vapply(c("g_rel", "g_abs", "kendall"),
  function(o) f2(levels[method == cr & shift == s & outcome == o]$median), "")))), collapse = " & ")
tab_lv <- c(
  "\\begin{table}[ht]",
  "\\centering",
  sprintf("\\caption{\\textbf{Median outcome at %g\\%% of items, by shift.} Median across benchmarks of $G$, $\\Phi$ and Kendall's $\\tau$ of the first selected items, measured on the higher-scoring population, over the benchmarks where the criterion has a regular fit (no-fit counts in Table~\\ref{tab:shift-selection}); Random is the mean of 10 random orders.}", 100 * flags$frac),
  "\\label{tab:shift-levels}",
  "\\resizebox{\\linewidth}{!}{%",
  "\\begin{tabular}{lccccccccc}",
  "\\toprule",
  paste0(" & ", hdr_sh, " \\\\"), cm,
  paste0("Criterion & ", sub_hdr, " \\\\"),
  "\\midrule",
  paste0(vapply(c(CRIT, "random"), row_lv, ""), " \\\\"),
  "\\bottomrule",
  "\\end{tabular}}",
  "\\end{table}")
writeLines(tab_lv, file.path(flags$out, "shift_table_levels.tex"))

log_msg("wrote %s/{shift_table_main,shift_table_by_shift,shift_table_levels}.tex and summary CSVs", flags$out)
print(merge(merge(dcast(pooled, method ~ outcome, value.var = c("beats", "med_delta")), pooled_bs, by = "method"), pooled_any, by = "method"))
print(dcast(by_shift, method ~ shift + outcome, value.var = "beats"))
print(dcast(levels, method ~ shift + outcome, value.var = "median", fun.aggregate = function(x) round(x, 2)))
print(d[no_fit == TRUE, .N, by = .(method, status)])
print(range(cells$n10))
