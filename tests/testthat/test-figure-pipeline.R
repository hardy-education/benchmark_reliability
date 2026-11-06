# Populations, the cached compute layer, figure helpers and the LaTeX writer.

sim_long <- function() {
  mk <- function(b, M, I, seed) {
    X <- sim_responses(M, I, seed = seed)
    data.table::data.table(benchmark = b, model = rep(rownames(X), ncol(X)), item = rep(colnames(X), each = nrow(X)),
                           resp = as.vector(X))
  }
  long <- data.table::rbindlist(list(mk("b1", 70, 40, 11), mk("b2", 30, 25, 12)))
  data.table::setkeyv(long, c("benchmark", "model", "item"))
  long
}

test_that("new populations have the documented membership", {
  sc <- data.table::data.table(benchmark = "b", model = sprintf("m%02d", 1:40), score = seq(0, 1, length.out = 40))
  mid <- population_models(sc, "b", "middle_half")
  expect_length(mid, 20)
  expect_false(any(c("m01", "m40") %in% mid))
  r <- population_models(sc, "b", "random_frac:0.5:7")
  rc <- population_models(sc, "b", "random_comp:0.5:7")
  expect_length(intersect(r, rc), 0)
  expect_setequal(c(r, rc), sc$model)
  set.seed(1); a <- runif(1); set.seed(1); invisible(population_models(sc, "b", "random_frac:0.5:7")); b <- runif(1)
  expect_identical(a, b)                       # drawing a random half leaves the caller's RNG alone
  expect_equal(population_label(c("middle_half", "top_n:10", "bottom_half->top_half")),
               c("Middle 50%", "Top n=10", "Bottom 50% -> Top 50%"))
})

test_that("eligibility: top subsets need > 5 models and at most half the benchmark", {
  mk <- function(M) data.table::data.table(benchmark = "b", model = paste0("m", 1:M), score = seq_len(M) / M)
  expect_false(population_eligible(mk(41), "b", "top_frac:0.1"))   # 4 models
  expect_false(population_eligible(mk(59), "b", "top_frac:0.1"))   # 5 models
  expect_true(population_eligible(mk(60), "b", "top_frac:0.1"))    # 6 models
  expect_true(population_eligible(mk(20), "b", "top_n:10"))        # 10 of 20
  expect_false(population_eligible(mk(19), "b", "top_n:10"))       # 10 of 19 > half
  expect_true(population_eligible(mk(41), "b", "top_half"))
  expect_false(population_eligible(mk(41), "b", "middle_half->top_frac:0.1"))
  expect_equal(parse_pair("bottom_half->top_half"), list(reference = "bottom_half", measured = "top_half"))
  expect_equal(pair_tag("top_frac:0.1", "all"), "top_frac-0.1__all")
})

test_that("model names are cleaned like janitor::make_clean_names()", {
  expect_equal(clean_model_name(c("allenai/OLMo-1.7-7B-hf", "CohereForAI/aya-23-8B", "01-ai/Yi-1.5-6B",
                                  "deepseek-ai/DeepSeek-R1-Distill-Qwen-14B")),
               c("allenai_ol_mo_1_7_7b_hf", "cohere_for_ai_aya_23_8b", "x01_ai_yi_1_5_6b",
                 "deepseek_ai_deep_seek_r1_distill_qwen_14b"))
})

test_that("cached curves are computed once, extended by method, and checkable", {
  long <- sim_long(); scores <- model_scores(long); dir <- tempfile("cache")
  meth <- c("lambda_star", "difficulty", "random")
  cv <- cached_curves(long, scores, c("b1", "b2"), "bottom_half", "top_half", methods = meth, n_random = 2, dir = dir)
  expect_setequal(unique(cv$method), meth)
  f <- file.path(dir, "curves", "bottom_half__top_half", "b1.csv.gz")
  expect_true(file.exists(f))
  t1 <- file.mtime(f)
  cv2 <- cached_curves(long, scores, c("b1", "b2"), "bottom_half->top_half", methods = meth, n_random = 2, dir = dir)
  expect_equal(file.mtime(f), t1)                       # read from cache, not recomputed
  expect_equal(nrow(cv2), nrow(cv))
  cv3 <- cached_curves(long, scores, "b1", "bottom_half", "top_half", methods = c(meth, "ciic"), n_random = 2, dir = dir)
  expect_true("ciic" %in% cv3$method)                   # missing method appended
  expect_true(all(c("lambda_star", "ciic") %in% data.table::fread(f)$method))
  ck <- check_curves(cv)
  expect_true(all(ck$pct_outside_01 == 0))
  expect_true(all(ck$n_points > 0))

  # An explicit order, cached under its own tag.
  X <- response_matrix(long, "b1")
  my <- order_to_metrics(list(b1 = rev(colnames(X))), method = "reverse")
  cvm <- cached_curves(long, scores, "b1", "all", "all", methods = "reverse", metrics = my, run_tag = "mine", dir = dir)
  expect_true(file.exists(file.path(dir, "curves", "all__all__mine", "b1.csv.gz")))
  direct <- reliability_curve(X, rev(colnames(X)))
  expect_equal(cvm$g_rel, direct$g_rel, tolerance = 1e-12)
})

test_that("check_curves agrees with lmer on refitted prefixes", {
  skip_if_not_installed("lme4")
  long <- sim_long(); scores <- model_scores(long); dir <- tempfile("cache")
  cv <- cached_curves(long, scores, "b2", "all", "all", methods = c("difficulty"), dir = dir)
  im <- cached_item_metrics(long, scores, "b2", "all", "difficulty", dir = dir)
  ck <- check_curves(cv, long, scores, verify = 3, metrics = im)
  expect_lt(ck$max_abs_diff_lmer, 1e-4)
})

test_that("cached G-studies: proportions sum to one and ineligible rows are flagged", {
  long <- sim_long(); scores <- model_scores(long)
  g <- cached_gstudies(long, scores, c("b1", "b2"), c("all", "top_frac:0.1", "top_n:10"), name = "t", dir = tempfile())
  ok <- g[eligible == TRUE]
  expect_equal(ok$prop_model + ok$prop_item + ok$prop_residual, rep(1, nrow(ok)), tolerance = 1e-12)
  expect_false(g[benchmark == "b2" & population == "top_frac:0.1"]$eligible)   # 3 of 30 models
  expect_true(g[benchmark == "b1" & population == "top_frac:0.1"]$eligible)    # 7 of 70
  expect_true(is.na(g[benchmark == "b2" & population == "top_frac:0.1"]$var_model))
  vc <- variance_components(response_matrix(long, "b1"))
  expect_equal(g[benchmark == "b1" & population == "all"]$var_model, vc$var_model)
})

test_that("landscape classes match the SNR definitions", {
  set.seed(3)
  pm <- runif(500, 0, 0.6); pr <- runif(500) * (1 - pm)
  for (n in c(1, 10, 1000)) for (s in c(4, 9, 100)) {
    cat <- landscape_category(pm, pr, n, s)
    snr_rel <- n * pm / pr; snr_abs <- n * pm / (1 - pm)
    expect_equal(cat == "both", snr_abs >= s)
    expect_equal(cat == "relative", snr_abs < s & snr_rel >= s)
  }
  expect_equal(snr_threshold_value(9), 0.9)
  expect_equal(snr_threshold_value(4, "tau"), (2 / pi) * asin(sqrt(0.8)))
})

test_that("every figure type builds and saves", {
  skip_if_not_installed("ggplot2")
  long <- sim_long(); scores <- model_scores(long); dir <- tempfile("cache")
  meth <- c("lambda_star", "difficulty", "random")
  cv <- cached_curves(long, scores, c("b1", "b2"), "all", "all", methods = meth, n_random = 2, dir = dir)
  cvh <- cached_curves(long, scores, c("b1", "b2"), "top_half", "top_half", methods = "random", n_random = 2, dir = dir)
  g <- cached_gstudies(long, scores, c("b1", "b2"), c("all", "top_half", "top_n:10"), dir = dir)
  plots <- list(
    plot_selection_curves(cv, "g_rel", lines = "raw"),
    plot_selection_curves(cv, "kendall", lines = "both"),
    plot_reliability_contrast(rbind(cv, cvh, fill = TRUE), c("all", "top_half"), lines = "raw"),
    plot_snr_r2_path(rbind(cv, cvh, fill = TRUE), "measured"),
    plot_snr_r2_path(cv, "method"),
    plot_dstudy(g),
    plot_reliability_landscape(g, 9, c(1, 10)),
    plot_population_dots(g),
    plot_population_histograms(scores, c("b1", "b2")),
    plot_items_needed(g, 9))
  for (p in plots) expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  f <- tempfile(fileext = ".pdf")
  save_figure(plots[[1]], f, height = 3)
  expect_gt(file.size(f), 1000)
})

test_that("LaTeX output has one figure environment per manifest row", {
  expect_equal(latex_escape("Top 10% & bbh_v2"), "Top 10\\% \\& bbh\\_v2")
  man <- data.table::data.table(section = c("body", "appendix", "appendix"), group = c("g1", "g2", "g2"),
                                file = c("a.pdf", "b.pdf", "c.pdf"), caption = c("A", "B", "C"),
                                label = c("fig:a", "fig:b", "fig:c"))
  f <- tempfile(fileext = ".tex")
  write_latex_figures(man, f, clear_every = 2)
  tex <- readLines(f)
  expect_equal(sum(grepl("\\begin{figure}", tex, fixed = TRUE)), 3)
  expect_true(any(grepl("\\label{fig:b}", tex, fixed = TRUE)))
  expect_lt(which(grepl("Main-text", tex)), which(grepl("Appendix", tex)))
})

test_that("Complete-set cells round-trip even when item ids are multi-line prompts", {
  d <- tempfile("raw"); dir.create(d)
  eee <- data.table::data.table(
    eee_benchmark = rep(c("bench_a", "bench_b"), each = 12),
    model_id = rep(rep(paste0("m", 1:4), each = 3), 2),
    sample_id = c(rep(c("q1", "q2", "q3"), 4), rep(c("What is\n2, \"plus\" 2?", "line\r\ntwo", strrep("x", 80)), 4)),
    is_correct = c(rep(c(TRUE, FALSE, TRUE), 4), rep(c(TRUE, TRUE, FALSE), 2), rep(c(FALSE, TRUE, FALSE), 2)))
  data.table::fwrite(eee, file.path(d, "eee_item_level_filtered.csv"))
  out <- tempfile(fileext = ".csv.gz")
  suppressMessages(build_complete_cells(complete_sources(d), out))
  x <- complete_responses(out, aggdf = NULL)
  expect_type(x$resp, "double")
  expect_equal(sort(unique(x[benchmark == "bench_b"]$item)), c("q1", "q2", "q3"))
  expect_equal(sort(unique(x[benchmark == "bench_a"]$item)), c("q1", "q2", "q3"))
  expect_equal(x[benchmark == "bench_b", mean(resp)], mean(eee[eee_benchmark == "bench_b"]$is_correct))
})

test_that("positive-shift complements partition the models", {
  sc <- data.table::data.table(benchmark = "b", model = sprintf("m%03d", 1:83), score = seq(0, 1, length.out = 83))
  top <- population_models(sc, "b", "top_frac:0.1"); rest <- population_models(sc, "b", "not_top_frac:0.1")
  expect_length(top, 8); expect_length(rest, 75)
  expect_length(intersect(top, rest), 0); expect_setequal(c(top, rest), sc$model)
  expect_true(min(sc[model %in% top]$score) > max(sc[model %in% rest]$score))
  expect_length(population_models(sc, "b", "not_top_n:10"), 73)
  expect_equal(population_label(c("not_top_frac:0.1", "not_top_n:10")), c("Bottom 90%", "Bottom N-10"))
  expect_true(population_eligible(sc, "b", "not_top_frac:0.1->top_frac:0.1"))   # 90% is fine for a bottom group
  expect_true(population_eligible(sc, "b", "bottom_frac:0.9"))
  expect_false(population_eligible(sc[1:50], "b", "not_top_frac:0.1->top_frac:0.1"))  # top 10% of 50 = 5 models
})

test_that("temporal halves need upload dates for most of a benchmark's models", {
  sc <- data.table::data.table(benchmark = rep(c("dated", "sparse"), each = 30), model = paste0("m", 1:60),
                               score = runif(60), date = as.Date("2024-01-01") + c(1:30, c(1:3, rep(NA, 27))))
  expect_true(population_eligible(sc, "dated", "early_half->late_half"))
  expect_false(population_eligible(sc, "sparse", "early_half->late_half"))
  e <- population_models(sc, "dated", "early_half"); l <- population_models(sc, "dated", "late_half")
  expect_length(intersect(e, l), 0)
  expect_true(max(sc[model %in% e]$date) < min(sc[model %in% l]$date))
})

test_that("plot_pair_curves draws one line per pair with measured-population colours", {
  long <- sim_long(); scores <- model_scores(long); dir <- tempfile("cache")
  pairs <- c("all->all", "bottom_half->top_half", "top_half->top_half")
  cv <- data.table::rbindlist(lapply(pairs, function(pp)
    cached_curves(long, scores, c("b1", "b2"), pp, methods = c("difficulty", "random"), n_random = 2, dir = dir)), fill = TRUE)
  p <- plot_pair_curves(cv, "difficulty", "g_rel", pairs = pairs, lines = "raw")
  b <- ggplot2::ggplot_build(p)
  ln <- b$data[[length(b$data)]]
  expect_equal(length(unique(ln$group)), 3)
  sc <- scale_pair(pairs, "linetype")[[1]]
  expect_equal(unname(sc$palette(3)), c("solid", "22", "solid"))
})

test_that("selection curves can put population pairs in rows with a budget marker", {
  long <- sim_long(); scores <- model_scores(long); dir <- tempfile("cache")
  pairs <- c("bottom_half->top_half", "all->all")
  cv <- data.table::rbindlist(lapply(pairs, function(pp)
    cached_curves(long, scores, c("b1", "b2"), pp, methods = c("lambda_star", "random"), n_random = 2, dir = dir)), fill = TRUE)
  p <- plot_selection_curves(cv, "g_rel", pair_rows = TRUE, pairs = pairs, mark_frac = 0.1, lines = "raw")
  b <- ggplot2::ggplot_build(p)
  expect_equal(nrow(b$layout$layout), 4)                     # 2 pairs x 2 benchmarks
  vl <- b$data[[which(vapply(b$data, function(x) "xintercept" %in% names(x), NA))[1]]]
  N <- cv[, .(N = max(n_items)), by = benchmark]$N
  expect_setequal(round(10^vl$xintercept), pmax(2, round(0.1 * N)))
})
