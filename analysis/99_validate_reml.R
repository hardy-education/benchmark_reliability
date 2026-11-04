# Validation: closed-form REML vs lme4::lmer on the real benchmarks.
#
#   Rscript analysis/99_validate_reml.R --benchmarks=all --workers=8
#
# For each benchmark and population (all models, top 10%) it compares:
#   * the full matrix: closed form vs lmer;
#   * prefixes of the lambda_star order and of one random order, at n = 2..40
#     and 15 log-spaced larger n: closed form vs lmer (where lmer is
#     affordable), and vs the pre-fix pooling rule that set both components
#     to zero whenever both moment estimates were negative;
#   * benchmarks with missing cells: lmer on the observed cells vs the
#     balanced formulas with na.rm = TRUE.
#
# Writes results/validation/reml_prefixes.csv.gz, reml_missing.csv and prints
# a summary.

source("analysis/_common.R")
flags <- parse_flags(defaults = list(benchmarks = "all18", workers = 1L, max_lmer_cells = 2e5))
long <- load_responses(benchmarks = flags$benchmarks)
benches <- sort(unique(long$benchmark))
scores <- model_scores(long)
pops <- c("all", "top_frac:0.1")

# The pre-fix rule, kept here only for comparison.
old_reml <- function(ssm, ssi, ssr, M, I) {
  dfm <- M - 1; dfi <- I - 1; dfr <- (M - 1) * (I - 1)
  msm <- ssm / dfm; msi <- ssi / dfi; msr <- ssr / dfr
  vm <- (msm - msr) / I; vi <- (msi - msr) / M; vr <- msr
  m0 <- vm < 0 & vi >= 0; i0 <- vi < 0 & vm >= 0; both <- vm < 0 & vi < 0
  vr[m0] <- ((ssm + ssr) / (dfm + dfr))[m0]; vm[m0] <- 0; vi[m0] <- ((msi - vr) / M)[m0]
  vr[i0] <- ((ssi + ssr) / (dfi + dfr))[i0]; vi[i0] <- 0; vm[i0] <- ((msm - vr) / I)[i0]
  vr[both] <- ((ssm + ssi + ssr) / (dfm + dfi + dfr))[both]; vm[both] <- 0; vi[both] <- 0
  list(var_model = vm, var_item = vi, var_residual = vr)
}

g_of <- function(vm, vr, n) vm / (vm + vr / n)

one <- function(job) {
  b <- job$b; p <- job$p
  X <- handle_missing(response_matrix(long, b, models = population_models(scores, b, p)), "auto")
  M <- nrow(X); N <- ncol(X)
  ref <- population_tag(p)
  met <- fread(results_path("item_metrics", ref, paste0(b, "__fast.csv.gz")))[method == "lambda_star"]
  orders <- list(lambda_star = selection_order(setNames(met$value, met$item), colnames(X), "lambda_star"),
                 random = selection_order(NULL, colnames(X), "random", seed = 2027))
  steps <- sort(unique(c(2:min(40, N), round(10^seq(log10(min(41, N)), log10(N), length.out = 15)))))
  rbindlist(lapply(names(orders), function(o) {
    ord <- orders[[o]]
    cv <- reliability_curve(X, ord, steps = steps, kendall = FALSE)
    Xo <- X[, ord, drop = FALSE]
    old <- rbindlist(lapply(steps, function(n) {
      Y <- Xo[, seq_len(n), drop = FALSE]; rs <- rowSums(Y)
      e <- benchrel:::.crossed_ss(sum(Y^2), sum(rs^2), sum(colSums(Y)^2), sum(rs), M, n, exact = TRUE)
      as.data.table(old_reml(e$ssm, e$ssi, e$ssr, M, n))
    }))
    lm <- rbindlist(lapply(steps, function(n) {
      if (M * n > flags$max_lmer_cells) return(data.table(var_model = NA_real_, var_item = NA_real_, var_residual = NA_real_, status = NA_character_))
      r <- tryCatch(suppressWarnings(benchrel:::.lmer_components(Xo[, seq_len(n), drop = FALSE])),
                    error = function(e) list(var_model = NA_real_, var_item = NA_real_, var_residual = NA_real_, status = "lmer_error"))
      as.data.table(r)
    }))
    data.table(benchmark = b, population = p, order = o, n_items = steps, n_models = M, status = cv$status,
               new_m = cv$var_model, new_i = cv$var_item, new_r = cv$var_residual,
               old_m = old$var_model, old_i = old$var_item, old_r = old$var_residual,
               lmer_m = lm$var_model, lmer_i = lm$var_item, lmer_r = lm$var_residual, lmer_status = lm$status)
  }))
}

jobs <- unlist(lapply(benches, function(b) lapply(pops, function(p) list(b = b, p = p))), recursive = FALSE)
t0 <- proc.time()[["elapsed"]]
res <- par_map(jobs, one, workers = flags$workers)
err <- vapply(res, inherits, NA, "try-error")
if (any(err)) stop("validation job failed: ", as.character(res[[which(err)[1]]]))
res <- rbindlist(res)
res[, `:=`(g_new = g_of(new_m, new_r, n_items), g_old = g_of(old_m, old_r, n_items), g_lmer = g_of(lmer_m, lmer_r, n_items))]
write_result(res, results_path("validation", "reml_prefixes.csv.gz"))
log_msg("prefix validation: %d prefixes in %.0f s", nrow(res), proc.time()[["elapsed"]] - t0)

# Missing cells: lmer on observed cells vs the balanced formulas with na.rm.
miss <- rbindlist(lapply(benches, function(b) rbindlist(lapply(pops, function(p) {
  X <- response_matrix(long, b, models = population_models(scores, b, p))
  if (!anyNA(X)) return(NULL)
  X <- X[rowSums(!is.na(X)) > 0, colSums(!is.na(X)) > 0, drop = FALSE]
  M <- nrow(X); I <- ncol(X); Xc <- X - mean(X, na.rm = TRUE)
  ssm <- I * sum(rowMeans(Xc, na.rm = TRUE)^2); ssi <- M * sum(colMeans(Xc, na.rm = TRUE)^2)
  ssr <- sum(Xc^2, na.rm = TRUE) - ssm - ssi
  nv <- benchrel:::.balanced_reml(ssm, ssi, ssr, M, I)
  lm <- variance_components(X, "lmer")
  cc <- variance_components(handle_missing(X, "auto"))
  data.table(benchmark = b, population = p, n_models = M, n_items = I, pct_missing = 100 * mean(is.na(X)),
             lmer_m = lm$var_model, lmer_i = lm$var_item, lmer_r = lm$var_residual,
             narm_m = nv$var_model, narm_i = nv$var_item, narm_r = nv$var_residual,
             complete_m = cc$var_model, complete_i = cc$var_item, complete_r = cc$var_residual,
             g_lmer = g_of(lm$var_model, lm$var_residual, I), g_narm = g_of(nv$var_model, nv$var_residual, I))
}))))
write_result(miss, results_path("validation", "reml_missing.csv"))

# Summary.
cat("\nStatus of the closed-form fit (prefixes checked):\n")
print(res[, .N, by = .(population, status)][order(population, status)])
cat("\nPre-fix rule vs fixed closed form:\n")
print(res[!is.na(g_new), .(prefixes = .N,
                          changed = sum(abs(new_m - old_m) + abs(new_i - old_i) + abs(new_r - old_r) > 1e-12),
                          g_changed = sum(abs(g_new - g_old) > 1e-9),
                          max_abs_dg = max(abs(g_new - g_old))), by = population])
cat("\nFixed closed form vs lmer (regular fits where lmer ran):\n")
cmp <- res[!is.na(new_m) & !is.na(lmer_m)]
cmp[, dmax := pmax(abs(new_m - lmer_m), abs(new_i - lmer_i), abs(new_r - lmer_r))]
cmp[, rel := dmax / (new_m + new_i + new_r)]
print(cmp[, .(prefixes = .N, max_abs_component_diff = max(dmax), max_rel_diff = max(rel),
              max_abs_dg = max(abs(g_new - g_lmer), na.rm = TRUE)), by = .(population, status)][order(population, status)])
cat("\nPre-fix rule vs lmer on the prefixes the fix changed:\n")
ch <- cmp[abs(new_m - old_m) + abs(new_i - old_i) + abs(new_r - old_r) > 1e-12]
if (nrow(ch)) print(ch[, .(prefixes = .N, old_vs_lmer_max = max(pmax(abs(old_m - lmer_m), abs(old_i - lmer_i), abs(old_r - lmer_r))),
                          new_vs_lmer_max = max(dmax), old_g_vs_lmer = max(abs(g_old - g_lmer)), new_g_vs_lmer = max(abs(g_new - g_lmer)))])
cat("\nDegenerate prefixes (no regular REML fit) and what lmer returns there:\n")
print(res[status == "degenerate", .(prefixes = .N, max_n_items = max(n_items),
                                    lmer_resid_median = median(lmer_r, na.rm = TRUE), lmer_g_median = median(g_lmer, na.rm = TRUE)),
          by = .(population, benchmark)])
cat("\nMissing cells: lmer on observed cells vs na.rm balanced formulas:\n")
if (nrow(miss)) print(miss[, .(benchmark, population, pct_missing = round(pct_missing, 2), g_lmer = round(g_lmer, 4), g_narm = round(g_narm, 4),
               lmer_m = signif(lmer_m, 4), narm_m = signif(narm_m, 4), lmer_r = signif(lmer_r, 4), narm_r = signif(narm_r, 4))])
