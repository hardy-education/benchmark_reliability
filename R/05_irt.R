# ==============================================================================
# 05_irt.R -- 2PL item response models via `mirt`.
#
# The 2PL discrimination parameter `a` is one of the item-selection criteria
# compared against lambda*_6 (App. D.8.1), and the conditional-reliability curve
# is the latent-trait analogue of the population-dependence argument in
# Sec. 3.2: test information, and therefore reliability, is a function of where
# the model population sits on the ability scale.
# ==============================================================================

# `ggmirt`'s plot constructors are used as the extraction path for item and
# person parameters, because their `@data` slots carry exactly the tidied
# columns the pipeline wants. Note that this makes three of the plots data
# sources rather than decorations:
#
#   personDist()  -> person ability values      (`th`)
#   itemDist()    -> item parameters a, b, g, u
#   conRelPlot()  -> conditional reliability curve
#
# Only `testInfoPlot()` and the ggpubr arrangement are display-only, so those
# are the parts skipped when `plots = FALSE`. The original built all five plus
# the arrangement unconditionally and only gated the `print()`, which meant
# `plots = FALSE` still paid for the figure work -- and paid it via `ggpubr`,
# a package none of the scripts ever loaded, so the first `plots = TRUE` call
# in a fresh session would have failed with "there is no package called ggpubr".

#' Fit 2PL item response models to one response matrix
#'
#' @param wide Wide response matrix (`model` plus one column per item).
#' @param theta_range Latent-trait range for scoring and plotting.
#' @param item_types Vector of `mirt` item types to fit. More than one triggers
#'   pairwise likelihood-ratio comparisons in the returned `comparisons` table.
#' @param metadata Output of `sample_metadata()`; computed if absent.
#' @param plots Draw and print the diagnostic figure panel. Requires `ggpubr`.
#' @param n_cluster Workers for `mirtCluster()`.
#' @return List with `models`, `comparisons`, `theta_curve` (conditional
#'   reliability), `items` (item parameters and fit), and `persons`.
fit_irt <- function(wide,
                    theta_range = c(-9, 9),
                    item_types = "2PL",
                    metadata = NULL,
                    plots = FALSE,
                    n_cluster = 5L) {
  if (plots && !requireNamespace("ggpubr", quietly = TRUE)) {
    stop("`plots = TRUE` needs the 'ggpubr' package; install it or use plots = FALSE.")
  }
  if (is.null(metadata)) metadata <- sample_metadata(wide)

  b        <- metadata$benchmark[1]
  n_items  <- metadata$ni[1]
  iter     <- metadata$iter[1]
  n_models <- metadata$nmods[1]

  scores <- item_block(wide) |> dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))

  mirt::mirtCluster(spec = n_cluster)
  on.exit(mirt::mirtCluster(remove = TRUE), add = TRUE)

  models  <- list()
  persons <- list()
  items   <- list()
  thetas  <- list()

  for (it in item_types) {
    tic(glue("mirt {it} fit for {b}"))
    fitted <- tryCatch(
      fit_one_irt(scores, wide$model, it, theta_range, b, n_items, iter,
                  n_models, plots),
      error = function(e) {
        warning(glue("mirt {it} fit failed for {b}: {conditionMessage(e)}"))
        irt_na_result(scores, wide$model, it, b, n_items, iter, n_models)
      }
    )
    toc()
    models[[it]]  <- fitted$model
    thetas[[it]]  <- fitted$theta_curve
    items[[it]]   <- fitted$items
    persons[[it]] <- fitted$persons
  }

  models <- models[!vapply(models, is.null, logical(1))]

  list(
    models      = models,
    comparisons = compare_irt_models(models, b, n_items, iter, n_models),
    theta_curve = purrr::list_rbind(thetas,  names_to = "irt"),
    items       = purrr::list_rbind(items,   names_to = "irt"),
    persons     = purrr::list_rbind(persons, names_to = "irt")
  )
}

# Fit a single item type and tidy its output. Split out of `fit_irt()` so the
# tryCatch wraps one comprehensible unit instead of 150 lines.
fit_one_irt <- function(scores, model_ids, item_type, theta_range,
                        benchmark, n_items, iter, n_models, plots) {
  md <- mirt::mirt(
    scores,
    model     = 1,
    itemtype  = item_type,
    accelerate = "squarem",
    # L-BFGS-B suits the 2PL; richer item types may need a different optimizer.
    optimizer = "L-BFGS-B",
    technical = list(NCYCLES = 2000, theta_lim = theta_range, MAXQUAD = 100000)
  )

  person_plot <- ggmirt::personDist(md, theta_range = theta_range)
  item_plot   <- ggmirt::itemDist(md, theta_range = theta_range,
                                  shape = 17, color = "red")
  conrel_plot <- ggmirt::conRelPlot(md, theta_range = theta_range,
                                    title = glue("{benchmark} rxx: {item_type}"))

  if (plots) {
    info_plot <- ggmirt::testInfoPlot(md, theta_range = theta_range) +
      ggplot2::labs(title = glue("{benchmark} info: {item_type}"))
    panel <- ggpubr::ggarrange(
      ggpubr::ggarrange(person_plot, item_plot, ncol = 1, align = "hv",
                        heights = c(1, 2)),
      ggpubr::ggarrange(info_plot, conrel_plot, ncol = 1, align = "hv",
                        heights = c(1.5, 1.5)),
      ncol = 2
    )
    print(panel)
  }

  preds <- mirt::fscores(md, method = "EAP", full.scores.SE = TRUE,
                         theta_lim = theta_range, max_theta = 20, QMC = TRUE)

  persons <- mirt::personfit(md, Theta = as.matrix(preds[, 1]),
                             method = "EAP", max_theta = 20, QMC = TRUE) |>
    tibble::tibble() |>
    dplyr::mutate(
      model     = model_ids,
      th        = person_plot@data$value,
      predicted = preds[, 1],
      se        = preds[, 2],
      irt       = item_type,
      benchmark = benchmark,
      n_items   = n_items,
      iter      = iter,
      n_models  = n_models,
      held_out  = FALSE,
      rxx       = mirt::empirical_rxx(preds)
    )

  items <- item_plot@data |>
    tibble::tibble() |>
    dplyr::mutate(irt = item_type, held_out = FALSE, benchmark = benchmark,
                  n_items = n_items, iter = iter, n_models = n_models) |>
    dplyr::rename(item = items, a = items.a, b = items.b,
                  g = items.g, u = items.u) |>
    dplyr::left_join(irt_item_fit(md, preds), by = "item")

  theta_curve <- conrel_plot@data |>
    tibble::tibble() |>
    dplyr::mutate(irt = item_type, benchmark = benchmark, n_items = n_items,
                  iter = iter, n_models = n_models)

  list(model = md, persons = persons, items = items, theta_curve = theta_curve)
}

#' Item fit statistics, selected to the columns that exist for the fitted type
#'
#' `mirt::itemfit()` returns a different column set per item type and per
#' `fit_stats` combination, so every call site in the original repeated the same
#' twelve-name `any_of()` list. It lives here once.
irt_item_fit <- function(md, preds = NULL, theta_range = c(-9, 9)) {
  args <- list(md, fit_stats = c("infit", "X2", "G2", "Zh"),
               method = "EAP", QMC = TRUE)
  if (!is.null(preds)) args$Theta <- as.matrix(preds[, 1])
  else args$theta_lim <- theta_range

  do.call(mirt::itemfit, args) |>
    tibble::tibble() |>
    dplyr::select(item, dplyr::any_of(IRT_FIT_STATS))
}

IRT_FIT_STATS <- c("infit", "outfit", "PV_Q1", "X2", "z.infit", "z.outfit",
                   "S_X2", "Zh", "X2*", "X2*_df", "PV_Q1*", "G2")

# The all-NA result returned when a fit fails, so a failed benchmark still
# contributes rows with the same columns as a successful one.
irt_na_result <- function(scores, model_ids, item_type, benchmark,
                          n_items, iter, n_models) {
  list(
    model = NULL,
    theta_curve = tibble::tibble(value = NA_real_, irt = item_type,
                                 benchmark = benchmark, n_items = n_items,
                                 iter = iter, n_models = n_models),
    items = tibble::tibble(
      item = colnames(scores), a = NA_real_, b = NA_real_,
      g = NA_real_, u = NA_real_, irt = item_type, benchmark = benchmark,
      iter = iter, n_items = n_items, n_models = n_models, held_out = FALSE
    ),
    persons = tibble::tibble(model = model_ids, th = NA_real_, irt = item_type,
                             benchmark = benchmark, n_items = n_items,
                             iter = iter, n_models = n_models, held_out = FALSE)
  )
}

#' Pairwise likelihood-ratio comparisons between fitted item types
#'
#' Returns an empty tibble when fewer than two models converged, which is the
#' usual case since the pipeline fits the 2PL alone.
compare_irt_models <- function(models, benchmark, n_items, iter, n_models) {
  if (length(models) < 2L) return(tibble::tibble())
  pairs <- utils::combn(seq_along(models), 2L, simplify = FALSE)

  purrr::map(pairs, function(ij) {
    i <- ij[1]; j <- ij[2]
    tryCatch(
      mirt::anova(models[[i]], models[[j]]) |>
        tibble::tibble() |>
        dplyr::mutate(
          benchmark = benchmark,
          irt  = c(names(models)[i], names(models)[j]),
          irt1 = names(models)[i],
          irt2 = names(models)[j],
          n_items = n_items, iter = iter, n_models = n_models
        ),
      error = function(e) {
        message(glue("Cannot compare {names(models)[i]} and {names(models)[j]}."))
        tibble::tibble()
      }
    )
  }) |>
    purrr::list_rbind()
}

# ------------------------------------------------------------------------------
# Scoring held-out models
# ------------------------------------------------------------------------------

#' Score a new model population with fixed item parameters
#'
#' The out-of-sample half of the distribution-shift analysis: item parameters
#' are held at their values from the fitting population, and only the new
#' models' abilities are estimated. `mod2values()` plus `TOL = NaN` is the
#' `mirt` idiom for "keep these parameters, score these responses".
#'
#' @param fit Output of `fit_irt()`.
#' @param new_wide Wide response matrix for the held-out models.
#' @param theta_range Latent-trait range. Was read from the calling
#'   environment in the original `mirt_test_preds()`, which made the function
#'   silently dependent on a global of the same name.
#' @return List with `persons` and `items` for the held-out population.
score_held_out <- function(fit, new_wide, theta_range = c(-9, 9)) {
  new_scores <- item_block(new_wide)
  persons <- list()
  items   <- list()

  for (nm in names(fit$models)) {
    md <- fit$models[[nm]]
    if (is.null(md) || is.na(md@Fit$AIC)) next

    sv      <- mirt::mod2values(md)
    md_new  <- mirt::mirt(new_scores, pars = sv, TOL = NaN)
    preds   <- mirt::fscores(md_new, response.pattern = new_scores,
                             method = "EAP", full.scores.SE = TRUE,
                             theta_lim = theta_range, max_theta = 20, QMC = TRUE)

    persons[[nm]] <- mirt::personfit(md_new, Theta = as.matrix(preds[, 1]),
                                     method = "EAP", max_theta = 20, QMC = TRUE) |>
      tibble::tibble() |>
      dplyr::mutate(
        model = new_wide$model, irt = nm, held_out = TRUE,
        predicted = preds[, 1], se = preds[, 2],
        rxx = mirt::empirical_rxx(preds),
        n_items = ncol(new_scores),
        n_models = n_unique(new_wide$model)
      )

    items[[nm]] <- irt_item_fit(md_new, preds) |>
      dplyr::mutate(irt = nm, held_out = TRUE,
                    n_models = n_unique(new_wide$model))
  }

  list(
    persons = purrr::list_rbind(persons, names_to = "irt"),
    items   = purrr::list_rbind(items,   names_to = "irt")
  )
}

#' Compare test information curves across fitted models
#'
#' Plots I(theta) for each model in `models` on one set of axes -- the
#' population-dependence picture of Sec. 3.2 made visual, since a benchmark is
#' reliable only over the ability range where its information is high.
plot_test_information <- function(models, theta_range = c(-4, 4),
                                  title = "Test information curves") {
  theta <- seq(theta_range[1], theta_range[2], by = 0.01)
  Theta <- matrix(theta)

  purrr::imap_dfr(models, function(md, nm) {
    tibble::tibble(theta = theta, information = as.vector(mirt::testinfo(md, Theta)),
                   model = nm)
  }) |>
    ggplot(aes(theta, information, colour = model)) +
    geom_line(alpha = 0.75) +
    labs(x = expression(theta), y = expression(I(theta)), title = title) +
    scale_br_colour() +
    theme_br(legend = "bottom")
}
