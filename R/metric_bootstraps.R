## Item Reliability Bootstraps
## 

# Load Libraries ------

library(tictoc)
library(here)
library(easystats)
library(performance)
library(parameters)
library(modelsummary)
library(modelbased)
library(parallel)
library(data.table)
library(glue)
library(psych)
library(tidyverse)
library(lme4)
library(lmerTest)
library(fixest)
library(broom.mixed)
library(janitor)
library(naniar)
library(broom)
library(furrr)
library(mirt)
library(ggmirt)
library(future)
library(parallel)
library(futureverse)
library(data.table)
library(fixest)
library(future)
library(future.apply)
library(carrier)
library(fastverse)
fastverse_extend(Rfast,dtplyr, 
                 coop,Rfast2,cheapr,
                 stringi,stringdist,
                 vctrs, dqrng,
                 parallelDist
)
library(data.table)
library(pcaPP)
library(future.apply)
future::plan("multisession")
# plan(multicore) # for Linux/Mac
# 
# Load Data ------
## download this dataset and put in the data folder: https://drive.google.com/file/d/1SL7MNKiynqWhtn5pCN59xp4XpZpK1Qln/view?usp=sharing
aggdf <- fread(here("benchmark_reliability", "data", "aggdf.csv")) |>
  pivot_longer(cols = -c(benchmark, item), names_to = "model", values_to = "resp")

### Summary Counts ------
item_counts <- aggdf |>
  select(benchmark, item) |>
  distinct() |>
  group_by(benchmark) |>
  summarize(n = n())
mod_counts <- aggdf |>
  select(benchmark, model) |>
  distinct() |>
  group_by(benchmark) |>
  summarize(n = n())

# Functions -------
## Utility Functions -------


make_hilo_splits <- function(df) {
  df |>
    group_by(model, benchmark) |>
    summarise(resp = mean(resp, na.rm = T)) |>
    ungroup() |>
    group_by(benchmark) |>
    mutate(
      med = median(resp, na.rm = T),
      split = if_else(resp <= med, "train", "test")
    ) |>
    ungroup() |>
    drop_na() |>
    distinct()
}


make_sample_response <- function(sampdf, longrespdf, nitems = 100,
                                 samp = "train",
                                 items = NULL,
                                 npadding = 10,
                                 b = "air_bench_2024") {
  sampdf <- sampdf |>
    filter(benchmark == b, split == samp) |>
    select(model, benchmark) |>
    distinct()


  resplongdf <- semi_join(longrespdf,
    sampdf,
    by = join_by(benchmark, model)
  ) |>
    select(-benchmark)

  if (nitems >= n_unique(resplongdf$item) - npadding) {
    nitems <- (n_unique(resplongdf$item)) - npadding
  }

  if (is.null(items) & !(samp == "test")) {
    items <- resplongdf |>
      distinct(item) |>
      slice_sample(n = nitems) |>
      pull(item)
  }
  resplongdf |>
    filter(item %in% items) |>
    long2resp() |>
    drop_na() |>
    remove_constant()
}

make_feols_df <- function(wide) {
  wide |>
    pivot_longer(-model, names_to = "item", values_to = "resp") |>
    left_join(
      wide |> pivot_longer(-model,
        names_to = "itemx",
        values_to = "otheritemscore"
      ),
      by = c("model"), relationship = "many-to-many"
    ) |>
    filter(item != itemx)
}




long2resp <- function(df, removecols = c("benchmark", "split", "med", "iter")) {
  # Check if the input is a data frame
  if (is.numeric(df$item[1])) {
    df |>
      select(-any_of(removecols)) |>
      pivot_wider(names_from = item, values_from = resp, names_prefix = "i") |>
      janitor::remove_constant()
  } else {
    df |>
      select(-any_of(removecols)) |>
      pivot_wider(names_from = item, values_from = resp) |>
      janitor::remove_constant()
  }
}
checkmtx <- function(df) {
  # Check if the input is a data frame
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }
  if ((dim(df)[2] == 3) & ("item" %in% names(df))) {
    df <- long2resp(df)
  }
  df
}


create_cortab <- function(corlong) {
  tic()
  res <- corlong |>
    group_by(item) |>
    summarize( # p1 = sum(p==1)/n(),
      # psig = sum(p<0.05)/n(),
      # nonpsig = sum(p>=0.05)/n(),
      cneg = sum(rho < 0) / n(),
      clow = sum(rho < 0.1) / n(),
      c10 = quantile(rho, 0.10, na.rm = T),
      c5 = quantile(rho, 0.05, na.rm = T),
      c1 = quantile(rho, 0.01, na.rm = T),
      c25 = quantile(rho, 0.25, na.rm = T),
      c75 = quantile(rho, 0.75, na.rm = T),
      c90 = quantile(rho, 0.90, na.rm = T),
      c95 = quantile(rho, 0.95, na.rm = T),
      c99 = quantile(rho, 0.99, na.rm = T),
      cmax = max(rho, na.rm = T),
      cmin = min(rho, na.rm = T),
      cmean = mean(rho, na.rm = T),
      cvar = var(rho, na.rm = T),
      cse = sd(rho, na.rm = T) / sqrt(n()),
      cmedse = mad(rho, na.rm = T) / sqrt(n()),
      cmed = median(rho, na.rm = T)
    )
  toc()
  res
}

combine_mtx <- function(m1, m2) {
  if (is.numeric(m1$item)) {
    m1$item <- str_c("i", m1$item)
  }
  if (is.numeric(m2$item)) {
    m2$item <- str_c("i", m2$item)
  }
  inner_join(m1, m2, by = "item")
}

### Plotting Functions -------


testInfoCompare <- function(modlist, # model1, model2,
                            theta_range = c(-4, 4),
                            title = "Parallel Tests",
                            subtitle = "Test Information Curves") {
  theta_range <- seq(theta_range[1], theta_range[2], by = .01)

  Theta <- matrix(theta_range)

  modinfo <- map_dfc(namedList(modlist), function(x) testinfo(x, Theta)) |>
    mutate(Theta = theta_range)
  # information1 <- testinfo(model1, Theta)
  # information2 <- testinfo(model2, Theta)


  p <- modinfo %>%
    gather(key, value, -Theta) %>%
    ggplot() +
    geom_line(aes(x = Theta, y = value, color = key), alpha = .75) +
    labs(
      x = expression(theta), y = expression(I(theta)),
      title = title, subtitle = subtitle
    ) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    theme(legend.title = element_blank())

  return(p)
}

## Metric Functions -------


create_sample_metadata <- function(df, b = NULL, iter = 1,
                                   col_list = list(
                                     iter = iter,
                                     nmods = n_unique(df$model),
                                     ni = ncol(df) - 1
                                   ),
                                   split = "train") {
  if (is.null(b)) {
    b <- names(df)[3] |> str_remove("_q?[0-9]+$")
  }

  col_list$benchmark <- b

  sampmeta <- df |>
    pivot_longer(-c(model), names_to = "item", values_to = "resp") |>
    group_by(item) |>
    summarize(
      n_iobs = n(),
      i_mean = mean(resp, na.rm = T),
      i_sd = sd(resp, na.rm = T),
      i_var = var(resp, na.rm = T)
    ) |>
    ungroup() |>
    mutate( # benchmark = b,
      !!!col_list
    )
  return(sampmeta)
}



get_omega <- function(df, replaceval = 0,
                      nfactors = 3,
                      cut = 0.3,
                      fm = "minres",
                      rotate = "oblimin",
                      poly = T, ...) {
  # Calculate tetrachoric correlation
  df <- checkmtx(df)
  tic(stringr::str_c("omega_", nfactors))
  om <- psych::omegah(
    df |> select(-any_of(c("model", "id"))) |>
      naniar::replace_na_with(replaceval),
    nfactors = nfactors, flip = F,
    poly = poly,
    plot = F,
    digits = 8,
    fm = fm, use = "pairwise.complete.obs",
    rotate = rotate, ...
  )
  toc()
  om
}

get_omega_loadings <- function(om, nf) {
  sl <- om$schmid$sl |>
    as_tibble(rownames = "item") |>
    clean_names()
  names(sl) <- str_c(names(sl), "_", nf)
  sl |> rename("item" = str_c("item_", nf))
}

get_omega_corlong <- function(om) {
  om$R |>
    as_tibble(rownames = "idx") |>
    pivot_longer(cols = -idx, names_to = "item", values_to = "rho")
}

get_all_omegadf <- function(df, nf = 5, ...) {
  # Check if the input is a data frame
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }
  df <- checkmtx(df)
  om <- get_omega(df, nfactors = nf, ...)
  # Calculate tetrachoric correlation
  loads <- get_omega_loadings(om, nf = nf)
  cortab <- create_cortab(get_omega_corlong(om))

  # Combine the results into a single data frame
  combine_mtx(cortab, loads)
}


get_pc_loading <- function(df, replaceval = 0,
                           nfactors = 3,
                           cut = 0.3,
                           fm = "minres",
                           rotate = "oblimin",
                           poly = T, ...) {
  # Calculate tetrachoric correlation
  df <- checkmtx(df)
  tic()
  pa <- parameters::principal_components(df |> select(-any_of(c("model", "id"))),
    n = nfactors,
    use = "pairwise.complete.obs",
    rotation = rotate, ...
  )
  toc()
  pa |>
    tibble() |>
    clean_names() |>
    rename(item = variable) |>
    pivot_longer(cols = -item, names_to = "metric", values_to = "val") |>
    mutate(metric = str_c(metric, "_", nfactors)) |>
    pivot_wider(names_from = metric, values_from = val)
}


get_alpha <- function(df, replaceval = 0) {
  # Check if the input is a data frame
  df <- checkmtx(df)
  tic()
  alpha <- psych::alpha(
    df |> select(-any_of(c("model", "id"))) |>
      naniar::replace_na_with(replaceval),
    check.keys = F,
    warnings = F,
    use = "pairwise.complete.obs"
  )
  toc()
  inner_join(alpha$item.stats |> as_tibble(rownames = "item"),
    alpha$alpha.drop |> as_tibble(rownames = "item"),
    by = "item"
  ) |>
    clean_names() |>
    rename(
      r_rest = r_cor,
      alpha_rest = raw_alpha,
      alpha_rest_std = std_alpha,
      g6_rest = g6_smc,
      snr_alpha = s_n,
      valid_n = n
    )
}

# Modified Functions from the Mokken package for single-item reliability using Mokken's
# nonparametric approach: compute the Positive Positive matrix
# @param X data matrix (persons x items)
# @param P vector with order of the items
# @param N number of persons
# @param J number of items
# @param m number of response categories
compute.PP <- function(X, P, N, J, m = 2) {
  label <- as.vector(t(outer(
    paste("P(X",
      substr(as.character(100 + (1:J)), 2, 3), ">=",
      sep = ""
    ),
    paste(1:(m - 1), ")", sep = ""), paste,
    sep = ""
  )))
  PP <- matrix(0, J * (m - 1), J * (m - 1))
  i <- 0
  j <- 0
  for (i in 1:(J - 1)) {
    for (j in (i + 1):J) {
      PP[((i - 1) * (m - 1) + 1):((i - 1) * (m - 1) + (m - 1)), ((j - 1) * (m - 1) + 1):((j - 1) * (m - 1) + (m - 1))] <-
        t(outer(X[, i], 0:(m - 2), ">")) %*% outer(X[, j], 0:(m - 2), ">") / N
    }
  }
  PP <- PP + t(PP) + kronecker(diag(J), matrix(-1, m - 1, m - 1))
  PP[PP < -.5] <- NA
  dimnames(PP) <- list(label, label)
  PP <- PP[order(P), order(P)]
  return(PP)
}


# Function to estimate the single-item reliability with Pi_ii estimated using Mokken's
# nonparametric approach
# Function to estimate MS using Mokken's nonparametric approach

singlemokken <- function(data) {
  data <- na.omit(data)
  data.0 <- data - min(data)
  J <- ncol(data)
  m <- max(data.0) + 1
  N <- nrow(data)

  P <- matrix(t(apply(outer(as.matrix(data.0), 1:(m - 1), ">=") * 1, c(2, 3), mean, na.rm = TRUE)), nrow = (m - 1) * J)
  PP <- compute.PP(data.0, P, N, J, m)
  P <- matrix(sort(P))
  off.boundary <- P > 0 & P < 1
  P <- matrix(P[off.boundary, 1])
  PP <- PP[off.boundary, off.boundary]
  km <- length(P)
  is.na.PP <- is.na(PP)
  lower.bound.PP <- P %*% t(P)
  upper.bound.PP <- outer(as.numeric(P), as.numeric(P), FUN = "pmin")
  dimnames(P) <- list(dimnames(PP)[[1]], "")

  OO <- is.na(PP)
  set.matrix <- outer(as.numeric(P), as.numeric(P), "==") * 1
  unique.cells <- which(apply(set.matrix, 1, sum) == 1)
  Type <- set.matrix
  Type[unique.cells, unique.cells] <- 0
  set.vector <- sign(apply(Type, 1, sum))
  Type <- outer(set.vector, set.vector) + outer(rep(1, km), set.vector) * 2 + outer(set.vector, rep(1, km))

  for (i in 1:km) {
    for (j in 1:km) {
      if (is.na(PP[i, j])) {
        if (Type[i, j] == 4) {
          PP[i, j] <- mean(PP[set.matrix[i, ] == 1, set.matrix[, j] == 1], na.rm = T)
        } else {
          if (Type[i, j] == 1) {
            RightN <- ifelse(any(!is.na(PP[i, j:km])), j + min(which(!is.na(PP[i, j:km]))) - 1, NA)
            RightPP <- ifelse(is.na(RightN), NA, mean(PP[set.matrix[i, ] == 1, set.matrix[, RightN] == 1], na.rm = T))
            LeftN <- ifelse(any(!is.na(PP[i, 1:j])), max(which(!is.na(PP[i, 1:j]))), NA)
            LeftPP <- ifelse(is.na(LeftN), NA, mean(PP[set.matrix[i, ] == 1, set.matrix[, LeftN] == 1], na.rm = T))
            LowerN <- UpperN <- i
            LowerPP <- UpperPP <- mean(PP[set.matrix[i, ] == 1, set.matrix[, j] == 1], na.rm = T)
          }
          if (Type[i, j] == 2) {
            LowerN <- ifelse(any(!is.na(PP[i:km, j])), i + min(which(!is.na(PP[i:km, j]))) - 1, NA)
            LowerPP <- ifelse(is.na(LowerN), NA, mean(PP[set.matrix[LowerN, ] == 1, set.matrix[, j] == 1], na.rm = T))
            UpperN <- ifelse(any(!is.na(PP[1:i, j])), max(which(!is.na(PP[1:i, j]))), NA)
            UpperPP <- ifelse(is.na(UpperN), NA, mean(PP[set.matrix[UpperN, ] == 1, set.matrix[, j] == 1], na.rm = T))
            RightN <- LeftN <- j
            RightPP <- LeftPP <- mean(PP[set.matrix[i, ] == 1, set.matrix[, j] == 1], na.rm = T)
          }
          if (Type[i, j] == 0) {
            RightN <- ifelse(any(!is.na(PP[i, j:km])), j + min(which(!is.na(PP[i, j:km]))) - 1, NA)
            RightPP <- ifelse(is.na(RightN), NA, mean(PP[set.matrix[i, ] == 1, set.matrix[, RightN] == 1], na.rm = T))
            LeftN <- ifelse(any(!is.na(PP[i, 1:j])), max(which(!is.na(PP[i, 1:j]))), NA)
            LeftPP <- ifelse(is.na(LeftN), NA, mean(PP[set.matrix[i, ] == 1, set.matrix[, LeftN] == 1], na.rm = T))
            LowerN <- ifelse(any(!is.na(PP[i:km, j])), i + min(which(!is.na(PP[i:km, j]))) - 1, NA)
            LowerPP <- ifelse(is.na(LowerN), NA, mean(PP[set.matrix[LowerN, ] == 1, set.matrix[, j] == 1], na.rm = T))
            UpperN <- ifelse(any(!is.na(PP[1:i, j])), max(which(!is.na(PP[1:i, j]))), NA)
            UpperPP <- ifelse(is.na(UpperN), NA, mean(PP[set.matrix[UpperN, ] == 1, set.matrix[, j] == 1], na.rm = T))
          }

          E17a <- ifelse(is.na(LowerN), NA, LowerPP * P[i] / P[LowerN])
          E17b <- ifelse(is.na(RightN), NA, RightPP * P[j] / P[RightN])
          E17c <- ifelse(is.na(UpperN), NA, UpperPP * P[i] / P[UpperN])
          E17d <- ifelse(is.na(LeftN), NA, LeftPP * P[j] / P[LeftN])
          E21a <- ifelse(is.na(LowerN), NA, LowerPP * (1 - P[i]) / (1 - P[LowerN]) -
            P[j] * (P[LowerN] - P[i]) / (1 - P[LowerN]))
          E21b <- ifelse(is.na(RightN), NA, RightPP * (1 - P[j]) / (1 - P[RightN]) -
            P[i] * (P[RightN] - P[j]) / (1 - P[RightN]))
          E21c <- ifelse(is.na(UpperN), NA, UpperPP * (1 - P[i]) / (1 - P[UpperN]) +
            P[j] * (P[i] - P[UpperN]) / (1 - P[UpperN]))
          E21d <- ifelse(is.na(LeftN), NA, LeftPP * (1 - P[j]) / (1 - P[LeftN]) +
            P[i] * (P[j] - P[LeftN]) / (1 - P[LeftN]))

          PP[i, j] <- mean(c(E17a, E17b, E17c, E17d, E21a, E21b, E21c, E21d), na.rm = T)
        }
      }
    }
  }
  PP[is.nan(PP)] <- 0
  PP[PP > upper.bound.PP & OO] <- upper.bound.PP[PP > upper.bound.PP & OO]
  PP[PP < lower.bound.PP & OO] <- lower.bound.PP[PP < lower.bound.PP & OO]

  # Sort the data in the original order
  PP <- PP[sort(colnames(PP)), sort(colnames(PP))]
  P <- matrix(P[order(dimnames(P)[[1]])])
  rownames(P) <- dimnames(PP)[[1]]

  num <- PP - outer(as.numeric(P), as.numeric(P))

  # Determine whether all the categories occur in the data and correct for this.
  # The first line 'find.cats' is a function that indicates which scores from a certain range occur in a given vector (x).
  # The second line applies that function to the columns of the data. The result is a list 'which.cat' with J items.
  # The third line calculates the number of ISRFs for each item.
  # The fourth line indicates where a new item starts in 'num'.

  find.cats <- function(x, range = -1000:1000) range[range %in% x]
  which.cat <- list()
  for (j in 1:J) {
    which.cat[[j]] <- find.cats(data[, j])
  }
  number.ISRFs <- sapply(which.cat, length) - 1
  position <- c(1, cumsum(number.ISRFs)) # c(1, cumsum(number.ISRFs[-1]))
  position <- position[-1]

  # per item

  item.vars <- apply(data, 2, var)
  singlemok <- rep(NA, J)
  for (j in 1:J) {
    rows <- ((position[j] - (number.ISRFs[j] - 1)):position[j])
    singlemok[j] <- sum(num[rows, rows]) / item.vars[j]
  }

  return(singlemok)
}

feols_estimations <- function(idf, get_coef_stats = F) {
  tic("feols fit")

  suppressMessages({
    feols_fit <- idf |>
      feols(
        fml = resp ~ 1 |
          itemx[[otheritemscore]]
          + model[[otheritemscore]],
        data = _,
        na.action = na.exclude,
        split = ~item,
        cluster = ~model,
        warn = F, verbose = 0, notes = F,
        panel.id = ~ model + itemx,
        nthreads = 8,
      )
  })
  fdf <- map_df(1:length(feols_fit),
    \(x) as_tibble_row(fixest::r2(feols_fit[[x]])) |>
      mutate(
        item = feols_fit[[x]]$model_info$sample$value,
        meanscores = mean((feols_fit[[x]]$scores), na.rm = T),
        meanobsr2 = mean((feols_fit[[x]]$scores)^2, na.rm = T),
        # sqcor = feols_fit[[x]]$sq.cor,
        nobs = feols_fit[[x]]$nobs
      ) |>
      bind_cols(feols_fit[[x]]$coeftable |>
        as_tibble() |> clean_names() |>
        rename(intercept = estimate)),
    .progress = T
  )
  # get_coef_stats not implemented
  if (F) {
    fdf <- fdf |> bind_cols(
      model_performance(feols_fit) |>
        bind_rows() |> clean_names() |>
        select(aic, bic, rmse, sigma)
    )
  }
  fdf
}


plan(multicore)

# install.packages(c("data.table", "fixest", "future", "future.apply"))



#' Efficiently Estimate Guttman's Lambda 6 Components
#'
#' This function performs a series of regressions required for calculating Guttman's
#' Lambda 6. For each item in a wide-format response matrix, it regresses the item
#' on all other items, using a complex fixed-effect structure with random slopes.
#'
#' The implementation is highly optimized for speed and memory efficiency by:
#' 1. Using a data.table backend.
#' 2. Avoiding the creation of a massive intermediate dataset.
#' 3. Iterating through each item and creating a smaller, temporary dataset on-the-fly.
#' 4. Leveraging the 'future' framework for parallel processing of item regressions.
#'
#' @param wide_dt A data.table (or an object coercible to one) in wide format.
#'   Must contain a 'model' column and multiple item columns.
#' @param n_workers Integer. The number of parallel workers to use. Defaults to
#'   `future::availableCores() - 1`.
#'
#' @return A data.table summarizing the regression results for each item,
#'   including R-squared values, intercept estimates, and observation counts.
#'
#' @examples
#' # --- Generate realistic sample data ---
#' set.seed(123)
#' N_MODELS <- 215
#' N_ITEMS <- 100 # Using 100 instead of 1000 for a quick example
#'
#' # Create a wide data.table
#' item_names <- paste0("item_", 1:N_ITEMS)
#' wide_data <- data.table(model = 1:N_MODELS)
#' wide_data[, (item_names) := lapply(1:N_ITEMS, \(i) rbinom(N_MODELS, 1, runif(1, 0.2, 0.8)))]
#'
#' # --- Run the estimation ---
#' # Using 2 workers for the example. Increase for real use.
#' # The first run might be slower due to package loading in new sessions.
#' estimations <- estimate_lambda6_components(wide_data, n_workers = 2)
#'
#' # --- View results ---
#' print(estimations)
#'
#' # The overall Lambda 6 value is the mean of the R-squareds
#' lambda6 <- mean(estimations$r2)
#' cat("\nEstimated Guttman's Lambda 6:", lambda6, "\n")
#'
estimate_lambda6_components <- function(wide_dt, n_workers = NULL) {
  # --- 1. Setup ---
  if (!is.data.table(wide_dt)) {
    setDT(wide_dt)
  }

  item_cols <- setdiff(names(wide_dt), "model")
  n_items <- length(item_cols)

  message(sprintf(
    "Starting estimation for %d items across %d models.",
    n_items, nrow(wide_dt)
  ))

  # Configure parallel processing
  if (is.null(n_workers)) {
    n_workers <- future::availableCores(omit = 1)
  }
  if (n_workers > 1) {
    message(sprintf("Using %d parallel workers.", n_workers))
    plan(multicore, workers = n_workers)
  } else {
    message("Using a single core (sequential processing).")
    plan(sequential)
  }

  # --- 2. Parallel Estimation using future_lapply ---
  # future_lapply will distribute 'item_cols' among the workers.
  # Each worker gets a chunk of items to process.

  # .options is used to load necessary packages on each worker.
  # future.seed = TRUE ensures reproducibility.
  # fopts <- future.options(seed = TRUE, packages = c("data.table", "fixest"))

  results_list <- future_lapply(item_cols, function(target_item) {
    # --- Per-Item Data Preparation ---
    # This is the core of the memory optimization.
    # We only create the data needed for *this specific item's* regression.

    # Identify all other items
    other_items <- setdiff(item_cols, target_item)

    # Melt only the 'other_items' columns into a long format.
    # This creates a manageable data.table with (N_items-1) * N_models rows.
    reg_data <- melt(wide_dt,
      id.vars = "model",
      measure.vars = other_items,
      variable.name = "itemx",
      value.name = "otheritemscore",
      variable.factor = FALSE
    ) # Important for performance

    # Add the dependent variable ('resp') by joining back to the original data.
    # This is an efficient data.table update-join.
    reg_data[, resp := wide_dt[reg_data, on = "model", get(target_item)]]

    # --- FEOLS Estimation ---
    # The formula remains the same as in your original code.
    # We disable threading within feols since we are parallelizing at a higher level.
    # setFixest_nthreads(1)

    fit <- feols(
      fml = resp ~ 1 | itemx[[otheritemscore]] + model[[otheritemscore]],
      data = reg_data,
      warn = FALSE,
      notes = FALSE,
      verbose = 0,
      cluster = ~model,
      panel.id = ~ model + itemx # Panel.id helps convergence
    )

    # --- Result Extraction ---
    # Extract coefficients and R-squared values efficiently.
    # Guttman's Lambda 6 is the average R-squared across all items.
    r2_vals <- as.list(fixest::r2(fit, type = "all"))
    ct <- coeftable(fit)

    # Return a single-row data.table for this item.
    data.table(
      item = target_item,
      nobs = fit$nobs,
      r2 = r2_vals$r2,
      ar2 = r2_vals$ar2,
      wr2 = r2_vals$wr2,
      war2 = r2_vals$war2,
      cor2 = r2_vals$cor2,
      wapr2 = r2_vals$wapr2,
      wpr2 = r2_vals$wpr2,
      pr2 = r2_vals$pr2,
      meanscores = fmean((fit$scores), na.rm = T),
      meanobsr2 = fmean((fit$scores)^2, na.rm = T),
      intercept = ct["(Intercept)", "Estimate"],
      intercept_se = ct["(Intercept)", "Std. Error"]
    )
  }, future.seed = TRUE, future.packages = c("data.table", "fixest"))

  # --- 3. Aggregation and Cleanup ---
  # Combine the list of data.tables from each worker into one.
  final_results <- rbindlist(results_list)

  # Shut down the parallel workers
  plan(sequential)

  message("Estimation complete.")
  return(final_results)
}






MS_est <- function(df) {
  df <- checkmtx(df)
  singlemokken(df |> select(-any_of(c("model", "id"))))

  msdf <- tryCatch(
    {
      tibble(
        MS = replace_inf(singlemokken(as.matrix(df |> select(-any_of(c("model")))))),
        item = colnames(df |> select(-any_of(c("model"))))
      )
    },
    error = function(e) {
      tibble(
        MS = NA,
        item = colnames(df |> select(-any_of(c("model"))))
      )
    }
  )
  return(msdf)
}


HiZi_est <- function(df) {
  hi <- mokken::coefH(df |> select(-any_of(c("model"))) |>
    mutate(across(everything(), as.numeric)), se = F, results = F)
  zi <- mokken::coefZ(df |> select(-any_of(c("model"))) |>
    mutate(across(everything(), as.numeric)))

  reldf <- bind_cols(
    hi$Hi |> as_tibble(rownames = "item") |> rename(Hi = value),
    zi$Zi |> as_tibble() |> rename(Zi = value)
  ) |> mutate(h = hi$H, z = zi$Z)
  return(reldf)
}

mirt_test_preds <- function(rfit, newdat) {
  mods <- rfit$moddf$model |> unique()
  items <- rfit$itdf$item |> unique()
  bench <- rfit$irtdf$benchmark[1]
  mods <- newdat$model |> unique()
  newthetas <- list()
  newitvals <- list()
  for (irmod in names(rfit$irt_models)) {
    if (is.na(rfit$irt_models[[irmod]]@Fit$AIC)) {
      next
    }
    sv <- mod2values(rfit$irt_models[[irmod]]) # translate into parameter estimation structure
    mod_new <- mirt(newdat |> select(-model), pars = sv, TOL = NaN) ## create new dummy mod that contains the new responses

    preds <- fscores(mod_new,
      response.pattern = newdat |> select(-model),
      method = "EAP",
      full.scores.SE = T,
      theta_lim = theta_range,
      max_theta = 20, QMC = T
    )
    newthetas[[irmod]] <- personfit(mod_new,
      Theta = as.matrix(preds[, 1]),
      method = "EAP",
      max_theta = 20, QMC = T
    ) |>
      tibble() |>
      mutate(
        split = T,
        model = newdat$model,
        irt = irmod,
        predicted = preds[, 1],
        se = preds[, 2],
        ni_used = n_unique(items),
        val_test_set = T,
        rxx = empirical_rxx(preds),
        n_test_models = n_unique(mods)
      )
    newitvals[[irmod]] <- itemfit(mod_new,
      fit_stats = c("infit", "X2", "G2"),
      Theta = as.matrix(preds[, 1]),
      # method = "EAP",
      max_theta = 20, QMC = T
    ) |>
      tibble() |>
      select(item, any_of(c(
        "infit", "outfit",
        "PV_Q1", "X2",
        "z.infit", "z.outfit",
        "S_X2", "Zh", "X2*", "X2*_df",
        "PV_Q1*", "G2", starts_with("RMSEA")
      ))) |>
      mutate(
        split = T,
        n_test_models = n_unique(mods)
      )
  }
  return(list(
    newth = list_rbind(newthetas),
    newit = list_rbind(newitvals)
  ))
}


mirt_fits <- function(data_wide,
                      dat_wide_test = NULL,
                      theta_range = c(-9, 9),
                      irts = c("2PL"),
                      val_test_set = F,
                      samp = "train",
                      # b = "air_bench_2024",
                      mirtlist = NULL,
                      sampmeta = NULL,
                      plots = T) {
  if (is.null(sampmeta)) {
    sampmeta <- create_sample_metadata(data_wide)
  }
  b <- sampmeta$benchmark[1]
  ni <- sampmeta$ni[1]
  i <- sampmeta$iter[1]
  nmods <- sampmeta$nmods[1]


  tic("mirt irt")
  irtdf <- list()
  itdfs <- list()
  thtdf <- list()
  moddfs <- list()
  tempirtfits <- list()



  if (!is.null(dev.list())) {
    dev.off()
  }

  mirtCluster(spec = 5)
  on.exit(mirtCluster(remove = T))
  for (irt in irts) {
    print(irt)
    tic(glue("mirt fit {irt}"))

    irtmod <- tryCatch(
      {
        md <- mirt::mirt(
          data_wide |> select(-model) |>
            mutate(across(everything(), as.numeric)),
          model = 1,
          itemtype = irt,
          accelerate = "squarem",
          optimizer = "L-BFGS-B", ## needs to be changed for non-2PLs
          technical = list(
            NCYCLES = 2000,
            theta_lim = theta_range,
            MAXQUAD = 100000
          )
        )

        # Person parameter distribution
        p1 <- personDist(md, theta_range = theta_range) +
          labs(title = glue("{b} Item {ni} Person {nmods} Map: {irt}"))

        # Item difficulty distribution
        p2 <- itemDist(md,
          theta_range = theta_range,
          shape = 17, color = "red"
        ) +
          labs(title = glue("{b} item {ni} diff: {irt}"))

        # Change item labelling if no. items > 10
        if (ni > 10) {
          p2 <- p2 +
            geom_text(aes(label = items),
              nudge_x = .75,
              color = "darkgrey", size = 2, check_overlap = T
            ) +
            theme(
              axis.text.y = element_blank(),
              panel.grid.major.y = element_blank()
            )
        }

        # Scale Characteristic Curve
        # Test information curve
        p4 <- testInfoPlot(md, theta_range = theta_range) +
          labs(title = glue("{b} test i{ni} m{nmods} info: {irt}"))

        # Conditional reliability curve
        p5 <- conRelPlot(md,
          title = glue("{b} rxx: {irt}"),
          theta_range = theta_range
        )
        if (plots) {
          p <- ggpubr::ggarrange(
            ggpubr::ggarrange(p1, p2,
              ncol = 1,
              align = "hv",
              heights = c(1, 2)
            ),
            ggpubr::ggarrange(p4,
              # p3,
              p5,
              ncol = 1,
              align = "hv",
              heights = c(1.5, 1.5)
            ),
            ncol = 2
          )
          print(show(p))
        }



        preds <- fscores(md,
          method = "EAP",
          full.scores.SE = T,
          theta_lim = theta_range,
          max_theta = 20,
          QMC = T
        )

        mo_vals <- personfit(md,
          Theta = as.matrix(preds[, 1]),
          method = "EAP",
          max_theta = 20, QMC = T
        ) |>
          tibble() |>
          mutate(
            model = data_wide$model,
            th = p1@data$value,
            predicted = preds[, 1],
            se = preds[, 2],
            irt = irt,
            benchmark = b,
            n_items = ni, iter = i,
            n_models = nmods,
            split = F,
            rxx = empirical_rxx(preds)
          )
        th_vals <- p5@data |>
          tibble() |>
          mutate(
            irt = irt,
            # split = F,
            benchmark = b,
            n_items = ni, iter = i,
            n_models = nmods
          )
        it_vals <- p2@data |>
          tibble() |>
          mutate(
            irt = irt, split = F,
            benchmark = b,
            n_items = ni, iter = i,
            n_models = nmods
          ) |>
          rename(
            item = items, a = items.a, b = items.b,
            g = items.g, u = items.u
          ) |>
          left_join(
            itemfit(md,
              fit_stats = c("infit", "X2", "G2", "Zh"),
              method = "EAP", Theta = as.matrix(preds[, 1]),
              QMC = T
            ) |>
              tibble() %>%
              select(item, any_of(c(
                "infit", "outfit",
                "PV_Q1", "X2",
                "z.infit", "z.outfit",
                "S_X2", "Zh", "X2*", "X2*_df",
                "PV_Q1*", "G2", starts_with("RMSEA")
              ))),
            by = "item"
          )


        list(
          irtmod = md,
          th_vals = th_vals,
          it_vals = it_vals,
          mo_vals = mo_vals
        )
      },
      error = function(e) {
        th_vals <- tibble(
          value = NA, irt = irt, benchmark = b,
          n_items = ni, iter = i,
          n_models = nmods
        )
        it_vals <- tibble(
          item = colnames(data_wide |> select(-model)),
          a = NA, b = NA,
          g = NA, u = NA, irt = irt, benchmark = b, iter = i,
          ni_used = ncol(data_wide) - 1,
          n_items = ni,
          n_models = nmods
        )
        mo_vals <- tibble(
          model = data_wide$model,
          th = NA,
          irt = irt,
        )
        return(list(
          irtmod = NULL,
          th_vals = th_vals,
          it_vals = it_vals,
          mo_vals = mo_vals
        ))
      }
    )

    toc()
    thtdf[[irt]] <- irtmod$th_vals
    itdfs[[irt]] <- irtmod$it_vals
    moddfs[[irt]] <- irtmod$mo_vals
    tempirtfits[[irt]] <- irtmod$irtmod
  }
  tempirtfits <- tempirtfits[!sapply(tempirtfits, is.null)]

  irtdf <- list()
  if (length(tempirtfits) >= 2) {
    for (ii in 1:(length(tempirtfits) - 1)) {
      for (jj in (ii + 1):length(tempirtfits)) {
        if (ii != jj) {
          irtdf[[length(irtdf) + 1]] <- tryCatch(
            {
              mirt::anova(tempirtfits[[ii]], tempirtfits[[jj]]) |>
                tibble() |>
                mutate(
                  benchmark = b,
                  irt = c(
                    names(tempirtfits)[ii],
                    names(tempirtfits)[jj]
                  ),
                  irt1 = names(tempirtfits)[ii],
                  irt2 = names(tempirtfits)[jj],
                  n_items = ni,
                  iter = i,
                  n_models = nmods
                )
            },
            error = function(e) {
              print(glue::glue("Cannot compare {names(tempirtfits)[ii]} and {names(tempirtfits)[jj]}"))
              tibble()
            }
          )
        }
      }
    }
  }
  if (length(irtdf) > 0) {
    irtdf <- irtdf |> bind_rows()
  } else {
    irtdf <- tibble()
  }

  return(list(
    irt_models = tempirtfits,
    irtdf = irtdf,
    thtdf = list_rbind(thtdf, names_to = "irt"),
    itdf = list_rbind(itdfs, names_to = "irt"),
    moddf = list_rbind(moddfs, names_to = "irt")
  ))
}



get_bonus_metrics <- function(df) {
  df |>
    select(-any_of(c("model"))) |>
    item_similarity_enhanced()
}


recover_bonus_metrics_from_irdf <- function(aggdf, hilo_splits, irdf) {
  bonus_metric_names <- names(item_similarity_enhanced(matrix(c(1, 1, 0, 1), nrow = 2)))[-1]
  obsdf <- aggdf |>
    semi_join(hilo_splits |> filter(split == "train") |> select(model, benchmark), by = c("model", "benchmark")) |>
    semi_join(irdf |> select(item, benchmark), by = c("item", "benchmark"))

  L <- list()
  for (it in irdf$iter |> unique()) {
    L[[as.character(it)]] <- obsdf |>
      semi_join(irdf |> filter(iter == it), by = c("item")) |>
      pivot_wider(names_from = item, values_from = resp) |>
      select(-any_of(c("benchmark", "model"))) |>
      item_similarity_enhanced()
  }

  bonus_metrics <- list_rbind(L, names_to = "iter") |> inner_join(
    irdf |> select(-any_of(bonus_metric_names)),
    by = c("item", "iter")
  )

  return(bonus_metrics)
}


recover_bonus_metrics <- function(rfit) {
  rfit$irt_models.2PL@Data$data |> item_similarity_enhanced()
}

recover_irt_metrics <- function(rfit, theta_range = c(-9, 9)) {
  itemDist(rfit$irt_models.2PL, theta_range = theta_range)@data |>
    tibble() |>
    rename(
      item = items, a = items.a, b = items.b,
      g = items.g, u = items.u
    ) |>
    left_join(
      itemfit(rfit$irt_models.2PL,
        fit_stats = c("infit", "X2", "G2", "Zh"),
        method = "EAP", theta_lim = theta_range,
        max_theta = 20,
        QMC = T
      ) |>
        tibble() %>%
        select(item, any_of(c(
          "infit", "outfit",
          "PV_Q1", "X2",
          "z.infit", "z.outfit",
          "S_X2", "Zh", "X2*", "X2*_df",
          "PV_Q1*", "G2", starts_with("RMSEA")
        ))),
      by = "item"
    )
}


make_gstudy_df <- function(dat, verbose = F) {
  if (fnunique(dat$item) < 20) {
    optimizer <- "Nelder_Mead"
  } else {
    optimizer <- "bobyqa"
  }

  b <- dat$benchmark[1]
  if (verbose) {
    print(glue::glue("G-study for benchmark {b} with {n_unique(dat$item)} items and {n_unique(dat$model)} models"))
  }
  mod <- NULL
  mod <- try(dat |>
    lmer(
      formula = resp ~ 1
        + (1 | item)
        + (1 | model),
      na.action = na.exclude,
      control = lmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 1e5),
        calc.derivs = F
      )
    ), silent = T)
  # print("G-study model fit complete")
  if (inherits(mod, "try-error")) {
    return(tibble(
      benchmark = b,
    ))
  }
  if (verbose) {
    print(summary(mod))
  }

  modvars <- mod |>
    VarCorr() |>
    janitor::clean_names() |>
    as_tibble() |>
    select(-var2, -var1, -sdcor) |>
    mutate(benchmark = b) |>
    pivot_wider(
      names_from = grp,
      values_from = vcov
    ) |>
    mutate(
      m_rel = model / (model + Residual),
      i_rel = item / (item + Residual),
      m_dep = model / (item + model + Residual),
      i_dep = item / (item + model + Residual),
      m_snr = m_rel / (1 - m_rel),
      i_snr = i_rel / (1 - i_rel),
      md_snr = m_dep / (1 - m_dep),
      id_snr = i_dep / (1 - i_dep)
    )
  return(modvars)
}



make_dstudy_df <- function(benchdf, # aggdf,
                           item_samples) {
  tic("G-D study fits")
  rankrefits <- list()
  for (b in unique(benchdf$benchmark)) {
    isdf <- item_samples |>
      filter(benchmark == b) |>
      select(item, metric) |>
      distinct()
    bdf <- benchdf |>
      semi_join(isdf, by = "item")
    print("Starting metric: ")
    print(b)
    rankrefits[[b]] <- future_map(funique(isdf$metric),
      \(m) make_gstudy_df(
        dat = bdf |>
          merge(isdf |> fsubset(metric == m),
            by = "item"
          ) # |>
        , verbose = F
      ) |> fmutate(metric = m),

      # distinct()
      .progress = T,
      .options = furrr::furrr_options(
        seed = TRUE,
        conditions = structure("condition",
          exclude = "message"
        )
      )
    )
    rankrefits[[b]] <- list_rbind(rankrefits[[b]])
  }
  toc()
  list_rbind(rankrefits)
}


cumulative_kendall_by_metric <- function(
    benched,
    item_samples,
    max_items = 500
) {
  setDT(benched)
  setDT(item_samples)

  benched <- benched[, .(benchmark, item, model, samp, resp, average)]
  item_samples <- item_samples[, .(benchmark, item, metric, estimate)]

  # Split item_samples by metric (lightweight: list of pointers)
  metric_groups <- split(item_samples, by = "metric", keep.by = TRUE)

  result_list <- vector("list", length(metric_groups))
  i <- 1L

  result_list <- future_lapply(
    metric_groups,
    FUN = function(isamp) {
      # same per-metric code
    # Join ONLY this metric
    dt <- benched[
      isamp,
      on = .(benchmark, item),
      nomatch = 0L
    ]

    # Order items by estimate
    setorder(dt, benchmark, -estimate)
    
    # Limit to max_items if specified
    if (!is.null(max_items) && max_items > 0) {
      dt <- dt[, head(.SD, max_items), by = .(benchmark, model, samp)]
    }

    # Global prefix index
    dt[, n_items := seq_len(.N), by = .(benchmark, model, samp)]

    # Cumulative mean per model & sample
    dt[, cum_mean := cumsum(resp) / seq_len(.N),
       by = .(benchmark, model, samp)]
    # Kendall τ vs overall average
      dt[
      ,
      .(
        kcor = suppressWarnings(
          cor.fk(cum_mean, average)
        )
      ),
      by = .(benchmark, metric, samp, n_items)
    ]
  }, future.seed = TRUE,
  future.globals = list(
    benched = benched
  ),
  future.packages = c("data.table", "pcaPP"))
  rbindlist(result_list, use.names = TRUE)
}
# Bootstraps ------
## Bootstrap Setup ---------

benchmarks <- aggdf$benchmark |> unique()
hilo_splits <- make_hilo_splits(aggdf)
relfits <- list()

rerunbootstraps <- F
rerunadjustment <- F
rerunitemsamples <- F
rerungstudies <- F


n_boots <- 1:10
ni_vals <- 1:200
recordno <- 1


if (F) {
  hilo_splits |> ggplot(aes(x = resp, fill = split)) +
    geom_histogram() +
    theme_minimal() +
    facet_wrap(~benchmark, scales = "free", nrow = 3) +
    labs(
      title = "Score Distributions by Benchmark and Split",
      x = "Percent Correct", y = "Count"
    ) +
    theme(text = element_text(family = "times", size = 13)) +
    ggthemes::scale_fill_colorblind()
}

## Bootstrap loop ---------
if (rerunbootstraps) {
  tic("Bootstrap Loop")
  for (b in benchmarks) {
    for (ni in ni_vals) {
      if (ni > sum(item_counts |>
        filter(benchmark == b) |>
        pull(n))) {
        print(glue::glue("Skipping {b} for ni {ni} - not enough items"))
        next
      }
      for (i in 1:n_boots) {
        for (ordered_models in c(T)) {
          print(glue::glue("Bootstrap {i} - Items {ni} - Benchmark {b}"))
          tic("Total Iteration Time")

          # Load Data
          data_wide <- make_sample_response(
            hilo_splits,
            aggdf,
            b = b,
            nitems = ni,
            samp = "train",
          ) |> as.data.frame()

          reldf <- create_sample_metadata(data_wide,
            iter = length(relfits) + 1
          ) |>
            mutate(boot = i)

          # # IRT Modeling
          mirt_results <- mirt_fits(
            data_wide = data_wide,
            theta_range = c(-9, 9),
            irts = c("2PL"),
            val_test_set = F,
            samp = "train",
            sampmeta = reldf,
            plots = F
          )

          if (!is.null(mirt_results$itdf)) {
            reldf <- reldf |> left_join(
              mirt_results$itdf |>
                filter(irt == "2PL"),
              by = c("item")
            )
          }

          tic("faster estimations")
          reldf <- reldf |> left_join(HiZi_est(data_wide), by = "item")

          reldf <- reldf |> left_join(estimate_lambda6_components(data_wide), by = "item")

          reldf <- reldf |> left_join(
            create_cortab(cor(data_wide |>
              select(-model)) |>
              as_tibble(rownames = "item") |>
              pivot_longer(-item,
                names_to = "itemx",
                values_to = "rho"
              )),
            by = "item"
          )

          # reldf = reldf |> left_join(MS_est(data_wide),by="item")

          if (exists("item_similarity_enhanced")) {
            reldf <- reldf |> left_join(get_bonus_metrics(data_wide |> select(-model)), by = "item")
          }
          toc()

          tic("alpha")
          reldf <- suppressMessages(
            tryCatch(
              {
                reldf |> left_join(get_alpha(data_wide), by = "item")
              },
              error = function(e) {
                reldf
              }
            )
          )
          toc()
          # tic("Guttman, Omega, and PCA")
          # reldf = reldf |> left_join(get_all_omegadf(data_wide,nf=3),by="item")
          # reldf = reldf |> left_join(get_pc_loading(data_wide,nfactors=3),by="item")
          # toc()
          # Combine reliability fit statistics

          relfits[[length(relfits) + 1]] <- c(
            list(
              reldf = reldf
            ),
            irtdf = mirt_results$irtdf,
            thtdf = mirt_results$thtdf |> bind_rows(),
            irt_models = mirt_results$irt_models,
            itdf = mirt_results$itdfs |> bind_rows(),
            moddf = mirt_results$moddfs |> bind_rows()
          )
          toc()



          gc()
          toc()
        }
      }
      saveRDS(relfits, here::here(
        "benchmark_reliability", "data", "results",
        "boots", glue::glue("itemfit_rel_temp_boot{recordno}_{today()}.rds")
      ))
    }
  }
  toc()

  # Combine Results into Item Reliability df ------

  tic("Combine Results")
  irdf <- map(relfits,
    \(x) x$reldf |>
      select(-ends_with(".y")) |>
      rename_with(\(x) str_remove(x, ".x")), # |>
    .progress = T
  ) |>
    list_rbind(names_to = "iter_grp") |>
    mutate(
      CA = (r_rest)^2 / alpha_rest,
      across(
        any_of(c(
          "cor2", "ar2", "r2", "pr2", "apr2",
          "wr2", "war2", "wpr2", "wapr2"
        )),
        \(x) 1 - (1 - x) / i_var,
        .names = "{.col}_pgl6"
      ), # pseudo GL6
      DI_g6 = (r_rest)^2 / g6_rest,
      DI_std = (r_rest)^2 / alpha_rest_std,
    ) |>
    rename(
      L6 = r2, D_L6 = intercept,
      itcor = raw_r,
      itcor_std = std_r,
      ctt_disc = r_rest, ## corrected item-total correlation
      obsr2 = meanobsr2
    ) |>
    replace_inf() |>
    select(-any_of(c("aic", "bic", "rmse", "sigma", "irt"))) |>
    distinct() |>
    group_by(iter, iter_grp, benchmark) |>
    mutate( # n_items = n_distinct(item),
      nvali = n_distinct(item, na.rm = T) - sum(is.na(cor2))
    ) |>
    ungroup() |>
    mutate(
      n_valmods = round(nobs / ni),
      n_models = if_else(is.na(nmods), n_valmods, nmods),
      valid_item = !is.na(cor2)
    ) |>
    group_by(item) |>
    mutate(countvalid = sum(valid_item), split = "train") |>
    ungroup() |>
    select(
      iter, benchmark, item, iter_grp,
      n_iobs:ni, any_of(c(
        "nobs", "valid_n", "n_valmods", "irt",
        "n_models", "boot", "h", "z", "split", "samp"
      )),
      nvali:countvalid,
      L6, CA,
      # G2,D,itcor,
      everything(), -any_of(c("aic", "bic", "rmse", "sigma", "irt"))
    ) |>
    distinct() |>
    mutate(iter = glue("{ifelse(ordered_models,'hilo','rand')}_long{recordno}_{iter}")) |>
    select(iter, everything()) |>
    bind_rows(readRDS(here("benchmark_reliability/data/results/boots/item_rel_boot_2026-01-20.rds")))

  saveRDS(irdf, here::here(
    "benchmark_reliability", "data", "results", "boots",
    glue::glue("item_rel_boot_{today()}.rds")
  ))
  gc()
  toc()
} else {
  irdf <- readRDS(here("benchmark_reliability/data/results/boots/item_rel_boot_2026-01-20.rds"))
}

#
# Adjust restuls from bootstrap ---------
rerunadjustment <- T
if (rerunadjustment) {
  traindiff <- aggdf |>
    semi_join(hilo_splits |> filter(split == "train") |> select(model, benchmark)) |>
    fgroup_by(benchmark, item) |>
    fsummarise(diff = 1 - fmean(resp)) |>
    fungroup()

  metrics_for_study <- c(
    c(names(irdf |> select(L6:misonmI))),
    "L6", "MS", "CA", "Hi", "Zi", "a",
    "guttman_lambda2", "guttman_lambda4", "guttman_lambda5", "guttman_lambda6",
    "forbes_adj", "hardy_frba", "snr_alpha", "ctt_disc", "g6_rest", "alpha_rest",
    "G2",
    "itcor",
    "average_r",
    "cralpha", "cralpha_rv",
    "diff", "diff_raw", "comp", "dalf"
  )

  rand_metrics <- c("rand", glue("rand{1:10}"))


  tic("Adjust Results")

  tic("setting up for adjustment")
  irdf_filtered <- irdf |>
    mutate(
      uniq_L6 = 1 - L6,
      L6_qual = uniq_L6 * L6,
      L6_war_qual = L6 * (1 - war2_pgl6),
      alph_qual = snr_alpha * (1 - cralpha),
    ) |>
    select(
      iter, benchmark, item, iter_grp,
      n_iobs, nmods:ni, any_of(c(
        "nobs", "valid_n", "n_valmods", "irt",
        "n_models", "boot", "h", "z", "split", "samp"
      )),
      boot, h, z, nvali:countvalid,
      L6, CA, any_of(metrics_for_study),
    ) |>
    group_by(item) |>
    mutate(countvalid = sum(valid_item)) |>
    ungroup() |>
    filter(countvalid > 2) |>
    as.data.table() |>
    remove_constant()
  toc()
  tic("overall adjustment")
  asdf <- irdf_filtered %>%
    feols(
      fml = as.formula(paste0(
        "c(", paste0(
          names(irdf_filtered |>
            select(any_of(metrics_for_study))),
          collapse = ","
        ),
        ") ~ 1 | iter^boot^benchmark"
      )),
      panel.id = ~ item + iter,
      data = .
    ) |>
    fixest:::residuals.fixest_multi(na.rm = F)
  toc()
  asdf <- as.data.frame(asdf)

  names(asdf) <- names(irdf_filtered |> select(any_of(metrics_for_study)))

  adf <- irdf_filtered |>
    select(-(any_of(metrics_for_study))) |>
    bind_cols(asdf) |>
    pivot_longer(any_of(metrics_for_study), names_to = "metric", values_to = "est") |>
    group_by(benchmark, item, metric) |>
    summarise(est = mean(est, na.rm = T)) |>
    ungroup() |>
    pivot_wider(names_from = metric, values_from = est)




  adf <- adf |>
    mutate(
      rand = runif(nrow(adf)),
      rand2 = runif(nrow(adf)),
      rand3 = runif(nrow(adf)),
      rand4 = runif(nrow(adf)),
      rand5 = runif(nrow(adf)),
      rand6 = runif(nrow(adf))
    ) |>
    left_join(traindiff, # |> # rename(diff_raw = diff),
      by = c("benchmark", "item")
    )
  gc()
  toc()
  adf |> saveRDS(here::here(
    "benchmark_reliability/data/results/boots",
    glue::glue("item_agg_adj_full_{today()}.rds")
  ))
} else {
  adf <- readRDS(here::here("benchmark_reliability/data/results/boots", "item_agg_adj_reduced_2026-01-22.rds")) # "item_agg_adj_full_2026-01-22.rds"
}


rev_code_metrics <- c(
  "outfit", "infit",
  # "z.outfit",
  "z.infit",
  "clow", "average_r",
  "cneg", "cse", "cvar",
  "std_error", "obsr2", "war2_pgl6", "pr_t", "t_value",
  "b",
  "f1", "jac",
  "cralpha",
  "star_kap",
  "Star_f1", "Star_j", "hardy_f1", "star_P4", "hardy_j",
  "star_taub", "hardy_tau",
  "sorgen_star",
  "pmi", "pmi2",
  "Zh", "del_alpha"
)

item_samples_base <- adf |>
  group_by(benchmark) |>
  mutate( # z.up.fit =  z.infit - z.outfit,
    # diff = 1-i_mean,
    dalf = (dense_rank(diff) + dense_rank(L6) +
      dense_rank(a) + dense_rank(forbes_adj)) / (4 * n()),
    comp = (dense_rank(L6) + dense_rank(Hi) +
      dense_rank(a) + dense_rank(-cralpha) # switch lines if mirt models estimated
    # dense_rank(a1_approx_cov)
    ) / (4 * n())
  ) |>
  ungroup() |>
  pivot_longer(cols = any_of(c(metrics_for_study, rand_metrics)), names_to = "metric", values_to = "est") |>
  mutate(
    est = if_else(metric %in% c(
      "outfit", "infit" # ,"z.infit" "z.outfit",
    ), abs(est), est),
    est = if_else(metric %in% rev_code_metrics, 1 - est, est),
    metric = if_else(metric %in% rev_code_metrics, str_c(metric, "_rv"), metric)
  )


metrics_for_study <- c(
  "L6", "MS", "CA", "Hi", "Zi", "a",
  "snr_alpha", "ctt_disc",
  "G2",
  "itcor",
  "cralpha_rv",
  "diff"
)


nis <- c(1:100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 
         210, 220, 230, 240, 300, 350, 400, 450, 500)
rerunitemsamples=T
for(ni in nis){
if(rerunitemsamples){
  nitems_for_gstudy = ni
  print(glue::glue("Preparing item samples for n={nitems_for_gstudy}"))
  tic(glue::glue("n={nitems_for_gstudy}"))
  validbenches = item_counts |> 
      filter(n > nitems_for_gstudy) |> 
      pull(benchmark)
  item_samples = item_samples_base |>
    filter(metric %in% c(metrics_for_study,rand_metrics)) |>
    filter(benchmark %in% validbenches) |>
    slice_max(order_by = est, 
              by = c(benchmark,metric),
              n = nitems_for_gstudy) |> 
    select(benchmark,item,metric) |> distinct()
  
  benchdf = aggdf |> 
    semi_join(item_samples, by = c("benchmark","item")) |> 
    semi_join(hilo_splits |> 
                filter(split=="test") |> 
                select(benchmark,model) |> 
                distinct(),
              by=c("benchmark","model")) |> drop_na()
  benchdf_train = aggdf |> 
    semi_join(item_samples, by = c("benchmark","item")) |> 
    semi_join(hilo_splits |> 
                filter(split=="train") |> 
                select(benchmark,model) |> 
                distinct(),
              by=c("benchmark","model")) |> drop_na()
  
  gc()
}

## Run generalizability studies -------

rerungstudies = T
if(rerungstudies){
  gc()
  tic()
  dstudydf = make_dstudy_df(benchdf,item_samples) |> mutate(samp="test")
  dstudydf_train = make_dstudy_df(benchdf_train,item_samples) |> mutate(samp="train")
  dstudydf = bind_rows(dstudydf, dstudydf_train)
  dstudydf |> saveRDS(here::here("benchmark_reliability/data/results/boots", 
                                 glue::glue("item_inc_gt3val_gdstudydf_reduce_metrics_n{ni}_{today()}.rds")))
  gc()
  toc()
  } else {
  dstudydf = readRDS(here("benchmark_reliability/data/results/boots/item_inc_gt3val_gdstudydf_n90_2026-01-10"))
  }
toc()
}



tic()
corrstudydf = cumulative_kendall_by_metric(benchdf  |> left_join(hilo_splits |> select(model,benchmark,resp,split) |> rename(average = resp,samp=split) |> distinct(),by=c("model","benchmark")),item_samples_base |> filter(metric %in% c(metrics_for_study,rand_metrics)) |> rename(estimate = est),max_items = 500) 
toc()

tic()
corrstudydf = bind_rows(corrstudydf,  cumulative_kendall_by_metric(benchdf_train  |> left_join(hilo_splits |> select(model,benchmark,resp,split) |> rename(average = resp,samp=split) |> distinct(),by=c("model","benchmark")),item_samples_base|> filter(metric %in% c(metrics_for_study,rand_metrics))  |> rename(estimate = est),max_items = 500 ))
toc()
corrstudydf |> saveRDS(here::here("benchmark_reliability/data/results/boots", 
                                  glue::glue("item_inc_gt3val_corrtudydf_reduce_metrics_Reduced_{today()}.rds")))
gc()


# randomly selected models replication ------
tic()

set.seed(12345)

benchranddf = aggdf |> 
  semi_join(item_samples_base, by = c("benchmark","item")) |> 
  inner_join(hilo_splits |> slice_sample(prop = 0.5, by = "benchmark") |> 
               select(benchmark,model,resp) |> rename(average = resp) |> distinct(),
            by = c("model","benchmark")) |> drop_na() |> fmutate(samp = "rand")
toc()


tic()
corrstudydf_rand = cumulative_kendall_by_metric(benchranddf ,item_samples_base |> rename(estimate = est),max_items = 500)
toc()



# Analyze results --------
# Plot set up -------
## Aggregation of saved dstudy results -------


modcounts = hilo_splits |> select(benchmark,split) |> group_by(benchmark) |> summarise(ntrain = sum(split=="train"), ntest = sum(split=="test")) |> ungroup()


mtx_to_display = c("L6",#"obsr2_rv",
                   "Hi", "MS",
                   "cralpha_rv",
                   "uniq_alph" ,
                   "diff",
                   "diff_raw",
                   "snr_alpha",
                   "war2_pgl6_rv","CA",
                   "a") #,"comp"


file_list = list.files(here::here("benchmark_reliability/data/results/boots/"),
                       pattern = "item_inc_gt[23]val_gdstudydf_reduce_metrics_n", # final. Here are more from earlier, which do not have correct "rand" and do not have both train and test sets run on them "item_inc_gt[23]val_gdstudydf_n",
                       full.names = T)
all_dstudydf = list()
for(f in file_list){
  # print(f)
  nitems = str_extract(f,"_n[0-9]+_") |> str_remove_all("_n|_")
  # print(glue::glue("Number of items: {nitems}"))
  all_dstudydf[[length(all_dstudydf)+1]] = readRDS(f) |> mutate(n_items = as.integer(nitems))
}
all_dstudydf = list_rbind(all_dstudydf)
gc()