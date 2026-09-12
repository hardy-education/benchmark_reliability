# ==============================================================================
# 04b_mokken_single.R -- Molenaar-Sijtsma single-item reliability via Mokken's
#                        nonparametric approach.
#
# Adapted from the `mokken` package (van der Ark), which implements the
# Molenaar-Sijtsma method; see App. D.8.2 and Mokken (2011), Sijtsma & Molenaar
# (2002). The estimator needs the matrix of joint "positive-positive"
# probabilities Pi_ij between item step response functions, of which the
# diagonal blocks Pi_ii are unobservable and must be imputed by interpolation
# from neighbouring cells.
#
# This file is kept separate from R/04_item_metrics.R because it is adapted
# third-party numerical code rather than something written for this paper. It is
# reproduced here with its arithmetic untouched -- the refactor is limited to
# naming, dead-code removal, input guards and commentary, and
# tests/testthat/test-mokken.R checks it against the original implementation
# cell for cell.
# ==============================================================================

#' Matrix of joint positive-positive probabilities between item steps
#'
#' For every pair of distinct items (i, j) and every pair of item-step response
#' functions, the proportion of respondents exceeding both steps. Diagonal
#' blocks are set to -1 as a sentinel and then converted to NA, since a step
#' cannot be jointly observed with itself.
#'
#' @param X Integer response matrix (persons x items), scores starting at 0.
#' @param P Vector giving the ordering of the item steps.
#' @param N Number of persons.
#' @param J Number of items.
#' @param m Number of response categories.
#' @return A `J*(m-1)` square matrix, reordered by `P`, with NA on the
#'   unobservable diagonal blocks.
compute_pp <- function(X, P, N, J, m = 2) {
  if (J < 2L) stop("compute_pp() needs at least 2 items.")

  step_labels <- as.vector(t(outer(
    paste("P(X", substr(as.character(100 + seq_len(J)), 2, 3), ">=", sep = ""),
    paste(seq_len(m - 1), ")", sep = ""),
    paste, sep = ""
  )))

  n_steps <- J * (m - 1)
  PP <- matrix(0, n_steps, n_steps)

  # Upper triangle of item-pair blocks: proportion exceeding both steps.
  for (i in seq_len(J - 1L)) {
    for (j in (i + 1L):J) {
      rows <- ((i - 1L) * (m - 1L) + 1L):((i - 1L) * (m - 1L) + (m - 1L))
      cols <- ((j - 1L) * (m - 1L) + 1L):((j - 1L) * (m - 1L) + (m - 1L))
      PP[rows, cols] <-
        t(outer(X[, i], 0:(m - 2L), ">")) %*% outer(X[, j], 0:(m - 2L), ">") / N
    }
  }

  # Symmetrise, then mark the within-item diagonal blocks as unobservable.
  PP <- PP + t(PP) + kronecker(diag(J), matrix(-1, m - 1L, m - 1L))
  PP[PP < -0.5] <- NA
  dimnames(PP) <- list(step_labels, step_labels)
  PP[order(P), order(P)]
}

#' Molenaar-Sijtsma single-item reliability
#'
#' Estimates the reliability of each item separately by imputing the
#' unobservable within-item joint probabilities Pi_ii from observable
#' neighbouring cells, then forming, per item, the sum of imputed covariances
#' over the item's observed variance.
#'
#' The imputation distinguishes four neighbourhood types (`Type` 0, 1, 2, 4),
#' which encode whether the row step, the column step, both, or neither are
#' shared with other steps of equal probability. Missing cells are filled with
#' the average of eight interpolation estimates -- four from Mokken's Eq. (17)
#' and four from Eq. (21) -- and then clipped to the Pi bounds
#' `P_i P_j <= Pi_ij <= min(P_i, P_j)`.
#'
#' @param data Numeric response matrix (persons x items).
#' @return Numeric vector of per-item reliability estimates, in column order.
single_mokken <- function(data) {
  data <- stats::na.omit(data)
  if (ncol(data) < 2L) stop("single_mokken() needs at least 2 items.")

  data_0 <- data - min(data)
  J <- ncol(data)
  m <- max(data_0) + 1L
  N <- nrow(data)

  # Marginal step proportions P(X >= s), stacked item by item.
  P <- matrix(
    t(apply(outer(as.matrix(data_0), seq_len(m - 1L), ">=") * 1, c(2, 3), mean, na.rm = TRUE)),
    nrow = (m - 1L) * J
  )
  PP <- compute_pp(data_0, P, N, J, m)

  # Drop steps at the boundary (nobody or everybody passes), which carry no
  # information and would divide by zero below.
  P <- matrix(sort(P))
  off_boundary <- P > 0 & P < 1
  P  <- matrix(P[off_boundary, 1])
  PP <- PP[off_boundary, off_boundary]
  km <- length(P)
  dimnames(P) <- list(dimnames(PP)[[1]], "")

  lower_bound_pp <- P %*% t(P)
  upper_bound_pp <- outer(as.numeric(P), as.numeric(P), FUN = "pmin")
  was_missing    <- is.na(PP)

  # `set_matrix[i, j] == 1` when steps i and j have identical proportions, so
  # cells in the same set are interchangeable donors for interpolation.
  set_matrix    <- outer(as.numeric(P), as.numeric(P), "==") * 1
  unique_cells  <- which(rowSums(set_matrix) == 1)
  type_base     <- set_matrix
  type_base[unique_cells, unique_cells] <- 0
  set_vector    <- sign(rowSums(type_base))
  Type <- outer(set_vector, set_vector) +
    outer(rep(1, km), set_vector) * 2 +
    outer(set_vector, rep(1, km))

  for (i in seq_len(km)) {
    for (j in seq_len(km)) {
      if (!is.na(PP[i, j])) next

      if (Type[i, j] == 4) {
        # Both steps belong to equal-probability sets: average the donors.
        PP[i, j] <- mean(PP[set_matrix[i, ] == 1, set_matrix[, j] == 1], na.rm = TRUE)
        next
      }

      # Locate the nearest observed cell in each of the four directions.
      if (Type[i, j] == 1) {
        RightN  <- if (any(!is.na(PP[i, j:km]))) j + min(which(!is.na(PP[i, j:km]))) - 1L else NA
        RightPP <- if (is.na(RightN)) NA else mean(PP[set_matrix[i, ] == 1, set_matrix[, RightN] == 1], na.rm = TRUE)
        LeftN   <- if (any(!is.na(PP[i, 1:j]))) max(which(!is.na(PP[i, 1:j]))) else NA
        LeftPP  <- if (is.na(LeftN)) NA else mean(PP[set_matrix[i, ] == 1, set_matrix[, LeftN] == 1], na.rm = TRUE)
        LowerN  <- UpperN  <- i
        LowerPP <- UpperPP <- mean(PP[set_matrix[i, ] == 1, set_matrix[, j] == 1], na.rm = TRUE)
      } else if (Type[i, j] == 2) {
        LowerN  <- if (any(!is.na(PP[i:km, j]))) i + min(which(!is.na(PP[i:km, j]))) - 1L else NA
        LowerPP <- if (is.na(LowerN)) NA else mean(PP[set_matrix[LowerN, ] == 1, set_matrix[, j] == 1], na.rm = TRUE)
        UpperN  <- if (any(!is.na(PP[1:i, j]))) max(which(!is.na(PP[1:i, j]))) else NA
        UpperPP <- if (is.na(UpperN)) NA else mean(PP[set_matrix[UpperN, ] == 1, set_matrix[, j] == 1], na.rm = TRUE)
        RightN  <- LeftN  <- j
        RightPP <- LeftPP <- mean(PP[set_matrix[i, ] == 1, set_matrix[, j] == 1], na.rm = TRUE)
      } else {  # Type == 0: neither step is shared
        RightN  <- if (any(!is.na(PP[i, j:km]))) j + min(which(!is.na(PP[i, j:km]))) - 1L else NA
        RightPP <- if (is.na(RightN)) NA else mean(PP[set_matrix[i, ] == 1, set_matrix[, RightN] == 1], na.rm = TRUE)
        LeftN   <- if (any(!is.na(PP[i, 1:j]))) max(which(!is.na(PP[i, 1:j]))) else NA
        LeftPP  <- if (is.na(LeftN)) NA else mean(PP[set_matrix[i, ] == 1, set_matrix[, LeftN] == 1], na.rm = TRUE)
        LowerN  <- if (any(!is.na(PP[i:km, j]))) i + min(which(!is.na(PP[i:km, j]))) - 1L else NA
        LowerPP <- if (is.na(LowerN)) NA else mean(PP[set_matrix[LowerN, ] == 1, set_matrix[, j] == 1], na.rm = TRUE)
        UpperN  <- if (any(!is.na(PP[1:i, j]))) max(which(!is.na(PP[1:i, j]))) else NA
        UpperPP <- if (is.na(UpperN)) NA else mean(PP[set_matrix[UpperN, ] == 1, set_matrix[, j] == 1], na.rm = TRUE)
      }

      # Mokken Eq. (17): scale the donor by the ratio of step proportions.
      E17a <- if (is.na(LowerN)) NA else LowerPP * P[i] / P[LowerN]
      E17b <- if (is.na(RightN)) NA else RightPP * P[j] / P[RightN]
      E17c <- if (is.na(UpperN)) NA else UpperPP * P[i] / P[UpperN]
      E17d <- if (is.na(LeftN))  NA else LeftPP  * P[j] / P[LeftN]

      # Mokken Eq. (21): the complementary form, scaled by 1 - P.
      E21a <- if (is.na(LowerN)) NA else
        LowerPP * (1 - P[i]) / (1 - P[LowerN]) - P[j] * (P[LowerN] - P[i]) / (1 - P[LowerN])
      E21b <- if (is.na(RightN)) NA else
        RightPP * (1 - P[j]) / (1 - P[RightN]) - P[i] * (P[RightN] - P[j]) / (1 - P[RightN])
      E21c <- if (is.na(UpperN)) NA else
        UpperPP * (1 - P[i]) / (1 - P[UpperN]) + P[j] * (P[i] - P[UpperN]) / (1 - P[UpperN])
      E21d <- if (is.na(LeftN))  NA else
        LeftPP  * (1 - P[j]) / (1 - P[LeftN])  + P[i] * (P[j] - P[LeftN])  / (1 - P[LeftN])

      PP[i, j] <- mean(c(E17a, E17b, E17c, E17d, E21a, E21b, E21c, E21d), na.rm = TRUE)
    }
  }

  PP[is.nan(PP)] <- 0
  # Clip imputed cells (only those) to the theoretical Pi bounds.
  PP[PP > upper_bound_pp & was_missing] <- upper_bound_pp[PP > upper_bound_pp & was_missing]
  PP[PP < lower_bound_pp & was_missing] <- lower_bound_pp[PP < lower_bound_pp & was_missing]

  # Back to the original item order.
  PP <- PP[sort(colnames(PP)), sort(colnames(PP))]
  P  <- matrix(P[order(dimnames(P)[[1]])])
  rownames(P) <- dimnames(PP)[[1]]

  covariances <- PP - outer(as.numeric(P), as.numeric(P))

  # Items may not use every response category, so the number of ISRFs varies by
  # item and the block boundaries in `covariances` have to be computed, not
  # assumed uniform.
  observed_categories <- lapply(seq_len(J), function(j) {
    range_all <- -1000:1000
    range_all[range_all %in% data[, j]]
  })
  n_isrfs  <- vapply(observed_categories, length, integer(1)) - 1L
  position <- c(1L, cumsum(n_isrfs))[-1L]

  item_vars <- apply(data, 2, stats::var)
  vapply(seq_len(J), function(j) {
    rows <- (position[j] - (n_isrfs[j] - 1L)):position[j]
    sum(covariances[rows, rows]) / item_vars[j]
  }, numeric(1))
}
