# The Molenaar-Sijtsma implementation in R/04b_mokken_single.R is adapted
# third-party numerical code. These tests fix its behaviour against the original
# implementation that produced the paper's results, so the cleanup cannot have
# changed any number.

# The original, reproduced verbatim from the pre-refactor metric_bootstraps.R.
# Deliberately left in its original style: it is a reference, not code to use.
compute.PP_orig <- function(X, P, N, J, m = 2) {
  label <- as.vector(t(outer(paste("P(X", substr(as.character(100 + (1:J)), 2, 3), ">=", sep = ""),
    paste(1:(m - 1), ")", sep = ""), paste, sep = "")))
  PP <- matrix(0, J * (m - 1), J * (m - 1))
  for (i in 1:(J - 1)) for (j in (i + 1):J) {
    PP[((i-1)*(m-1)+1):((i-1)*(m-1)+(m-1)), ((j-1)*(m-1)+1):((j-1)*(m-1)+(m-1))] <-
      t(outer(X[, i], 0:(m-2), ">")) %*% outer(X[, j], 0:(m-2), ">") / N
  }
  PP <- PP + t(PP) + kronecker(diag(J), matrix(-1, m-1, m-1))
  PP[PP < -.5] <- NA
  dimnames(PP) <- list(label, label)
  PP[order(P), order(P)]
}

singlemokken_orig <- function(data) {
  data <- na.omit(data); data.0 <- data - min(data); J <- ncol(data)
  m <- max(data.0) + 1; N <- nrow(data)
  P <- matrix(t(apply(outer(as.matrix(data.0), 1:(m-1), ">=")*1, c(2,3), mean, na.rm = TRUE)),
              nrow = (m-1)*J)
  PP <- compute.PP_orig(data.0, P, N, J, m); P <- matrix(sort(P))
  off.boundary <- P > 0 & P < 1; P <- matrix(P[off.boundary, 1])
  PP <- PP[off.boundary, off.boundary]; km <- length(P)
  lower.bound.PP <- P %*% t(P)
  upper.bound.PP <- outer(as.numeric(P), as.numeric(P), FUN = "pmin")
  dimnames(P) <- list(dimnames(PP)[[1]], ""); OO <- is.na(PP)
  set.matrix <- outer(as.numeric(P), as.numeric(P), "==")*1
  unique.cells <- which(apply(set.matrix, 1, sum) == 1); Type <- set.matrix
  Type[unique.cells, unique.cells] <- 0; set.vector <- sign(apply(Type, 1, sum))
  Type <- outer(set.vector, set.vector) + outer(rep(1, km), set.vector)*2 +
    outer(set.vector, rep(1, km))
  for (i in 1:km) for (j in 1:km) if (is.na(PP[i, j])) {
    if (Type[i, j] == 4) {
      PP[i, j] <- mean(PP[set.matrix[i, ] == 1, set.matrix[, j] == 1], na.rm = TRUE)
    } else {
      if (Type[i, j] == 1) {
        RightN <- ifelse(any(!is.na(PP[i, j:km])), j + min(which(!is.na(PP[i, j:km]))) - 1, NA)
        RightPP <- ifelse(is.na(RightN), NA, mean(PP[set.matrix[i, ] == 1, set.matrix[, RightN] == 1], na.rm = TRUE))
        LeftN <- ifelse(any(!is.na(PP[i, 1:j])), max(which(!is.na(PP[i, 1:j]))), NA)
        LeftPP <- ifelse(is.na(LeftN), NA, mean(PP[set.matrix[i, ] == 1, set.matrix[, LeftN] == 1], na.rm = TRUE))
        LowerN <- UpperN <- i
        LowerPP <- UpperPP <- mean(PP[set.matrix[i, ] == 1, set.matrix[, j] == 1], na.rm = TRUE)
      }
      if (Type[i, j] == 2) {
        LowerN <- ifelse(any(!is.na(PP[i:km, j])), i + min(which(!is.na(PP[i:km, j]))) - 1, NA)
        LowerPP <- ifelse(is.na(LowerN), NA, mean(PP[set.matrix[LowerN, ] == 1, set.matrix[, j] == 1], na.rm = TRUE))
        UpperN <- ifelse(any(!is.na(PP[1:i, j])), max(which(!is.na(PP[1:i, j]))), NA)
        UpperPP <- ifelse(is.na(UpperN), NA, mean(PP[set.matrix[UpperN, ] == 1, set.matrix[, j] == 1], na.rm = TRUE))
        RightN <- LeftN <- j
        RightPP <- LeftPP <- mean(PP[set.matrix[i, ] == 1, set.matrix[, j] == 1], na.rm = TRUE)
      }
      if (Type[i, j] == 0) {
        RightN <- ifelse(any(!is.na(PP[i, j:km])), j + min(which(!is.na(PP[i, j:km]))) - 1, NA)
        RightPP <- ifelse(is.na(RightN), NA, mean(PP[set.matrix[i, ] == 1, set.matrix[, RightN] == 1], na.rm = TRUE))
        LeftN <- ifelse(any(!is.na(PP[i, 1:j])), max(which(!is.na(PP[i, 1:j]))), NA)
        LeftPP <- ifelse(is.na(LeftN), NA, mean(PP[set.matrix[i, ] == 1, set.matrix[, LeftN] == 1], na.rm = TRUE))
        LowerN <- ifelse(any(!is.na(PP[i:km, j])), i + min(which(!is.na(PP[i:km, j]))) - 1, NA)
        LowerPP <- ifelse(is.na(LowerN), NA, mean(PP[set.matrix[LowerN, ] == 1, set.matrix[, j] == 1], na.rm = TRUE))
        UpperN <- ifelse(any(!is.na(PP[1:i, j])), max(which(!is.na(PP[1:i, j]))), NA)
        UpperPP <- ifelse(is.na(UpperN), NA, mean(PP[set.matrix[UpperN, ] == 1, set.matrix[, j] == 1], na.rm = TRUE))
      }
      E17a <- ifelse(is.na(LowerN), NA, LowerPP*P[i]/P[LowerN])
      E17b <- ifelse(is.na(RightN), NA, RightPP*P[j]/P[RightN])
      E17c <- ifelse(is.na(UpperN), NA, UpperPP*P[i]/P[UpperN])
      E17d <- ifelse(is.na(LeftN), NA, LeftPP*P[j]/P[LeftN])
      E21a <- ifelse(is.na(LowerN), NA, LowerPP*(1-P[i])/(1-P[LowerN]) - P[j]*(P[LowerN]-P[i])/(1-P[LowerN]))
      E21b <- ifelse(is.na(RightN), NA, RightPP*(1-P[j])/(1-P[RightN]) - P[i]*(P[RightN]-P[j])/(1-P[RightN]))
      E21c <- ifelse(is.na(UpperN), NA, UpperPP*(1-P[i])/(1-P[UpperN]) + P[j]*(P[i]-P[UpperN])/(1-P[UpperN]))
      E21d <- ifelse(is.na(LeftN), NA, LeftPP*(1-P[j])/(1-P[LeftN]) + P[i]*(P[j]-P[LeftN])/(1-P[LeftN]))
      PP[i, j] <- mean(c(E17a, E17b, E17c, E17d, E21a, E21b, E21c, E21d), na.rm = TRUE)
    }
  }
  PP[is.nan(PP)] <- 0
  PP[PP > upper.bound.PP & OO] <- upper.bound.PP[PP > upper.bound.PP & OO]
  PP[PP < lower.bound.PP & OO] <- lower.bound.PP[PP < lower.bound.PP & OO]
  PP <- PP[sort(colnames(PP)), sort(colnames(PP))]
  P <- matrix(P[order(dimnames(P)[[1]])]); rownames(P) <- dimnames(PP)[[1]]
  num <- PP - outer(as.numeric(P), as.numeric(P))
  find.cats <- function(x, range = -1000:1000) range[range %in% x]
  which.cat <- list(); for (j in 1:J) which.cat[[j]] <- find.cats(data[, j])
  number.ISRFs <- sapply(which.cat, length) - 1
  position <- c(1, cumsum(number.ISRFs))[-1]
  item.vars <- apply(data, 2, var); singlemok <- rep(NA, J)
  for (j in 1:J) {
    rows <- ((position[j] - (number.ISRFs[j] - 1)):position[j])
    singlemok[j] <- sum(num[rows, rows])/item.vars[j]
  }
  singlemok
}

test_that("single_mokken is numerically identical to the original", {
  set.seed(7)
  for (trial in 1:6) {
    n <- sample(40:90, 1); p <- sample(6:15, 1)
    X <- matrix(stats::rbinom(n * p, 1, runif(p, 0.2, 0.8)), n, p,
                dimnames = list(NULL, paste0("it", seq_len(p))))
    expect_equal(single_mokken(X), singlemokken_orig(X),
                 info = glue("trial {trial}: n = {n}, p = {p}"))
  }
})

test_that("compute_pp is numerically identical to the original", {
  set.seed(21)
  X <- matrix(stats::rbinom(50 * 6, 1, 0.5), 50, 6)
  P <- matrix(colMeans(X))
  expect_equal(compute_pp(X, P, N = 50, J = 6, m = 2),
               compute.PP_orig(X, P, N = 50, J = 6, m = 2))
})

test_that("the unobservable within-item blocks start as NA", {
  set.seed(3)
  X <- matrix(stats::rbinom(30 * 4, 1, 0.5), 30, 4)
  PP <- compute_pp(X, matrix(colMeans(X)), N = 30, J = 4, m = 2)
  expect_true(all(is.na(diag(PP))))
})

test_that("degenerate inputs are errors rather than silent nonsense", {
  expect_error(compute_pp(matrix(1, 5, 1), matrix(1), N = 5, J = 1, m = 2),
               "at least 2 items")
  expect_error(single_mokken(matrix(1, 5, 1)), "at least 2 items")
})
