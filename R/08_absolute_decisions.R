#' Normal-approximation interval around an observed mean score
#'
#' Bounds are clamped to the score scale. For binary-scored benchmarks the
#' normal approximation is poor near the boundaries, so a clamped interval
#' there is a floor on the uncertainty, not an accurate one: a model at 0.05
#' with a wide SEM gets a lower bound of 0 because scores cannot go below it,
#' not because the estimate is precise. Use logit_interval() when the extremes
#' matter.
#'
#' @param score Observed mean score.
#' @param sem Standard error from sem_absolute() or sem_relative().
#' @param level Coverage, default 0.95.
#' @param bounds Score scale, default c(0, 1). Pass NULL for no clamping.
#' @return Data frame with lower, upper, and a flag for clamped rows.

sem_absolute <- function(var_item, var_residual, n_items) {
  var_item     <- pmax(var_item, 0)
  var_residual <- pmax(var_residual, 0)
  n_items      <- pmax(n_items, 1)
  sqrt((var_item + var_residual) / n_items)
}

sem_relative <- function(var_residual, n_items) {
  var_residual <- pmax(var_residual, 0)
  n_items      <- pmax(n_items, 1)
  sqrt(var_residual / n_items)
}

add_sems <- function(components, n_items) {
  components$sem_abs   <- sem_absolute(components$item, components$Residual, n_items)
  components$sem_rel   <- sem_relative(components$Residual, n_items)
  components$sem_ratio <- components$sem_abs / components$sem_rel
  components$n_items   <- n_items
  components
}

score_interval <- function(score, sem, level = 0.95, bounds = c(0, 1)) {
  z     <- stats::qnorm(1 - (1 - level) / 2)
  lower <- score - z * sem
  upper <- score + z * sem
  
  clamped <- rep(FALSE, length(lower))
  if (!is.null(bounds)) {
    clamped <- lower < bounds[1] | upper > bounds[2]
    lower   <- pmax(lower, bounds[1])
    upper   <- pmin(upper, bounds[2])
  }
  data.frame(lower = lower, upper = upper, clamped = clamped)
}

#' Interval on the logit scale, back-transformed
#'
#' Respects the [0, 1] boundary by construction rather than by clipping, so it
#' stays asymmetric near the extremes instead of piling up against the bound.
#' Undefined at exactly 0 or 1, where it returns NA.
#'
#' @inheritParams score_interval
#' @return Data frame with lower and upper bounds on the score scale.
logit_interval <- function(score, sem, level = 0.95) {
  z  <- stats::qnorm(1 - (1 - level) / 2)
  ok <- score > 0 & score < 1
  
  lower <- upper <- rep(NA_real_, length(score))
  # Delta-method SE of logit(p) is SE(p) / (p(1-p)).
  se_logit <- sem[ok] / (score[ok] * (1 - score[ok]))
  eta      <- stats::qlogis(score[ok])
  lower[ok] <- stats::plogis(eta - z * se_logit)
  upper[ok] <- stats::plogis(eta + z * se_logit)
  
  data.frame(lower = lower, upper = upper)
}