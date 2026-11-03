# Simulated 0/1 responses from a 2PL, models x items, no constant columns.
sim_responses <- function(M = 60, I = 40, seed = 1) {
  set.seed(seed)
  theta <- rnorm(M); a <- rlnorm(I, 0, 0.4); b <- rnorm(I)
  P <- plogis(outer(theta, a) - matrix(a * b, M, I, byrow = TRUE))
  X <- matrix(rbinom(M * I, 1, P), M, I, dimnames = list(paste0("m", 1:M), paste0("i", 1:I)))
  X[, apply(X, 2, var) > 0]
}
