logistic_analytic <- function(r, K, end_time, y0) {
  sapply(0:end_time, function(t) K / (1 + (K / y0 - 1) * exp(-r * t)))
}

logistic_dopri <- function(r, K, end_time, y0) {
  res <- solve_logistic(r, K, end_time, y0)
  dim(res) <- c(length(r), length(res) / 2)
  res
}

logistic_rhs <- function(t, y0, parms) {
  K <- parms$K
  r <- parms$r
  r * y0 * (1 - y0 / K)
}

logistic_dde <- function(r, K, end_time, y0) {
  tt <- 0:end_time
  dde::dopri(y0, tt, logistic_rhs,
             list(r = r, K = K),
             tcrit = tt,
             return_time = FALSE,
             return_by_column = FALSE)
}
