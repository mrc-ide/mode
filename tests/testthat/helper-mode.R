logistic_analytic <- function(r, K, end_time, y0) {
  sapply(0:end_time, function(ti)  K / (1 + (K / y0 - 1) * exp(-r * ti)))
}

logistic_dopri <- function(r, K, end_time, y0) {
  res <- solve_logistic(r, K, end_time, y0)
  dim(res) <- c(length(r), length(res) / 2)
  res
}
