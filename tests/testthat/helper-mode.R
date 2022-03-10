logistic_analytic <- function(r, K, times, y0) {
  sapply(times, function(t) K / (1 + (K / y0 - 1) * exp(-r * t)))
}

logistic_mode <- function(r, K, times, y0) {
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = r[[1]], r2 = r[[2]], K1 = K[[1]], K2 = K[[2]])
  mod <- gen$new(pars, 0)
  sapply(times, function(t) mod$solve(t))
}

logistic_rhs <- function(t, y0, parms) {
  K <- parms$K
  r <- parms$r
  r * y0 * (1 - y0 / K)
}

logistic_dde <- function(r, K, times, y0) {
  dde::dopri(y0, times, logistic_rhs,
             list(r = r, K = K),
             tcrit = times,
             return_time = FALSE,
             return_by_column = FALSE)
}
