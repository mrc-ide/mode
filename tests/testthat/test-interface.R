test_that("Can compile a simple model", {
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 100)
  n_particles <- 10
  mod <- gen$new(pars, pi, n_particles)
  expect_s3_class(mod, "mode")
  expect_equal(mod$time(), pi)
  expect_equal(mod$pars(), pars)
  expect_equal(mod$n_particles(), n_particles)
})

test_that("Returned state has correct dimensions", {
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 100)
  n_particles <- 10
  mod <- gen$new(pars, 1, n_particles)
  res <- mod$solve(2)
  expect_equal(dim(res), c(2, n_particles))
})

test_that("End time must be later than initial time", {
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 100)
  n_particles <- 10
  initial_time <- 5
  mod <- gen$new(pars, initial_time, n_particles)
  expect_equal(mod$time(), initial_time)
  e <- "'end_time' (2.000000) must be greater than current time (5.000000)"
  expect_error(mod$solve(2), e, fixed = TRUE)
})

test_that("cache hits don't compile", {
  skip_if_not_installed("mockery")

  ## Simple logisitic model, but with a random suffix
  code <- readLines(mode_file("examples/logistic.cpp"))
  path <- tempfile(pattern = "logistic")
  name <- basename(path)
  writeLines(gsub("logistic", name, code), path)
  hash <- hash_file(path)

  expect_false(paste0(name, hash) %in% names(cache))
  gen <- mode(path, quiet = TRUE)
  expect_true(paste0(name, hash) %in% names(cache))

  mock_compile_mode <- mockery::mock(cache[[paste0(name, hash)]])
  mockery::stub(mode, "compile_mode", mock_compile_mode)
  gen2 <- mode(path, quiet = TRUE)
  mockery::expect_called(mock_compile_mode, 0)
  expect_identical(gen2, gen)

  gen3 <- mode(path, quiet = TRUE, skip_cache = TRUE)
  mockery::expect_called(mock_compile_mode, 1)
  expect_identical(gen3, gen)
})

test_that("Can update time", {
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 100)
  n_particles <- 10
  initial_time <- 1
  mod <- gen$new(pars, initial_time, n_particles)
  expect_equal(mod$time(), initial_time)
  res <- mod$solve(5)
  expect_equal(mod$time(), 5)
  mod$update_state(time = initial_time)
  expect_equal(mod$time(), initial_time)
  res2 <- mod$solve(5)
  expect_identical(res, res2)
})

test_that("Can only update time for all particles at once", {
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 100)
  n_particles <- 5
  initial_time <- 1
  mod <- gen$new(pars, initial_time, n_particles)
  expect_error(mod$update_state(time = c(1, 2, 3, 4, 5)),
               "Expected 'time' to be a scalar value")
})

test_that("Error if state vector does not have correct length", {
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 100)
  n_particles <- 5
  mod <- gen$new(pars, 1, n_particles)
  expect_error(mod$update_state(state = c(1, 2, 3, 4, 5)),
               "Expected 'state' to be a vector of length 2 but was length 5")
})

test_that("Error if state matrix does not have correct dimensions", {
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 100)
  n_particles <- 5
  mod <- gen$new(pars, 1, n_particles)
  # check wrong ncol
  expect_error(mod$update_state(state = matrix(1, nrow = 2, ncol = 3)),
               "Expected 'state' to be a 2 by 5 matrix but was 2 by 3")
  # check wrong nrow
  expect_error(mod$update_state(state = matrix(1, nrow = 3, ncol = 5)),
               "Expected 'state' to be a 2 by 5 matrix but was 3 by 5")
})

test_that("Can update state with a vector", {
  y0 <- c(1, 1)
  r <- c(0.1, 0.2)
  k <- c(100, 100)
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = r[1], r2 = r[2], K1 = k[1], K2 = k[2])
  n_particles <- 2
  mod <- gen$new(pars, 0, n_particles)
  res <- vapply(1:10, function(t) mod$solve(t),
                matrix(0.0, 2, n_particles))
  prev_state <- res[, 1, 10]
  new_state <- prev_state + 10
  mod$update_state(state = new_state)
  analytic <- logistic_analytic(r, k, 1:5, new_state)
  res <- vapply(11:15, function(t) mod$solve(t),
                matrix(0.0, 2, n_particles))
  expect_equal(analytic, res[, 1, ], tolerance = 1e-7)
  expect_identical(res, res[, rep(1, 2), ])
})

test_that("Can update state with a matrix", {
  y0 <- c(1, 1)
  r <- c(0.1, 0.2)
  k <- c(100, 100)
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = r[1], r2 = r[2], K1 = k[1], K2 = k[2])
  n_particles <- 3
  mod <- gen$new(pars, 0, n_particles)
  res <- vapply(1:10, function(t) mod$solve(t),
                matrix(0.0, 2, n_particles))
  prev_state <- res[, 1, 10]
  new_state <- cbind(prev_state + 10, prev_state + 11, prev_state + 12)
  mod$update_state(state = new_state)
  analytic_1 <- logistic_analytic(r, k, 1:5, new_state[, 1])
  analytic_2 <- logistic_analytic(r, k, 1:5, new_state[, 2])
  analytic_3 <- logistic_analytic(r, k, 1:5, new_state[, 3])
  res <- vapply(11:15, function(t) mod$solve(t),
                matrix(0.0, 2, n_particles))
  expect_equal(analytic_1, res[, 1, ], tolerance = 1e-7)
  expect_equal(analytic_2, res[, 2, ], tolerance = 1e-7)
  expect_equal(analytic_3, res[, 3, ], tolerance = 1e-7)
})
