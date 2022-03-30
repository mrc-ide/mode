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
  expect_equal(mod$time(), initial_time)
})

test_that(
  "Updating time does not reset state iff set_initial_state is FAlSE", {
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 100)
  initial_time <- 1
  mod <- gen$new(pars, initial_time, 5)
  res <- mod$solve(5)

  mod$update_state(time = initial_time)
  res2 <- mod$solve(5)
  # expect results to be identical because state was reset
  expect_true(identical(res, res2))

  mod$update_state(time = initial_time, set_initial_state = FALSE)
  res3 <- mod$solve(5)
  # expect results to be different because state was not reset
  expect_false(identical(res, res3))
})

test_that("Updating time resets statistics", {
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 100)
  n_particles <- 5
  initial_time <- 1
  mod <- gen$new(pars, initial_time, n_particles)
  res <- mod$solve(5)
  expect_false(all(mod$statistics() == 0))

  mod$update_state(time = initial_time)
  expect_true(all(mod$statistics() == 0))
})

test_that("Updating time does not reset state if new state is provided", {
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 100)
  n_particles <- 5
  initial_time <- 1
  mod <- gen$new(pars, initial_time, n_particles)
  expect_equal(mod$time(), initial_time)
  res_t2 <- mod$solve(2)
  res_t3 <- mod$solve(3)
  mod$update_state(time = initial_time, state = res_t2[, 1])
  res2_t2 <- mod$solve(2)
  expect_equal(res_t3, res2_t2, tolerance = 1e-7)
})

test_that("'set_initial_state' cannot be TRUE unless 'state' is NULL", {
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 100)
  n_particles <- 5
  initial_time <- 1
  mod <- gen$new(pars, initial_time, n_particles)
  expect_equal(mod$time(), initial_time)
  expect_error(mod$update_state(state = c(2, 2), set_initial_state = TRUE),
               "'set_initial_state' cannot be TRUE unless 'state' is NULL")
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
  expect_identical(mod$state(), matrix(new_state, nrow = 2, ncol = 2))
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
  expect_identical(mod$state(), new_state)
})

test_that("Updating state does not reset statistics", {
  y0 <- c(1, 1)
  r <- c(0.1, 0.2)
  k <- c(100, 100)
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = r[1], r2 = r[2], K1 = k[1], K2 = k[2])
  n_particles <- 2
  mod <- gen$new(pars, 0, n_particles)
  res <- mod$solve(2)
  stats <- mod$statistics()
  mod$update_state(state = c(2, 2))
  expect_identical(stats, mod$statistics())
})

test_that("Nothing happens if any parameters are invalid", {
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 100)
  n_particles <- 5
  initial_time <- 1
  mod <- gen$new(pars, initial_time, n_particles)
  initial_state <- mod$state()
  expect_error(mod$update_state(state = c(1, 2, 3), time = 5),
               "Expected 'state' to be a vector of length 2 but was length 3")
  expect_equal(mod$time(), initial_time)

  expect_error(mod$update_state(state = c(1, 2), time = c(1, 2)),
                                "Expected 'time' to be a scalar value")
  expect_equal(mod$state(), initial_state)
})
