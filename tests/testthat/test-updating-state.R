test_that("Can update time", {
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 100)
  n_particles <- 10
  initial_time <- 1
  mod <- gen$new(pars, initial_time, n_particles)
  expect_equal(mod$time(), initial_time)
  res <- mod$run(5)
  expect_equal(mod$time(), 5)
  mod$update_state(time = initial_time)
  expect_equal(mod$time(), initial_time)
  res2 <- mod$run(5)
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
  res <- mod$run(5)

  mod$update_state(time = initial_time)
  res2 <- mod$run(5)
  # expect results to be identical because state was reset
  expect_true(identical(res, res2))

  mod$update_state(time = initial_time, set_initial_state = FALSE)
  res3 <- mod$run(5)
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
  res <- mod$run(5)
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
  res_t2 <- mod$run(2)
  res_t3 <- mod$run(3)
  mod$update_state(time = initial_time, state = res_t2[, 1])
  res2_t2 <- mod$run(2)
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
  expect_error(mod$update_state(state = c(1, 2), index = 1),
               "Expected 'state' to be a vector of length 1 but was length 2")
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

  # check wrong with index
  expect_error(mod$update_state(state = matrix(1, nrow = 2, ncol = 5),
                                index = 1),
               "Expected 'state' to be a 1 by 5 matrix but was 2 by 5")
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
  res <- vapply(1:10, function(t) mod$run(t),
                matrix(0.0, 2, n_particles))
  prev_state <- res[, 1, 10]
  new_state <- prev_state + 10
  mod$update_state(state = new_state)
  state <- mod$state()
  expect_identical(state[1:2, ], matrix(new_state, nrow = 2, ncol = 2))
  expect_identical(state[1, ] + state[2, ], state[3, ])
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
  res <- vapply(1:10, function(t) mod$run(t),
                matrix(0.0, 2, n_particles))
  prev_state <- res[, 1, 10]
  new_state <- cbind(prev_state + 10, prev_state + 11, prev_state + 12)
  mod$update_state(state = new_state)
  state <- mod$state()
  expect_identical(state[1:2, ], new_state)
  expect_identical(state[1, ] + state[2, ], state[3, ])
})

test_that("Can update state with a vector and index", {
  y0 <- c(1, 1)
  r <- c(0.1, 0.2)
  k <- c(100, 100)
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = r[1], r2 = r[2], K1 = k[1], K2 = k[2])
  n_particles <- 2
  mod <- gen$new(pars, 0, n_particles)
  res <- vapply(1:10, function(t) mod$run(t),
                matrix(0.0, 2, n_particles))
  prev_state <- res[, 1, 10]
  new_state <- prev_state + 10
  mod$update_state(state = new_state[1], index = 1)
  state <- mod$state()
  expect_identical(state[1:2, ], matrix(c(new_state[1],
                                         prev_state[2]), nrow = 2, ncol = 2))
  expect_identical(state[1, ] + state[2, ], state[3, ])
})

test_that("Can update state with a matrix and index", {
  y0 <- c(1, 1)
  r <- c(0.1, 0.2)
  k <- c(100, 100)
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = r[1], r2 = r[2], K1 = k[1], K2 = k[2])
  n_particles <- 3
  mod <- gen$new(pars, 0, n_particles)
  res <- vapply(1:10, function(t) mod$run(t),
                matrix(0.0, 2, n_particles))
  prev_state <- res[, 1, 10]
  new_state <- cbind(prev_state[1] + 10, prev_state[1] + 11, prev_state[1] + 12)
  mod$update_state(state = new_state, index = 1)
  state <- mod$state()
  expect_identical(state[1:2, ], rbind(new_state, rep(prev_state[2], 3)))
  expect_identical(state[1, ] + state[2, ], state[3, ])
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
  res <- mod$run(2)
  stats <- mod$statistics()
  mod$update_state(state = c(2, 2))
  expect_identical(stats, mod$statistics())
})

test_that("Nothing happens if any arguments are invalid", {
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  initial_pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 100)
  n_particles <- 5
  initial_time <- 1
  mod <- gen$new(initial_pars, initial_time, n_particles)
  initial_state <- mod$state()
  expect_error(mod$update_state(state = c(1, 2, 3),
                                time = 5,
                                pars = list(r1 = 0.1,
                                            r2 = 0.2,
                                            K1 = 200,
                                            K2 = 200)),
               "Expected 'state' to be a vector of length 2 but was length 3")
  expect_equal(mod$time(), initial_time)
  expect_equal(mod$pars(), initial_pars)

  expect_error(mod$update_state(state = c(1, 2),
                                time = c(1, 2),
                                pars = list(r1 = 0.1,
                                            r2 = 0.2,
                                            K1 = 200,
                                            K2 = 200)),
                                "Expected 'time' to be a scalar value")
  expect_equal(mod$state(), initial_state)
  expect_equal(mod$pars(), initial_pars)

  expect_error(mod$update_state(pars = list(r3 = 1),
                                state = c(1, 2),
                                time = 5))
  expect_equal(mod$state(), initial_state)
  expect_equal(mod$time(), initial_time)
})

test_that("Can update pars", {
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  initial_r <- c(0.1, 0.2)
  initial_k <- c(100, 100)
  initial_pars <- list(r1 = initial_r[[1]], r2 = initial_r[[2]],
                       K1 = initial_k[[1]], K2 = initial_k[[2]])
  n_particles <- 5
  initial_time <- 0
  mod <- gen$new(initial_pars, initial_time, n_particles)
  y1 <- mod$run(1)
  analytic <- logistic_analytic(initial_r, initial_k, 1, c(1, 1))
  expect_equal(mod$state(1:2), y1)
  expect_equal(analytic, y1[, 1, drop = FALSE], tolerance = 1e-7)
  new_k <- c(200, 200)
  new_pars <- list(r1 = initial_r[[1]], r2 = initial_r[[2]],
                   K1 = new_k[[1]], K2 = new_k[[2]])
  mod$update_state(pars = new_pars, set_initial_state = FALSE)
  expect_equal(mod$state(1:2), y1)
  expect_equal(mod$time(), 1)
  expect_equal(mod$pars(), new_pars)

  y2 <- mod$run(2)

  expect_equal(mod$state(1:2), y2)

  mod$update_state(time = 1, pars = new_pars, state = y1)
  expect_equal(mod$state(1:2), y1)
  expect_equal(mod$time(), 1)
  expect_equal(mod$pars(), new_pars)

  y3 <- mod$run(2)

  analytic <- logistic_analytic(initial_r, new_k, 1, y1[, 1])

  expect_equal(analytic, y3[, 1, drop = FALSE], tolerance = 1e-7)
  expect_equal(analytic, y2[, 1, drop = FALSE], tolerance = 1e-7)
  expect_true(all(y2 == y3))
})

test_that("Updating pars set initial state by default", {
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  initial_r <- c(0.1, 0.2)
  initial_k <- c(100, 100)
  initial_pars <- list(r1 = initial_r[[1]], r2 = initial_r[[2]],
                       K1 = initial_k[[1]], K2 = initial_k[[2]])
  n_particles <- 5
  initial_time <- 0
  mod <- gen$new(initial_pars, initial_time, n_particles)
  y1 <- mod$run(2)
  expect_equal(mod$state(1:2), y1)
  new_k <- c(200, 200)
  new_pars <- list(r1 = initial_r[[1]], r2 = initial_r[[2]],
                   K1 = new_k[[1]], K2 = new_k[[2]])
  mod$update_state(pars = new_pars)
  expect_equal(mod$pars(), new_pars)
  expect_equal(mod$state(1:2), matrix(1, ncol = n_particles, nrow = 2))
})

test_that("Error if particle re-ordering index is wrong length", {
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 100)
  n_particles <- 5
  initial_time <- 1
  mod <- gen$new(pars, initial_time, n_particles)

  expect_error(mod$reorder(c(5, 4, 3)), "'index' must be a vector of length 5")
})

test_that("Error if particle re-ordering index is out of range", {
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 100)
  n_particles <- 5
  initial_time <- 1
  mod <- gen$new(pars, initial_time, n_particles)
  expect_error(mod$reorder(c(2, 3, 4, 5, 6)),
               "All elements of 'index' must lie in [1, 5]", fixed = TRUE)
})

test_that("Can reorder particles", {
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 100)
  n_particles <- 5
  initial_time <- 1
  mod <- gen$new(pars, initial_time, n_particles)
  mod$run(2)
  y <- matrix(as.numeric(1:10), ncol = 5, nrow = 2)
  mod$update_state(state = y)

  mod$reorder(c(5, 4, 3, 2, 1))
  ans <- mod$state(1:2)
  expect_equal(ans, y[, 5:1])

  # also check that the stepper has been re-initialised correctly
  # run sufficiently forward in time that statistics for particles diverge
  mod_fresh <- gen$new(pars, initial_time, n_particles)
  mod_fresh$run(2)
  y <- matrix(as.numeric(1:10), ncol = 5, nrow = 2)
  mod_fresh$update_state(state = y)
  mod$run(100)
  mod_fresh$run(100)
  stats <- mod$statistics()
  expect_false(all(unclass(stats) == stats[, 5:1]))
  expect_identical(stats[, 5:1], unclass(mod_fresh$statistics()))
  expect_identical(mod$state()[, 5:1], mod_fresh$state())
})

test_that("Can reorder particles with duplication", {
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 100)
  n_particles <- 5
  initial_time <- 1
  mod <- gen$new(pars, initial_time, n_particles)
  mod$run(2)
  y <- matrix(as.numeric(1:10), ncol = 5, nrow = 2)
  mod$update_state(state = y)

  order_index <- c(1, 2, 2, 2, 2)
  mod$reorder(order_index)
  ans <- mod$state(1:2)
  expect_equal(ans, y[, order_index])

  # also check that the stepper has been re-initialised correctly
  # run sufficiently forward in time that statistics for particles diverge
  mod_fresh <- gen$new(pars, initial_time, n_particles)
  mod_fresh$run(2)
  mod_fresh$update_state(state = y)
  mod$run(100)
  mod_fresh$run(100)
  stats <- mod$statistics()
  stats_fresh <- mod_fresh$statistics()
  expect_false(all(unclass(stats_fresh) == stats_fresh[, order_index]))
  expect_identical(unclass(stats), stats_fresh[, order_index])
  expect_identical(mod$state(), mod_fresh$state()[, order_index])
})

test_that("Can reorder particles mid-flight", {
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 100)
  n_particles <- 5
  initial_time <- 1
  y0 <- matrix(as.numeric(1:10), ncol = 5, nrow = 2)

  ## Run in one go:
  mod <- gen$new(pars, initial_time, n_particles)
  mod$update_state(state = y0, time = initial_time)
  mod$run(5)
  y1 <- mod$run(10)
  s1 <- mod$statistics()

  ## Run half way, reorder, continue:
  mod$update_state(state = y0, time = initial_time)

  y2 <- mod$run(5)
  s2 <- mod$statistics()
  mod$reorder(5:1)
  y3 <- mod$run(10)
  s3 <- mod$statistics()

  expect_identical(y3, y1[, 5:1])
  expect_true(all(unclass(s3) == s3[, 5:1]))
})
