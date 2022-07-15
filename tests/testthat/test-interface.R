test_that("Can compile a simple model", {
  ex <- example_logistic()
  n_particles <- 10
  mod <- ex$generator$new(ex$pars, pi, n_particles)
  expect_s3_class(mod, "mode")
  expect_equal(mod$time(), pi)
  expect_equal(mod$pars(), ex$pars)
  expect_equal(mod$info(), c("N1", "N2"))
  expect_equal(mod$n_particles(), n_particles)
  expected_control <- mode_control(max_steps = 10000, rtol = 1e-6, atol = 1e-6,
                          step_size_min = 1e-8, step_size_max = Inf)
  expect_equal(mod$control(), expected_control)
})

test_that("Can compile a simple model with control", {
  ex <- example_logistic()
  n_particles <- 10
  control <- mode_control(max_steps = 10, rtol = 0.01, atol = 0.02,
                          step_size_min = 0.1, step_size_max = 1)
  mod <- ex$generator$new(ex$pars, pi, n_particles, control = control)
  ctl <- mod$control()
  expect_s3_class(control, "mode_control")
  expect_s3_class(ctl, "mode_control")
  expect_equal(ctl, control)
})

test_that("Can compile a simple model with partial control", {
  ex <- example_logistic()
  n_particles <- 10
  control <- mode_control(max_steps = 10, atol = 0.2)
  mod <- ex$generator$new(ex$pars, pi, n_particles, control = control)
  expect_s3_class(control, "mode_control")
  ctl <- mod$control()
  expect_s3_class(ctl, "mode_control")
  expect_equal(ctl$max_steps, 10)
  expect_equal(ctl$atol, 0.2)
  expect_equal(ctl$rtol, 1e-6)
  expect_equal(ctl$step_size_min, 1e-8)
  expect_equal(ctl$step_size_max, Inf)
})

test_that("Returns full state from run when no index set", {
  ex <- example_logistic()
  n_particles <- 10
  mod <- ex$generator$new(ex$pars, 1, n_particles)
  res <- mod$run(2)
  expect_equal(dim(res), c(3, n_particles))

  state <- mod$state()
  expect_identical(res, state)
})

test_that("Returns state from run for set index", {
  ex <- example_logistic()
  n_particles <- 10
  mod <- ex$generator$new(ex$pars, 1, n_particles)
  mod$set_index(1)
  res <- mod$run(2)
  expect_equal(dim(res), c(1, n_particles))

  state <- mod$state()
  expect_identical(res, state[1, , drop = FALSE])
})

test_that("Can get arbitrary partial state", {
  ex <- example_logistic()
  n_particles <- 10
  mod <- ex$generator$new(ex$pars, 1, n_particles)
  res <- mod$run(2)
  expect_equal(dim(res), c(3, n_particles))

  state <- mod$state(1)
  expect_identical(state, res[1, , drop = FALSE])

  state <- mod$state(2)
  expect_identical(state, res[2, , drop = FALSE])

  state <- mod$state()
  expect_identical(state, res)
})

test_that("Error if partial state index is invalid", {
  ex <- example_logistic()
  n_particles <- 10
  mod <- ex$generator$new(ex$pars, 1, n_particles)
  expect_error(mod$state(4),
               "All elements of 'index' must lie in [1, 3]",
               fixed = TRUE)
  expect_error(mod$state(c(1, 2, 4)),
               "All elements of 'index' must lie in [1, 3]",
               fixed = TRUE)
})

test_that("Can set vector index", {
  ex <- example_logistic()
  n_particles <- 10
  mod <- ex$generator$new(ex$pars, 1, n_particles)
  mod$set_index(c(1, 2))
  res <- mod$run(2)
  expect_equal(dim(res), c(2, n_particles))
})

test_that("Can retrieve index", {
  ex <- example_logistic()
  n_particles <- 10
  mod <- ex$generator$new(ex$pars, 1, n_particles)
  expect_equal(NULL, mod$index())
  mod$set_index(c(1, 2))
  expect_equal(c(1, 2), mod$index())
})

test_that("Setting a named index returns names", {
  ex <- example_logistic()
  r <- c(0.1, 0.2)
  k <- c(100, 100)
  pars <- list(r1 = r[[1]], r2 = r[[2]], K1 = k[[1]], K2 = k[[2]])
  n_particles <- 5
  mod <- ex$generator$new(pars, 0, n_particles)
  analytic <- logistic_analytic(r, k, 1, c(1, 1))
  mod$set_index(c(y1 = 1L, y2 = 2L))
  expect_equal(
    mod$run(1),
    rbind(y1 = rep(analytic[1, ], n_particles),
          y2 = rep(analytic[2, ], n_particles)),
    tolerance = 1e-7)
})

test_that("Can clear index", {
  ex <- example_logistic()
  r <- c(0.1, 0.2)
  k <- c(100, 100)
  pars <- list(r1 = r[[1]], r2 = r[[2]], K1 = k[[1]], K2 = k[[2]])
  n_particles <- 5
  mod <- ex$generator$new(pars, 0, n_particles)
  analytic <- logistic_analytic(r, k, 1:2, c(1, 1))
  mod$set_index(c(y1 = 1L))
  expect_equal(
    mod$run(1),
    rbind(y1 = rep(analytic[1, 1], n_particles)),
    tolerance = 1e-7)
  expect_equal(mod$index(), c(y1 = 1L))
  mod$set_index(NULL)
  expect_equal(
    mod$run(2)[1:2, ],
    rbind(rep(analytic[1, 2], n_particles),
          rep(analytic[2, 2], n_particles)),
    tolerance = 1e-7)
  expect_null(mod$index())
})

test_that("Cannot set invalid index", {
  ex <- example_logistic()
  n_particles <- 10
  mod <- ex$generator$new(ex$pars, 1, n_particles)
  expect_error(mod$set_index(0),
               "All elements of 'index' must lie in [1, 3]",
               fixed = TRUE)
  expect_error(mod$set_index(4),
               "All elements of 'index' must lie in [1, 3]",
               fixed = TRUE)
  expect_error(mod$set_index(c(1, 2, 4)),
               "All elements of 'index' must lie in [1, 3]",
               fixed = TRUE)
})

test_that("End time must be later than initial time", {
  ex <- example_logistic()
  n_particles <- 10
  initial_time <- 5
  mod <- ex$generator$new(ex$pars, initial_time, n_particles)
  expect_equal(mod$time(), initial_time)
  e <- "'end_time' (2.000000) must be greater than current time (5.000000)"
  expect_error(mod$run(2), e, fixed = TRUE)
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

test_that("Can retrieve statistics", {
  ex <- example_logistic()
  n_particles <- 5
  mod <- ex$generator$new(ex$pars, 1, n_particles)
  stats <- mod$statistics()
  expect_equal(dim(stats), c(3, n_particles))
  expect_equal(row.names(stats),
               c("n_steps", "n_steps_accepted", "n_steps_rejected"))
  expect_s3_class(stats, "mode_statistics")
  expect_true(all(stats == 0))
  lapply(1:10, function(t) mod$run(t))
  stats <- mod$statistics()
  expect_true(all(stats == stats[, rep(1, n_particles)]))
  expect_true(all(stats["n_steps", ] > 0))

  expect_null(attr(stats, "step_times")) # see below
})

test_that("Can retrieve statistics", {
  ex <- example_logistic()
  n_particles <- 5
  mod <- ex$generator$new(ex$pars, 1, n_particles)
  stats <- mod$statistics()
  expect_equal(dim(stats), c(3, n_particles))
  expect_equal(row.names(stats),
               c("n_steps", "n_steps_accepted", "n_steps_rejected"))
  expect_s3_class(stats, "mode_statistics")
  expect_true(all(stats == 0))
  lapply(1:10, function(t) mod$run(t))
  stats <- mod$statistics()
  expect_true(all(stats == stats[, rep(1, n_particles)]))
  expect_true(all(stats["n_steps", ] > 0))
})

test_that("Can get model size", {
  ex <- example_logistic()
  mod <- ex$generator$new(ex$pars, 1, 1)
  expect_equal(mod$n_state_run(), 3)
  expect_equal(mod$n_state_full(), 3)
  mod$set_index(1)
  expect_equal(mod$n_state_run(), 1)
  expect_equal(mod$n_state_full(), 3)
  mod$set_index(c(1, 2, 3))
  expect_equal(mod$n_state_run(), 3)
  expect_equal(mod$n_state_full(), 3)
})

test_that("can run to noninteger time", {
  ex <- example_logistic()
  mod <- ex$generator$new(ex$pars, 0, 1)

  t <- 3.95

  y <- mod$run(t)
  expect_equal(mod$time(), t)
  expect_equal(y[1:2, , drop = FALSE],
               logistic_analytic(c(0.1, 0.2), c(100, 100), t, c(1, 1)),
               tolerance = 1e-7)
})

test_that("Errors are reported", {
  ex <- example_logistic()
  mod <- ex$generator$new(ex$pars, 0, 2, control = mode_control(max_steps = 1))
  err <- expect_error(mod$run(5), "2 particles reported errors.")
  expect_match(
    err$message,
    "- 1: too many steps")
})

test_that("Can run a stochastic model", {
  path <- mode_file("examples/stochastic.cpp")
  gen <- mode(path, quiet = TRUE)

  np <- 10
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 200, v = 0.1)
  mod <- gen$new(pars, 0, np, seed = 1L)
  mod$set_stochastic_schedule(0:5)

  rng <- dust::dust_rng$new(n_streams = np, seed = 1L)

  expect_equal(mod$state(), matrix(1, 3, np))

  ## Events happen at t + eps so running to t leaves things unchanged:
  mod$run(0)
  expect_equal(mod$state(), matrix(1, 3, np))

  ## Any bit beyond and we will run the stochastic update
  y1 <- mod$run(1e-8)
  expect_equal(y1[1:2, ], matrix(1, 2, np))
  expect_equal(y1[3, ], drop(exp(rng$normal(1, 0, 0.1))))

  ## Run up to the next one and we won't run a stochastic step, but we
  ## will complete a full deterministic step
  y2 <- mod$run(1)
  expect_equal(y2[1, ],
               drop(logistic_analytic(0.1, 100 * y1[3, ], 1, c(1, 1))))
  expect_equal(y2[2, ],
               drop(logistic_analytic(0.2, 200 * y1[3, ], 1, c(1, 1))))
  expect_equal(y2[3, ], y1[3, ])

  ## Up to the end we've run 5 stochastic updates (but not the 6th)
  y_end <- mod$run(5)
  rng <- dust::dust_rng$new(n_streams = np, seed = 1L)
  expect_equal(y_end[3, ], apply(exp(rng$normal(5, 0, 0.1)), 2, prod))
})


test_that("Can validate the stochastic schedule times", {
  path <- mode_file("examples/stochastic.cpp")
  gen <- mode(path, quiet = TRUE)

  np <- 10
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 200, v = 0.1)
  mod <- gen$new(pars, 0, np, seed = 1L)
  expect_error(
    mod$set_stochastic_schedule(c(1, 2, 3, 3, 4, 5)),
    "schedule must be strictly increasing; see time[4]",
    fixed = TRUE)
  expect_error(
    mod$set_stochastic_schedule(c(1, 2, 3, 4, 1, 2, 3)),
    "schedule must be strictly increasing; see time[5]",
    fixed = TRUE)

  ## No schedule set:
  y <- mod$run(10)
  expect_equal(y[3, ], rep(1, np))
})

test_that("A null schedule clears stochastic schedule", {
  path <- mode_file("examples/stochastic.cpp")
  gen <- mode(path, quiet = TRUE)

  np <- 10
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 200, v = 0.1)
  mod <- gen$new(pars, 0, np, seed = 1L)
  mod$set_stochastic_schedule(0:5)
  mod$set_index(3)

  rng <- dust::dust_rng$new(n_streams = np, seed = 1L)
  ## running draws 6 numbers per particle:
  y <- drop(mod$run(10))
  expect_equal(y, apply(exp(rng$normal(6, 0, 0.1)), 2, prod))

  ## reset and rerun, draw another set:
  mod$update_state(time = 0)
  y <- drop(mod$run(10))
  expect_equal(y, apply(exp(rng$normal(6, 0, 0.1)), 2, prod))

  mod$update_state(time = 0)
  mod$set_stochastic_schedule(NULL)
  y <- drop(mod$run(10))
  expect_equal(y, rep(1, np))
})

test_that("Basic threading test", {
  path <- mode_file("examples/parallel.cpp")
  gen <- mode(path, quiet = TRUE)

  obj <- gen$new(list(sd = 1), 0, 10, n_threads = 2L, seed = 1L)
  obj$set_index(c(hasopenmp = 1L, threadnum = 2L))
  res <- obj$run(1)
  expect_true(all(res["hasopenmp", ] == obj$has_openmp()))
  if (obj$has_openmp()) {
    expect_equal(sum(res["threadnum", ] == 0), 5)
    expect_equal(sum(res["threadnum", ] == 1), 5)
  } else {
    expect_equal(sum(res["threadnum", ] == -1), 10)
  }
  ## And again without parallel
  obj <- gen$new(list(sd = 1), 0, 10, n_threads = 1L, seed = 1L)
  obj$set_index(c(hasopenmp = 1L, threadnum = 2L))
  res <- obj$run(1)
  expect_true(all(res["hasopenmp", ] == obj$has_openmp()))
  if (obj$has_openmp()) {
    expect_equal(sum(res["threadnum", ] == 0), 10)
  } else {
    expect_equal(sum(res["threadnum", ] == -1), 10)
  }
})

test_that("Can change the number of threads after initialisation", {
  ex <- example_logistic()
  np <- 5
  mod <- ex$generator$new(ex$pars, 0, np)
  expect_equal(withVisible(mod$set_n_threads(2)),
               list(value = 1L, visible = FALSE))
  expect_equal(mod$n_threads(), 2L)
  expect_equal(withVisible(mod$set_n_threads(1)),
               list(value = 2L, visible = FALSE))
})

test_that("Can't change to an impossible thread count", {
  ex <- example_logistic()
  np <- 5
  mod <- ex$generator$new(ex$pars, 0, np)
  expect_error(mod$set_n_threads(0),
               "'n_threads' must be positive")
  expect_error(mod$set_n_threads(-1),
               "'n_threads' must be positive")
})

test_that("Can get openmp support", {
  ex <- example_logistic()
  mod <- ex$generator$new(ex$pars, 0, 5)
  expect_equal(mod$has_openmp(), dust::dust_openmp_support()$has_openmp)
})


test_that("can get rng state", {
  gen <- mode(mode_file("examples/stochastic.cpp"), quiet = TRUE)
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 200, v = 0.1)
  mod <- gen$new(pars, 0, 10, seed = 1L)
  rng <- mod$rng_state()
  expect_type(rng, "raw")
  expect_identical(rng, dust::dust_rng$new(1, 10)$state())
  expect_identical(mod$rng_state(first_only = TRUE),
                   rng[seq_len(32)])
  expect_error(
    mod$rng_state(first_only = TRUE, last_only = TRUE),
    "Only one of 'first_only' or 'last_only' may be TRUE")
  expect_error(
    mod$rng_state(last_only = TRUE),
    "'last_only' not supported for mode models")
})


test_that("can set rng state into model", {
  gen <- mode(mode_file("examples/stochastic.cpp"), quiet = TRUE)
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 200, v = 0.1)
  mod1 <- gen$new(pars, 0, 10, seed = 1L)
  mod2 <- gen$new(pars, 0, 10, seed = 2L)
  mod1$set_stochastic_schedule(0:10)
  mod2$set_stochastic_schedule(0:10)
  mod1$set_rng_state(mod2$rng_state())
  y1 <- mod1$run(11)
  y2 <- mod2$run(11)
  expect_identical(y1, y2)
  expect_identical(mod1$rng_state(), mod2$rng_state())
})


test_that("Can get information about steps", {
  gen <- mode(mode_file("examples/stochastic.cpp"), quiet = TRUE)
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 200, v = 0.5)
  n_particles <- 5L
  control <- mode_control(debug_record_step_times = TRUE)
  mod <- gen$new(pars, 0L, n_particles, control = control, seed = 1L)
  stats <- mod$statistics()
  schedule <- seq(0, 5, length.out = 11)
  mod$set_stochastic_schedule(schedule)

  ## As usual:
  expect_equal(dim(stats), c(3, n_particles))
  expect_equal(row.names(stats),
               c("n_steps", "n_steps_accepted", "n_steps_rejected"))
  expect_s3_class(stats, "mode_statistics")
  expect_true(all(stats == 0))

  ## But we also have this:
  expect_equal(attr(stats, "step_times"), rep(list(numeric(0)), n_particles))

  mod$run(10)

  stats <- mod$statistics()
  steps <- attr(stats, "step_times")
  expect_equal(lengths(steps), stats["n_steps_accepted", ])
  ## Only end points of the steps are included:
  expect_false(0 %in% steps)
  ## But every point in the stochastic schedule is required:
  all(vapply(steps, function(s) all(schedule[-1] %in% s), TRUE))
})
