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

test_that("Returns full state when no index set", {
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 100)
  n_particles <- 10
  mod <- gen$new(pars, 1, n_particles)
  res <- mod$run(2)
  expect_equal(dim(res), c(2, n_particles))

  state <- mod$state()
  expect_identical(res, state)
})

test_that("Returns state for set index", {
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 100)
  n_particles <- 10
  mod <- gen$new(pars, 1, n_particles)
  mod$set_index(1)
  res <- mod$run(2)
  expect_equal(dim(res), c(1, n_particles))

  state <- mod$state()
  expect_identical(res, state[1, , drop = FALSE])
})

test_that("Can get arbitrary partial state", {
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 100)
  n_particles <- 10
  mod <- gen$new(pars, 1, n_particles)
  res <- mod$run(2)
  expect_equal(dim(res), c(2, n_particles))

  state <- mod$state(1)
  expect_identical(state, res[1, , drop = FALSE])

  state <- mod$state(2)
  expect_identical(state, res[2, , drop = FALSE])

  state <- mod$state()
  expect_identical(state, res)
})

test_that("Error if partial state index is invalid", {
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 100)
  n_particles <- 10
  mod <- gen$new(pars, 1, n_particles)
  expect_error(mod$state(3),
               "All elements of 'index' must lie in [1, 2]",
               fixed = TRUE)
  expect_error(mod$state(c(1, 2, 3)),
               "All elements of 'index' must lie in [1, 2]",
               fixed = TRUE)
})

test_that("Can set vector index", {
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 100)
  n_particles <- 10
  mod <- gen$new(pars, 1, n_particles)
  mod$set_index(c(1, 2))
  res <- mod$run(2)
  expect_equal(dim(res), c(2, n_particles))
})

test_that("Can retrieve index", {
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 100)
  n_particles <- 10
  mod <- gen$new(pars, 1, n_particles)
  expect_equal(NULL, mod$index())
  mod$set_index(c(1, 2))
  expect_equal(c(1, 2), mod$index())
})

test_that("Setting a named index returns names", {
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  r <- c(0.1, 0.2)
  k <- c(100, 100)
  pars <- list(r1 = r[[1]], r2 = r[[2]], K1 = k[[1]], K2 = k[[2]])
  n_particles <- 5
  mod <- gen$new(pars, 0, n_particles)
  analytic <- logistic_analytic(r, k, 1, c(1, 1))
  mod$set_index(c(y1 = 1L, y2 = 2L))
  expect_equal(
    mod$run(1),
    rbind(y1 = rep(analytic[1, ], n_particles),
          y2 = rep(analytic[2, ], n_particles)),
    tolerance = 1e-7)
})

test_that("Can clear index", {
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  r <- c(0.1, 0.2)
  k <- c(100, 100)
  pars <- list(r1 = r[[1]], r2 = r[[2]], K1 = k[[1]], K2 = k[[2]])
  n_particles <- 5
  mod <- gen$new(pars, 0, n_particles)
  analytic <- logistic_analytic(r, k, 1:2, c(1, 1))
  mod$set_index(c(y1 = 1L))
  expect_equal(
    mod$run(1),
    rbind(y1 = rep(analytic[1, 1], n_particles)),
    tolerance = 1e-7)
  expect_equal(mod$index(), c(y1 = 1L))
  mod$set_index(NULL)
  expect_equal(
    mod$run(2),
    rbind(rep(analytic[1, 2], n_particles),
          rep(analytic[2, 2], n_particles)),
    tolerance = 1e-7)
  expect_null(mod$index())
})

test_that("Cannot set invalid index", {
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 100)
  n_particles <- 10
  mod <- gen$new(pars, 1, n_particles)
  expect_error(mod$set_index(0),
               "All elements of 'index' must lie in [1, 2]",
               fixed = TRUE)
  expect_error(mod$set_index(3),
               "All elements of 'index' must lie in [1, 2]",
               fixed = TRUE)
  expect_error(mod$set_index(c(1, 2, 3)),
               "All elements of 'index' must lie in [1, 2]",
               fixed = TRUE)
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
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 100)
  n_particles <- 5
  mod <- gen$new(pars, 1, n_particles)
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

test_that("Can retrieve statistics", {
  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = 0.1, r2 = 0.2, K1 = 100, K2 = 100)
  n_particles <- 5
  mod <- gen$new(pars, 1, n_particles)
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
