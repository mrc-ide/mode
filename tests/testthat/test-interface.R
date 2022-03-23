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
  e <- "end_time (2.000000) must be greater than current time (5.000000)"
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
  expect_error(mod$update_state(c(1, 2, 3, 4, 5)),
               "expected 'time' to be a scalar value")
})
