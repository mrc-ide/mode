##' Create a mode model from a C++ input file.  This function will
##' compile the mode support around your model and return an object
##' that can be used to work with the model.
##'
##' @title Create a mode model from a C++ input file
##'
##' @param filename The path to a single C++ file
##'
##' @param quiet Logical, indicating if compilation messages from
##'   \code{pkgbuild} should be displayed.  Error messages will be
##'   displayed on compilation failure regardless of the value used.
##'
##' @param workdir Optional working directory to use.  If \code{NULL}
##'   uses a temporary directory.  By using a different directory of
##'   your choosing you can see the generated code.
##'
##' @param skip_cache Logical, indicating if the cache of previously
##'   compiled models should be skipped. If `TRUE` then your model will
##'   not be looked for in the cache, nor will it be added to the
##'   cache after compilation.
##'
##' @export
##' @return A generator object based on your source files
mode <- function(filename, quiet = FALSE, workdir = NULL, skip_cache = FALSE) {
  ## TODO: 'mode' is actually a terrible name for this as it conflicts
  ## with stats::mode, but something to think about later perhaps.
  stopifnot(file.exists(filename))

  config <- parse_metadata(filename)

  if (!skip_cache && !is.null(cache[[config$base]])) {
    pkg <- cache[[config$base]]
  } else {
    pkg <- compile_mode(filename, config, workdir, quiet)
    if (!skip_cache) {
      cache[[config$base]] <- pkg
    }
  }

  pkg$env[[config$name]]
}

mode_control <- function(max_steps = NULL, atol = NULL, rtol = NULL,
                         step_size_min = NULL, step_size_max = NULL) {
  ctl <- list(max_steps = max_steps, atol = atol, rtol = rtol,
              step_size_min = step_size_min, step_size_max = step_size_max)
  class(ctl) <- "mode_control"
  ctl
}

compile_mode <- function(filename, config, workdir, quiet) {
  path <- generate_mode(filename, config, workdir)
  pkgbuild::compile_dll(path, compile_attributes = TRUE,
                        quiet = quiet, debug = FALSE)
  pkg <- pkgload::load_all(path, compile = FALSE, recompile = FALSE,
                           warn_conflicts = FALSE, export_all = FALSE,
                           helpers = FALSE, attach_testthat = FALSE,
                           quiet = quiet)
  ## Don't pollute the search path
  detach(paste0("package:", config$base), character.only = TRUE)
  pkg
}


generate_mode <- function(filename, config, workdir) {
  model <- read_lines(filename)

  ## This will likely become quite complicated and need to move into
  ## its own thing.
  data <- list(model = model,
               name = config$name,
               class = config$class,
               base = config$base,
               path_mode_include = mode_file("include"))

  path <- mode_workdir(workdir)
  dir.create(file.path(path, "R"), FALSE, TRUE)
  dir.create(file.path(path, "src"), FALSE, TRUE)

  substitute_mode_template(data, "DESCRIPTION",
                           file.path(path, "DESCRIPTION"))
  substitute_mode_template(data, "NAMESPACE",
                           file.path(path, "NAMESPACE"))
  substitute_mode_template(data, "Makevars",
                           file.path(path, "src", "Makevars"))
  substitute_mode_template(data, "mode.R.template",
                           file.path(path, "R/mode.R"))
  substitute_mode_template(data, "mode.cpp",
                           file.path(path, "src/mode.cpp"))

  path
}


substitute_template <- function(data, src, dest) {
  template <- read_lines(src)
  txt <- glue_whisker(template, data)
  ## TODO: dust avoids writing out unchanged files
  writeLines(txt, dest)
}


substitute_mode_template <- function(data, src, dest) {
  substitute_template(data, mode_file(file.path("template", src)), dest)
}


mode_workdir <- function(path) {
  if (is.null(path)) {
    path <- tempfile()
  } else if (file.exists(path)) {
    ## TODO: see dust for a nice pattern to allow safely overwriting
    ## files.  For now we'll require that the directory does not
    ## exist.
    stop(sprintf("path '%s' already exists", path))
  }
  path
}


cache <- new.env(parent = emptyenv())
