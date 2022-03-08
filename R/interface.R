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
##' @export
##' @return A generator object based on your source files
mode <- function(filename, quiet = FALSE, workdir = NULL) {
  stopifnot(file.exists(filename))

  res <- generate_mode(filename, quiet, workdir)

  path <- res$path

  pkgbuild::compile_dll(path, compile_attributes = TRUE,
                        quiet = quiet, debug = FALSE)
  tmp <- pkgload::load_all(path, compile = FALSE, recompile = FALSE,
                           warn_conflicts = FALSE, export_all = FALSE,
                           helpers = FALSE, attach_testthat = FALSE,
                           quiet = quiet)
  ## Don't pollute the search path
  detach(paste0("package:", res$data$base), character.only = TRUE)

  res$dll <- file.path(path, "src", paste0(res$key, .Platform$dynlib.ext))
  res$env <- tmp$env
  res$gen <- res$env[[res$data$name]]
}


generate_mode <- function(filename, quiet, workdir) {
  ## It has been useful to allow metadata in the source code (in the
  ## form of C++11 psedo-attributes to control aspects of the code
  ## generation.  This is going to be important as soon as we allow
  ## for parameters to be changable, so I've supported it
  ## immediately...
  config <- parse_metadata(filename)
  hash <- hash_file(filename)

  ## TODO: See dust - there's some tricks here to allow nasty names
  ## through by sanitising or simply replacing with "mode".  The
  ## mangling (adding the hash) also needs disabling when building for
  ## a package.
  base <- paste0(config$name, hash)

  model <- read_lines(filename)

  ## This will likely become quiet complicated and need to move into
  ## its own thing.
  data <- list(model = model,
               name = config$name,
               class = config$class,
               base = base,
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

  code <- mode_code(data, config)
  writeLines(code$r, file.path(path, "R/mode.R"))
  writeLines(code$cpp, file.path(path, "src/mode.cpp"))

  list(key = base, data = data, path = path)
}


mode_code <- function(data, config) {
  mode_r <- substitute_mode_template(data, "mode.R.template", NULL)
  mode_cpp <- substitute_mode_template(data, "mode.cpp", NULL)
  list(r = mode_r,
       cpp = mode_cpp)
}


substitute_template <- function(data, src, dest) {
  template <- read_lines(src)
  txt <- glue_whisker(template, data)
  if (is.null(dest)) {
    return(txt)
  }
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
