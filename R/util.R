`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


system_file <- function(...) {
  system.file(..., mustWork = TRUE)
}


mode_file <- function(path) {
  system_file(path, package = "mode")
}


hash_file <- function(path, short = TRUE) {
  hash <- unname(tools::md5sum(path))
  if (short) {
    hash <- substr(hash, 1, 8)
  }
  hash
}


read_lines <- function(path) {
  paste(readLines(path), collapse = "\n")
}


glue_whisker <- function(template, data) {
  stopifnot(length(template) == 1L)
  glue::glue(template, .envir = data, .open = "{{", .close = "}}",
             .trim = FALSE)
}
