## TODO: see dust's metadata.R for a much more configurable section
## here.  For now, we'll just work out the class name using dust's
## heuristic and not do anything clever.
parse_metadata <- function(filename) {
  class <- parse_metadata_guess_class(readLines(filename))
  list(name = class,
       class = class)
}


parse_metadata_guess_class <- function(txt) {
  re <- "^\\s*class\\s+([^{ ]+)\\s*(\\{.*|$)"
  i <- grep(re, txt)
  if (length(i) != 1L) {
    stop("Could not automatically detect class name; add [[dust::class()]]?")
  }
  sub(re, "\\1", txt[[i]])
}
