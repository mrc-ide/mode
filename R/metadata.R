## TODO: see dust's metadata.R for a much more configurable section
## here.  For now, we'll just work out the class name using dust's
## heuristic and not do anything clever.
parse_metadata <- function(filename) {
  class <- parse_metadata_guess_class(readLines(filename))
  hash <- hash_file(filename)
  name <- class
  base <- paste0(name, hash)
  ## TODO: See dust - there's some tricks here to allow nasty names
  ## through by sanitising or simply replacing with "mode".  The
  ## mangling (adding the hash) also needs disabling when building for
  ## a package.
  list(name = name,
       base = base,
       class = class,
       hash = hash)
}


parse_metadata_guess_class <- function(txt) {
  re <- "^\\s*class\\s+([^{ ]+)\\s*(\\{.*|$)"
  i <- grep(re, txt)
  if (length(i) != 1L) {
    stop("Could not automatically detect class name; add [[dust::class()]]?")
  }
  sub(re, "\\1", txt[[i]])
}
