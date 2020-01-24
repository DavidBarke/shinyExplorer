default_options <- list(
  shinyExplorer.debug = FALSE
)

.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(default_options) %in% names(op))
  if (any(toset)) options(default_options[toset])

  invisible()
}
