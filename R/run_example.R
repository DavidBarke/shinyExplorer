#' Run shinyExplorer examples
#'
#' Launch a \code{shinyExplorer} example Shiny app thta shows how to use
#' \code{shinyExplorer} in an app.
#'
#' Call this function without arguments to see a list of available example apps.
#'
#' @param example The name of the example app to launch.
#'
#' @export
run_example <- function(example) {
  validExamples <- paste0(
    "Valid examples are: \"",
    paste(
      list.files(system.file("examples", package = "shinyExplorer")), collapse = "\", \"")
    , "\""
  )
  if (missing(example) || !nzchar(example)) {
    message("Please run `runExample()` with a valid example app as an argument.\n",
            validExamples)
    return(invisible(NULL))
  }
  appDir <- system.file("examples", example, package = "shinyExplorer")
  if (appDir == "") {
    errMsg(sprintf("Could not find example app `%s`\n%s",
                   example, validExamples))
  }
  shiny::runApp(appDir, display.mode = "normal")
}
