#' Positional Input
#'
#' A Shiny input behaving similarly to \code{\link[shiny]{actionButton}}. In
#' addition to \code{input$<inputId>} you may use \code{input$<inputId>_position},
#' which returns a list
#'
#' @param label The contents of the positional input - usually a text label, but you could also use any other HTML, like
#' an image.
#' @inheritParams shiny::actionLink
#'
#' @export
positional_input <- function(inputId, label, icon = NULL) {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "positionalInput",
      package = "shinyExplorer",
      version = utils::packageVersion("shinyExplorer"),
      src = "positionalInput",
      script = "js/positionalInput.js",
      stylesheet = "css/styles.css"
    ),
    htmltools::tags$div(
      id = inputId,
      class = "positional-input",
      `data-value` = 0,
      list(shiny:::validateIcon(icon), label)
    )
  )
}
