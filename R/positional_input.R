positional_input <- function(inputId, label, icon = NULL) {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "positionalInput",
      package = "shinyExplorer",
      version = packageVersion("shinyExplorer"),
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
