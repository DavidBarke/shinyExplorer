#' Show Contextmenu
#'
#' @export
show_contextmenu <- function(contextmenu, session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage("show_contextmenu", list(
    content = shiny:::processDeps(contextmenu, session)
  ))
}

#' Context Menu
#'
#' @export
contextmenu <- function(x, y, ...) {
  htmltools::tagList(
    htmltools::div(
      class = "context-menu",
      style = paste0(
        "left: ", x, "px; ",
        "top: ", y, "px;"
      ),
      htmltools::tags$ul(
        ...
      )
    )
  )
}

#' Contextmenu item
#'
#' @export
contextmenu_item <- function(inputId, label, icon = NULL, ...) {
  htmltools::tags$li(
    class = "context-menu-item",
    shiny::actionLink(
      inputId = inputId,
      label = label,
      icon = icon,
      ...
    )
  )
}

#' Contextmenu Horizontal Rule
#'
#' @export
contextmenu_hr <- function() {
  htmltools::tags$hr()
}
