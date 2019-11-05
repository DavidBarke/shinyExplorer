#' Show Contextmenu
#'
#' This causes a contextmenu to be displayed in the client browser, and is
#' typically used with \code{\link{contextmenu}}. \code{\link{use_contextmenu}}
#' must be called somewhere in your UI, prior to calling this function.
#'
#' @param contextmenu A \code{\link{contextmenu}}.
#' @param session The \code{session} object passed fo function given to
#' \code{shinyServer}.
#'
#' @export
show_contextmenu <- function(contextmenu, session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage("show_contextmenu", list(
    content = shiny:::processDeps(contextmenu, session)
  ))
}

#' Use Contextmenu
#'
#' Call this function in your UI to enable the functionality of
#' \code{\link{show_contextmenu}}.
#'
#' @export
use_contextmenu <- function() {
  htmltools::htmlDependency(
    name = "contextmenu",
    package = "shinyExplorer",
    version = packageVersion("shinyExplorer"),
    src = "contextmenu",
    script = "js/contextmenu.js",
    stylesheet = "css/styles.css"
  )
}

#' Contextmenu
#'
#' This creates the UI representing a contextmenu at absolute position
#' (\code{x}, \code{y}). Contextmenus are typically displayed after the user
#' clicked UI using the right mouse button. Contextmenus consist of
#' \code{\link{contextmenu_item}} and \code{\link{contextmenu_hr}}.
#'
#' @param x,y \code{\link[base:integer]{Integer}}. Contextmenus upper left corner
#' is displayed at (\code{x} px, \code{y} px).
#' @param ... UI elements to include in the contextmenu, typically \code{contextmenu_item}
#' and \code{contextmenu_hr}.
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
#' A \code{\link{contextmenu}} consists of multiple \code{contextmenu_item}. These
#' are in fact \code{\link[shiny:actionButton]{actionLinks}} with special styling.
#'
#' @param inputId The \code{input} slot that will be used to access the value.
#' @param label The content of the button or link - usually a text label, but you
#' could also use any other HTML, like an image.
#' @param icon An optional \code{\link[shiny:icon]{icon}} to appear on the
#' contextmenu_item.
#' @param ... Named attributes to be applied to the
#' \code{\link[shiny:actionButton]{actionLink}}.
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
#' This is just a wrapper for \code{\link[htmltools:hr]{htmltools::hr}}. Use
#' horizontal rules to structure a \code{\link{contextmenu}}.
#'
#' @export
contextmenu_hr <- function() {
  htmltools::hr()
}
