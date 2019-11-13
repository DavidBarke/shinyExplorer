#' @name rename_contextmenu_item
#'
#' @export
rename_contextmenu_item_ui <- function(id, .label_list) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    contextmenu_item(
      inputId = ns("rename"),
      label = .label_list$start_rename
    )
  )
}

#' Contextmenu item for renaming
#'
#' Clicking on this contextmenu item starts the renaming process for the
#' associated object of the contextmenued object of class \code{\link{ExplorerNode}}.
#'
#' @param input,output,session Calley by \code{\link[shiny]{callModule}}.
#' @inheritParams explorer_body
#' @param .label_list A list created with \code{\link{label_rename_contextmenu_item}}
#' containing labels for all UI elements used inside this module.
#'
#' @export
rename_contextmenu_item <- function(
  input, output, session, .values, .parent, .explorer_rvs, .label_list
) {

  ns <- session$ns

  self <- QWUtils::Node$new(ns("rename_contextmenu_item"), .parent, session)

  shiny::observeEvent(input$rename, {
    shiny::showModal(shiny::modalDialog(
      easyClose = TRUE,
      size = "s",
      title = .label_list$title_rename,
      shiny::textInput(
        inputId = ns("new_name"),
        label = NULL,
        value = .explorer_rvs$contextmenued_node$get_object()$get_name(),
        placeholder = "Name"
      ),
      footer = shiny::uiOutput(
        outputId = ns("footer")
      )
    ))
  })

  output$footer <- shiny::renderUI({
    if (length(input$new_name) == 0 || nchar(input$new_name) == 0) {
      ui <- NULL
    } else {
      ui <- QWUtils::actionButtonQW(
        inputId = ns("confirm_rename"),
        label = .label_list$confirm_rename
      )
    }

    ui
  })

  shiny::observeEvent(input$confirm_rename, {
    .explorer_rvs$contextmenued_node$get_object()$set_name(input$new_name)
    shiny::removeModal()
  })
}
