contextmenu_item_ui_factory <- function(
  .label_list = label_group_explorer_class()
) {
  function(id) {
    ns <- shiny::NS(id)

    htmltools::tagList(
      contextmenu_item(
        inputId = ns("add"),
        label = .label_list$add
      )
    )
  }
}

group_node_specific_contextmenu_items_ui_factory <- function(
  .label_list = label_group_explorer_class()
) {
  function(id) {
    ns <- shiny::NS(id)

    htmltools::tagList(
      contextmenu_item(
        inputId = ns("open"),
        label = .label_list$open
      ),
      contextmenu_item(
        inputId = ns("rename"),
        label = .label_list$rename
      )
    )
  }
}

group_node <- function(
  input, output, session, .values, .explorer_rvs,
  .label_list = label_group_explorer_class()
) {

  ns <- session$ns

  shiny::observeEvent(input$add, {
    .explorer_rvs$current_node$add_child(
      explorer_class_id = "__group__",
      object = GroupObject$new(
        name = .label_list$new_group_name
      )
    )
  })

  shiny::observeEvent(input$open, {
    .explorer_rvs$current_node <- .explorer_rvs$contextmenued_node
  })

  shiny::observeEvent(input$rename, {
    shiny::showModal(shiny::modalDialog(
      easyClose = TRUE,
      title = .label_list$rename,
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
      ui <- shiny::actionButton(
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

  return_list <- list(
  )

  return(return_list)
}
