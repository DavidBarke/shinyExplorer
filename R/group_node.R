group_node_specific_contextmenu_items_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    contextmenu_item(
      inputId = ns("open"),
      label = QWUtils::label_lang(
        de = "Öffnen",
        en = "Open"
      )
    ),
    contextmenu_item(
      inputId = ns("rename"),
      label = QWUtils::label_lang(
        de = "Umbenennen",
        en = "Rename"
      )
    )
  )
}

group_node <- function(
  input, output, session, .values, .parent, .explorer_rvs
) {
  
  ns <- session$ns
  
  self <- QWUtils::Node$new(ns("group_node"), .parent, session)
  
  shiny::observeEvent(input$rename, {
    shiny::showModal(shiny::modalDialog(
      easyClose = TRUE,
      title = QWUtils::label_lang(
        de = "Umbenennen",
        en = "Rename"
      ),
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
        label = QWUtils::label_lang(
          de = "Umbenennen",
          en = "Rename"
        )
      )
    }
    
    ui
  })
  
  shiny::observeEvent(input$confirm_rename, {
    .explorer_rvs$contextmenued_node$get_object()$set_name(input$new_name)
    shiny::removeModal()
  })
  
  return_list <- list(
    open_group_r = shiny::reactive({input$open})
  )
  
  return(return_list)
}
