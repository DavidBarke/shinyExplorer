person_explorer_class_contextmenu_item_ui <- function(id) {
  ns <- NS(id)

  contextmenu_item(
    inputId = ns("add"),
    label = "New person"
  )
}

person_explorer_class_specific_contextmenu_items_ui <- function(id) {
  ns <- NS(id)

  htmltools::tagList(
    rename_contextmenu_item_ui(
      id = ns("rename")
    )
  )
}

person_explorer_class_server <- function(
  input, output, session, .values, .explorer_rvs
) {

  shiny::observeEvent(input$add, {
    name <- randomNames(return.complete.data = TRUE)

    .explorer_rvs$current_node$add_child(
      explorer_class_id = "person",
      object = Person$new(name$first_name, name$last_name, "user")
    )
  })

  shiny::callModule(
    module = rename_contextmenu_item,
    .values = .values,
    .explorer_rvs = .explorer_rvs
  )
}
