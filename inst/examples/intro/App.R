library(shiny)
library(shinyExplorer)

ui <- shiny::fluidPage(
  shiny::includeCSS("www/css/styles.css"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      htmltools::p(
        "Right-click an element to select an action."
      ),
      htmltools::p(
        "Double-click an element to open it."
      ),
      shiny::checkboxInput(
        inputId = "group_nodes_addable",
        label = "If selected, group nodes are addable. Group nodes behave like folders.",
        value = TRUE
      )
    ),
    shiny::mainPanel(
      explorer_ui(
        id = "id_explorer"
      )
    )
  )
)

server <- function(input, output, session) {

  group <- group_explorer_class()

  .values <- list(
    tree = shiny::isolate(ExplorerTree$new("root")),
    explorer_classes = list(
      "__group__" = group
    )
  )

  shiny::isolate(.values$tree$get_root_node()$add_child(
    id = "Node",
    object = Object$new("New group")
  ))

  addable_explorer_classes_r <- shiny::reactive({
    if (input$group_nodes_addable) {
      return("__group__")
    } else {
      return(character())
    }
  })

  shiny::callModule(
    module = explorer,
    id = "id_explorer",
    .values = .values,
    .root_node_r = shiny::reactive({
      .values$tree$get_root_node()
    }),
    .explorer_classes = .values$explorer_classes,
    .addable_explorer_classes = addable_explorer_classes_r
  )
}

shinyApp(ui, server, options = list(display.mode = "showcase"))
