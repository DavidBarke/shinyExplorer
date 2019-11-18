library(shiny)
library(shinyExplorer)

ui <- fluidPage(

)

server <- function(input, output, session) {

  group <- group_explorer_class()

  .values <- list(
    tree = shiny::isolate(ExplorerTree$new("tree", "root")),
    explorer_classes = list(
      group
    )
  )

  shiny::callModule(
    module = explorer,
    .values = .values,
    .root_node_r = shiny::reactive({
      .values$tree$get_root_node()
    }),
    .explorer_classes =
  )
}

shinyApp(ui, server)
