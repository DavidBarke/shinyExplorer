explorer_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    # Header contains links to all ancestor nodes of the current node
    shiny::uiOutput(
      outputId = ns("header")
    ),
    # Body contains links to all child nodes of the current node
    explorer_body_ui(
      id = ns("id_explorer_body")
    )
  )
}

#' Explorer
#' 
#' Shiny module representing an explorer usable for selecting elements of different
#' kinds and invoking actions on them.
#' 
#' @param input,output,session Called by \code{\link[shiny:callModule]{callModule}}.
#' @param .values The \code{.values} list.
#' @param .parent The parent \code{\link{Node}} object.
#' @param .root_node_r A \code{\link[shiny:reactive]{reactive}} returning an
#' object of class \code{\link{ExplorerNode}}.
#' @param .group_nodes_addable \code{\link[base:logical]{Logical}} indicating
#' whether group nodes are addable or not.
#' @param .addable_explorer_classes_r A \code{\link[shiny:reactive]{reactive}} 
#' returning a \code{\link[base:character]{character}} vector containing the ids
#' of explorer classes that are addable to the explorer.
#' @param .visible_explorer_classes_r A \code{\link[shiny:reactive]{reactive}}
#' returning a \code{\link[base:character]{character}} vector containing the ids
#' of explorer classes that are displayed to the user. Group nodes are always
#' displayed. 
#' @param .display_header If \code{\link[base:logical]{TRUE}}, the navigation
#' header is displayed, otherwise it is not.
#'       
#' @export
explorer <- function(
  input, output, session, .values, .parent, .root_node_r, 
  .group_nodes_addable = TRUE, .addable_explorer_classes_r = shiny::reactive(NULL),
  .visible_explorer_classes_r = shiny::reactive(NULL), .display_header = TRUE
) {
  
  ns <- session$ns
  
  self <- QWUtils::Node$new(ns("explorer"), .parent, session)
  
  rvs <- shiny::reactiveValues(
    current_node = NULL,
    contextmenued_node = NULL,
    # named character vector storing the ids of the server functions of all
    # explorer classes in .values$explorer_classes
    module_ids = character()
  )
  
  # Establish reactive conntection between .root_node_r() and rvs$current_node
  shiny::observe({
    rvs$current_node <- .root_node_r()
  })
  
  # MODULE CONTENT -------------------------------------------------------------
  
  children_r <- shiny::reactive({
    rvs$current_node$children()
  })
  
  output$header <- shiny::renderUI({
    if (.display_header) {
      ui <- explorer_header_ui(
        id = ns("id_explorer_header")
      )
    } else {
      ui <- NULL
    }
    
    ui
  })
  
  # HANDLE EXPLORER CLASSES ----------------------------------------------------
  
  purrr::walk(.values$explorer_classes, function(explorer_class) {
    # Call explorer_classes' server functions and store their return list in the
    # explorer_class as well as the namespaced module id, so that the UI functions
    # may be called in nested modules
    module_id = "explorer_class" %_% explorer_class$id
    
    explorer_class$server_return <- shiny::callModule(
      module = explorer_class$server,
      id = module_id,
      .values = .values,
      .parent = self,
      .explorer_rvs = rvs,
      # Call server function with reference to own explorer class, so that inside
      # of the server function an element with this explorer class can be added
      .explorer_class = explorer_class
    )
    
    # Store module_id in .explorer_rvs. .explorer_rvs is made available in all
    # explorer_xxx modules and in the server functions of the explorer_classes.
    shiny::isolate({
      rvs$module_ids[explorer_class$id] <- ns(module_id)
    })
  })
  
  # CALL MODULES AND HANDLING OF RETURNS ---------------------------------------
  
  shiny::observeEvent(explorer_body_return$node_open_r(), {
    # Explorer body node_open_r returns a list with two elements: node
    # and rnd to guarantee that a node that shall be opened two times in a row
    # will be opened (between these two events occurs a navigation one level up
    # by the user by the explorer header)
    rvs$current_node <- explorer_body_return$node_open_r()$node
  })
  
  explorer_header_return <- shiny::callModule(
    module = explorer_header,
    id = "id_explorer_header",
    .values = .values,
    .parent = self,
    .explorer_rvs = rvs,
    .root_node_r = .root_node_r
  )
  
  explorer_body_return <- shiny::callModule(
    module = explorer_body,
    id = "id_explorer_body",
    .values = .values,
    .parent = self,
    .children_r = children_r,
    .root_node_r = .root_node_r,
    .explorer_rvs = rvs,
    .addable_explorer_classes_r = .addable_explorer_classes_r,
    .visible_explorer_classes_r = .visible_explorer_classes_r
  )
  
  return_list <- list(
    rvs = rvs,
    current_node_r = shiny::reactive({rvs$current_node}),
    # Selected means, that user clicked this node, but didn't dblclick it, to be
    # the new current node
    selected_child_node_r = explorer_body_return$selected_node_r
  )
  
  return(return_list)
}
