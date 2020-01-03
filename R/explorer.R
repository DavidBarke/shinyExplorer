#' @name explorer
#'
#' @param id The module's id.
#'
#' @importFrom utils packageVersion
#'
#' @export
explorer_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    htmltools::htmlDependency(
      name = "explorer",
      package = "shinyExplorer",
      version = utils::packageVersion("shinyExplorer"),
      src = "srcjs/explorer",
      stylesheet = "css/styles.css"
    ),
    use_contextmenu(),
    htmltools::div(
      class = "explorer",
      # Header contains links to all ancestor nodes of the current node
      shiny::uiOutput(
        outputId = ns("header")
      ),
      # Body contains links to all child nodes of the current node
      explorer_body_ui(
        id = ns("id_explorer_body")
      )
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
#' @param .root_node_r A \code{\link[shiny:reactive]{reactive}} returning an
#' object of class \code{\link{ExplorerNode}}. This needs not necessarily to be the
#' root node of an object of class \code{\link{ExplorerTree}}.
#' @param .explorer_classes A \code{\link[base]{list}} of objects of class
#' \code{\link{ExplorerClass}}.
#' @param .addable_explorer_classes_r A \code{\link[shiny:reactive]{reactive}}
#' returning a \code{\link[base:character]{character}} vector containing the ids
#' of explorer classes that are addable to the explorer.
#' @param .visible_explorer_classes_r A \code{\link[shiny:reactive]{reactive}}
#' returning a \code{\link[base:character]{character}} vector containing the ids
#' of explorer classes that are displayed to the user. Group nodes are always
#' displayed.
#' @param .display_header If \code{\link[base:logical]{TRUE}}, the navigation
#' header is displayed, otherwise it is not.
#' @param .label_list A \code{\link[base]{list}} created with \code{\link{label_explorer}}
#' containing labels for all buttons used inside the explorer module.
#' @param .state A \code{\link[base]{list}} which is passed to every explorer
#' class server function. Use this list to implement special behaviour of an
#' explorer class dependent on conditions outside of the explorer.
#'
#' @return The \code{explorer} module returns a list containing the following reactives, that
#' you may access in the calling server function.
#' \tabular{ll}{
#'   \code{contextmenued_node_r} \tab An object of class \code{\link{ExplorerNode}}. This is
#'   the node, which has been last contextmenued. \cr
#'   \code{current_node_r} \tab An object of class \code{\link{ExplorerNode}}. This is the node,
#'   whose children are currently displayed in the explorer's datatable.
#'   \code{selected_child_node_r} An object of class \code{\link{ExplorerNode}}. This is the
#'   node, which has been last clicked.
#' }
#'
#' @export
explorer <- function(
  input, output, session, .values, .root_node_r, .explorer_classes = list(),
  .addable_explorer_classes_r = shiny::reactive("__group__"),
  .visible_explorer_classes_r = shiny::reactive("__group__"),
  .display_header = TRUE, .label_list = label_explorer(), .state = list()
) {

  ns <- session$ns

  rvs <- shiny::reactiveValues(
    current_node = NULL,
    contextmenued_node = NULL,
    # named character vector storing the ids of the server functions of all
    # explorer classes in .explorer_classes
    module_ids = character()
  )

  # Establish reactive conntection between .root_node_r() and rvs$current_node
  shiny::observe({
    rvs$current_node <- .root_node_r()
  })

  # MODULE CONTENT -------------------------------------------------------------

  children_r <- shiny::reactive({
    rvs$current_node$get_children()$get_objects()
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

  # CHECK IF ALL NEEDED EXPLORER CLASSES ARE PRESENT
  shiny::observeEvent(TRUE, {
    needed_classes <- base::union(
      .addable_explorer_classes_r(),
      .visible_explorer_classes_r()
    )

    present_classes <- purrr::map_chr(.explorer_classes, function(class) {
      class$id
    })

    missing_classes <- base::setdiff(needed_classes, present_classes)

    if (length(missing_classes) > 0) {
      msg <- paste(
        "Explorer: explorer classes are requested to be addable or visible, but
        .explorer_classes is missing explorer classes with the following ids:",
        paste(missing_classes, collapse = ", ")
      )

      stop(msg)
    }
  })

  # HANDLE EXPLORER CLASSES ----------------------------------------------------
  # Explorer class returns is a list. Each element is a return list of an
  # explorer class server function.
  explorer_class_returns <- purrr::map(.explorer_classes, function(explorer_class) {
    # Call explorer_classes' server functions and store their return list in the
    # explorer_class as well as the namespaced module id, so that the UI functions
    # may be called in nested modules
    module_id = "explorer_class" %_% explorer_class$id

    # Store module_id in .explorer_rvs. .explorer_rvs is made available in all
    # explorer_xxx modules and in the server functions of the explorer_classes.
    shiny::isolate({
      rvs$module_ids[explorer_class$id] <- ns(module_id)
    })

    explorer_class_return <- shiny::callModule(
      module = explorer_class$server,
      id = module_id,
      .values = .values,
      .explorer_rvs = rvs,
      .state = .state
    )

    # If not implementen in server return list is_group_r defaults to FALSE
    if (purrr::is_null(explorer_class_return$is_group_r)) {
      explorer_class_return$is_group_r <- shiny::reactive(FALSE)
    }

    explorer_class_return
  })

  names(explorer_class_returns) <- purrr::map_chr(.explorer_classes, function(explorer_class) {
    explorer_class$id
  })

  # CALL MODULES AND HANDLING OF RETURNS ---------------------------------------
  explorer_header_return <- shiny::callModule(
    module = explorer_header,
    id = "id_explorer_header",
    .values = .values,
    .explorer_classes = .explorer_classes,
    .explorer_class_returns = explorer_class_returns,
    .explorer_rvs = rvs,
    .root_node_r = .root_node_r
  )

  explorer_body_return <- shiny::callModule(
    module = explorer_body,
    id = "id_explorer_body",
    .values = .values,
    .children_r = children_r,
    .root_node_r = .root_node_r,
    .explorer_classes = .explorer_classes,
    .explorer_class_returns = explorer_class_returns,
    .explorer_rvs = rvs,
    .addable_explorer_classes_r = .addable_explorer_classes_r,
    .visible_explorer_classes_r = .visible_explorer_classes_r,
    .label_list = .label_list
  )

  return_list <- list(
    rvs = rvs,
    contextmenued_node_r = shiny::reactive(rvs$contextmenued_node),
    current_node_r = shiny::reactive(rvs$current_node),
    # Selected means, that user clicked this node, but didn't dblclick it, to be
    # the new current node
    selected_child_node_r = explorer_body_return$selected_node_r,
    explorer_class_returns = explorer_class_returns
  )

  return(return_list)
}
