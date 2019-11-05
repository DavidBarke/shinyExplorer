#' @export
explorer_selector_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::uiOutput(
      outputId = ns("caption")
    ),
    DT::dataTableOutput(
      outputId = ns("selected_node")
    )
  )
}

#' Explorer Selector
#'
#' Use the \code{\link{explorer}} module to select a node.
#'
#' @param input,output,session Called by \code{\link[shiny:callModule]{callModule}}.
#' @param .values The .values list.
#' @param .parent The parent \code{Node} object.
#' @param .root_node_r A \code{\link[shiny:reactive]{reactive}} returning an
#' object of class \code{\link{ExplorerNode}}.
#' @param .group_nodes_selectable \code{\link[base:logical]{Logical}} indicating
#' whether group nodes are selectable or not.
#' @param .selectable_explorer_classes_r A \code{\link[shiny:reactive]{reactive}}
#' returning a \code{\link[base:character]{character}} vector containing the ids
#' of selectable explorer_classes. If \code{character()}, only group nodes are
#' selectible (if \code{.group_nodes_selectable = TRUE}).
#' @param .addable_explorer_classes_r A \code{\link[shiny:reactive]{reactive}}
#' returning a \code{\link[base:character]{character}} vector containing the ids
#' of explorer classes that are addable to the explorer.
#' @param .visible_explorer_classes_r A \code{\link[shiny:reactive]{reactive}}
#' returning a \code{\link[base:character]{character}} vector containing the ids
#' of explorer classes, that are displayed to the user. Group nodes are always
#' displayed.
#'
#' @export
explorer_selector <- function(
  input, output, session, .values, .parent, .root_node_r,
  .group_nodes_selectable = FALSE,
  .selectable_explorer_classes_r = shiny::reactive(character()),
  .addable_explorer_classes_r = .selectable_explorer_classes_r,
  .visible_explorer_classes_r = .selectable_explorer_classes_r
) {

  ns <- session$ns

  self <- QWUtils::Node$new(ns("node_name"), .parent, session)

  rvs <- shiny::reactiveValues(
    selected_node = NULL
  )

  # SELECTED NODE DATATABLE ----------------------------------------------------

  selected_node_datatable <- shiny::reactive({
    if (purrr::is_null(rvs$selected_node)) {
      data <- tibble(id = character(), icon = character(), name = character())
      names(data) <- c("id", "", "Name")
      return(data)
    }

    id_col <- rvs$selected_node$get_id()
    name_col <- rvs$selected_node$get_object()$get_name()

    explorer_class <- rvs$selected_node$get_explorer_class()

    icon_col <- if (rvs$selected_node$is_group_node()) {
      as.character(shiny::icon("folder"))
    } else if (purrr::is_null(explorer_class$server_return$icon_r)) {
      ""
    } else {
      as.character(explorer_class$server_return$icon_r())
    }

    data <- tibble::tibble(
      id = id_col,
      icon = icon_col,
      name = name_col
    )

    names(data) <- c("id", "", "Name")

    data
  })

  output$caption <- shiny::renderUI({
    if (purrr::is_null(rvs$selected_node)) {
      text <- QWUtils::label_lang(
        de = "Wähle ein Element",
        en = "Select element"
      )
    } else {
      text <- QWUtils::label_lang(
        de = "Ausgewähltes Element",
        en = "Selected element"
      )
    }

    ui <- htmltools::tagList(
      htmltools::tags$span(text),
      QWUtils::actionButtonQW(
        inputId = ns("select_node"),
        label = NULL,
        icon = shiny::icon("search"),
        tooltip = QWUtils::label_lang(
          de = "Element auswählen",
          en = "Select element"
        )
      )
    )
  })

  output$selected_node <- DT::renderDataTable({
    DT::datatable(
      data = selected_node_datatable(),
      options = list(
        dom = "t",
        columnDefs = list(
          list(
            visible = FALSE,
            targets = 0
          )
        )
      ),
      callback = datatable_callback(ns("selected_node")),
      escape = FALSE,
      rownames = FALSE
    )
  })

  shiny::observeEvent(input$selected_node_row_contextmenued, {
    # The implementation of this observer is based on the contextmenu of the
    # explorer_body module. But since the selected_node datatable will always
    # contain only one node, this node shouldn't be removable and the user is
    # not allowed to add new nodes, this is a simplified version.
    id <- input$selected_node_row_contextmenued$data[[1]]

    if (purrr::is_null(id)) {
      node <- NULL
    } else {
      # Get contextmenued node
      node <- rvs$selected_node
    }

    # Store reference to contextmenued node in .explorer_rvs, so that is readable
    # in user submitted explorer_classes. If the table is empty, the contextmenued
    # node is set to NULL
    explorer_return$rvs$contextmenued_node <- node

    if (purrr::is_null(node)) {
      # No specific contextmenu_items to display
      class_specific_contextmenu_items <- NULL
    } else if (node$is_group_node()) {
      # Display group specific contextmenu_items
      class_specific_contextmenu_items <- group_node_specific_contextmenu_items_ui(
        id = ns("id_group_node")
      )
    } else {
      explorer_class <- node$get_explorer_class()

      class_specific_contextmenu_items <- htmltools::tagList(
        explorer_class$ui$specific_contextmenu_items_ui(
          id = explorer_return$rvs$module_ids[explorer_class$id]
        )
      )
    }

    show_contextmenu(
      contextmenu(
        x = input$selected_node_row_contextmenued$mouse$x,
        y = input$selected_node_row_contextmenued$mouse$y,
        class_specific_contextmenu_items
      )
    )
  })

  # MODAL DIALOG ---------------------------------------------------------------

  shiny::observeEvent(input$select_node, {
    shiny::showModal(shiny::modalDialog(
      size = "s",
      easyClose = TRUE,
      explorer_ui(
        id = ns("id_explorer")
      ),
      footer = shiny::uiOutput(
        outputId = ns("footer")
      )
    ))
  })

  is_selectable_r <- shiny::reactive({
    selected_child_node <- shiny::req(explorer_return$selected_child_node_r())

    if (selected_child_node$is_group_node()) {
      if (.group_nodes_selectable) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }

    current_explorer_class <- selected_child_node$get_explorer_class()$id

    if (current_explorer_class %in% .selectable_explorer_classes_r()) {
      return(TRUE)
    }

    return(FALSE)
  })

  output$footer <- shiny::renderUI({
    if (is_selectable_r()) {
      ui <- htmltools::tagList(
        QWUtils::actionButtonQW(
          inputId = ns("confirm_selection"),
          label = QWUtils::label_lang(
            de = "Auswahl bestätigen",
            en = "Confirm selection"
          )
        )
      )
    } else {
      ui <- NULL
    }

    ui
  })

  shiny::observeEvent(input$confirm_selection, {
    rvs$selected_node <- explorer_return$selected_child_node_r()
    shiny::removeModal()
  })

  # CALL MODULES ---------------------------------------------------------------

  explorer_return <- shiny::callModule(
    module = explorer,
    id = "id_explorer",
    .values = .values,
    .parent = self,
    .root_node_r = .root_node_r,
    .addable_explorer_classes_r = .addable_explorer_classes_r,
    .visible_explorer_classes_r = .visible_explorer_classes_r
  )

  return_list <- list(
    selected_node_r = shiny::reactive({
      shiny::req(rvs$selected_node)
    })
  )

  return(return_list)
}



