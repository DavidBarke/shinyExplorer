#' @param id Module id.
#'
#' @name explorer_body
explorer_body_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::div(
    DT::dataTableOutput(
      outputId = ns("selector_table")
    )
  )
}

#' Explorer Body
#'
#' You usually don't want to call this module explicitly.
#'
#' @param input,output,session Called by \code{\link[shiny]{callModule}}.
#' @param .children_r \code{\link[shiny]{reactive}} containing the child nodes of
#' the current node.
#' @param .explorer_rvs \code{\link[shiny:reactiveValues]{ReactiveValues}}.
#' @inheritParams explorer
#'
#' @details#' \code{.explorer_rvs} must provide the following elements:
#' \tabular{ll}{
#'   \code{current_node} \tab \code{\link{ExplorerNode}}, which is currently
#'   selected in \code{\link{explorer}}. \cr
#'   \code{contextmenued_node} \tab \code{\link{ExplorerNode}}, which has been
#'   last contextmenued. \cr
#'   \code{module_ids} \tab \code{\link[base:list]{List}} with one entry per
#'   unique explorer class.
#' }
#'
#' @importFrom stats runif
explorer_body <- function(
  input, output, session, .values, .children_r, .root_node_r, .explorer_classes,
  .explorer_class_returns, .explorer_rvs, explorer_addable_r,
  explorer_visible_r, .label_list
) {

  ns <- session$ns

  rvs <- shiny::reactiveValues(
    show_contextmenu = FALSE
  )

  current_node_class_return_r <- shiny::reactive({
    .explorer_class_returns[[.explorer_rvs$current_node$get_explorer_class_id()]]
  })

  addable_label_r <- shiny::reactive({
    # Determine addable labels
    # Three cases for addable labels are distinguished:
    # 1. Addable inside the whole explorer
    # 2. Addable as children to a certain explorer class
    # 3. Addable as children to a certain node

    # Case 1
    explorer_addable <- explorer_addable_r()

    # Case 2
    if (shiny::is.reactive(
      current_node_class_return_r()$addable_r)
    ) {
      current_node_class_addable <- current_node_class_return_r()$addable_r()
    } else {
      current_node_class_addable <- NULL
    }

    # Case 3
    current_node <- .explorer_rvs$current_node

    current_node_addable <- current_node$get_addable()

    unique(c(
      explorer_addable,
      current_node_class_addable,
      current_node_addable
    ))
  })

  visible_label_r <- shiny::reactive({
    # Determine visible labels
    # Three cases for visible labels are distinguished:
    # 1. Visible inside the whole explorer
    # 2. Visible as children to a certain explorer class
    # 3. Visible as children to a certain node

    # Case 1
    explorer_visible <- explorer_visible_r()

    # Case 2
    if (shiny::is.reactive(
      current_node_class_return_r()$visible_r)
    ) {
      current_node_class_visible <- current_node_class_return_r()$visible_r()
    } else {
      current_node_class_visible <- NULL
    }

    # Case 3
    current_node <- .explorer_rvs$current_node

    current_node_visible <- current_node$get_visible()

    unique(c(
      explorer_visible,
      current_node_class_visible,
      current_node_visible
    ))
  })

  addable_explorer_class_ids_r <- shiny::reactive({
    is_addable <- purrr::map_lgl(.explorer_classes, function(explorer_class) {
      any(explorer_class$get_labels() %in% addable_label_r())
    })

    names(.explorer_classes)[is_addable]
  })

  visible_children_r <- shiny::reactive({
    is_visible <- purrr::map_lgl(.children_r(), function(child_node) {
      explorer_class <- .explorer_classes[[child_node$get_explorer_class_id()]]

      if (any(explorer_class$get_labels() %in% visible_label_r())) {
        return(TRUE)
      }

      return(FALSE)
    })

    .children_r()[is_visible]
  })

  selector_table_r <- shiny::reactive({
    id_col <- purrr::map_chr(visible_children_r(), function(child_node) {
      child_node$get_id()
    })

    name_col <- purrr::map_chr(visible_children_r(), function(child_node) {
      child_object <- child_node$get_object()

      if (!purrr::is_function(child_object$get_name)) {
        msg <- paste0(
          "explorer: Object of class ", class(child_object), " must possess a method
          with name 'get_name'. Explorer class id: ", child_node$get_explorer_class_id()
        )
        stop(msg)
      }

      child_object$get_name()
    })

    icon_col <- purrr::map_chr(visible_children_r(), function(child_node) {
      child_explorer_class_id <- child_node$get_explorer_class_id()
      child_return <- .explorer_class_returns[[child_explorer_class_id]]

      # If child node is group, the node has explorer_class NULL
      if (child_return$is_group_r()) {
        return(as.character(shiny::icon("folder")))
      }

      # Else the explorer_class' server function might return the reactive
      # icon_r, which returns an icon
      if (purrr::is_null(child_return$icon_r)) {
        return("")
      } else {
        return(as.character(child_return$icon_r()))
      }
    })

    data <- tibble::tibble(
      id = id_col,
      icon = icon_col,
      name = name_col
    )

    names(data) <- c("id", "", "Name")

    data
  })

  output$selector_table <- DT::renderDataTable({
    table <- selector_table_r()

    DT::datatable(
      table,
      options = list(
        dom = "t",
        columnDefs = list(
          list(
            visible = FALSE,
            targets = 0
          )
        ),
        scrollY = "250px",
        paging = FALSE
      ),
      class = "display fill-available",
      rownames = FALSE,
      escape = FALSE,
      callback = datatable_callback(ns("selector_table")),
      selection = "single",
      # editable = list(
      #   target = "cell",
      #   disable = list(
      #     columns = 1
      #   )
      # )
      editable = FALSE
    )
  })

  insert_contextmenu_hr <- function(...) {
    l <- purrr::compact(...)
    x <- list()
    size_x <- 0
    for (i in seq_along(l[-1])) {
      x[[size_x + 1]] <- l[[i]]
      x[[size_x + 2]] <- contextmenu_hr()
     size_x <- size_x + 2
    }
    x[[size_x + 1]] <- l[[length(l)]]
    x
  }

  shiny::observeEvent(input$selector_table_row_contextmenued, {
    id <- input$selector_table_row_contextmenued$data[[1]]

    if (purrr::is_null(id)) {
      node <- NULL
    } else {
      # Get contextmenued node
      node <- .explorer_rvs$current_node$get_child(id)
    }

    # Store reference to contextmenued node in .explorer_rvs, so that is readable
    # in user submitted explorer_classes. If the table is empty, the contextmenued
    # node is set to NULL
    .explorer_rvs$contextmenued_node <- node

    if (purrr::is_null(node)) {
      # No specific contextmenu_items to display
      class_specific_contextmenu_items <- NULL
    } else {
      explorer_class <- .explorer_classes[[node$get_explorer_class_id()]]

      class_specific_contextmenu_items <- htmltools::tagList(
        explorer_class$ui$specific_contextmenu_items_ui(
          id = .explorer_rvs$module_ids[explorer_class$id]
        )
      )
    }

    if (purrr::is_null(node) || !node$is_removable()) {
      remove_contextmenu_item <- NULL
    } else {
      remove_contextmenu_item <- contextmenu_item(
        inputId = ns("remove_node"),
        label = .label_list$delete_node
      )
    }

    add_explorer_class_contextmenu_items <- purrr::map(
      addable_explorer_class_ids_r(),
      function(explorer_class_id) {
        explorer_class <- .explorer_classes[[explorer_class_id]]
        explorer_class$ui$contextmenu_item_ui(
          id = .explorer_rvs$module_ids[explorer_class$id]
        )
      }
    )

    if (getOption("shinyExplorer.debug", FALSE)) {
      debug_menu_items <- htmltools::tagList(
        contextmenu_item(
          inputId = ns("print_node_info"),
          label = "Print node info"
        )
      )
    } else {
      debug_menu_items <- NULL
    }

    # Only show contextmenu if it contains any elements
    if (
      length(c(
        class_specific_contextmenu_items,
        remove_contextmenu_item,
        add_explorer_class_contextmenu_items,
        debug_menu_items
      ))
    ) {
      show_contextmenu(
        contextmenu(
          x = input$selector_table_row_contextmenued$mouse$x,
          y = input$selector_table_row_contextmenued$mouse$y,
          insert_contextmenu_hr(
            class_specific_contextmenu_items,
            remove_contextmenu_item,
            add_explorer_class_contextmenu_items,
            debug_menu_items
          )
        )
      )
    }
  })

  shiny::observeEvent(input$print_node_info, {
    node <- .explorer_rvs$current_node

    print("Node Info")
    print(paste("Id:", node$get_id()))
    print(paste("Explorer class id:", node$get_explorer_class_id()))
    print(paste("Addable:", node$get_addable()))
    print(paste("Visible:", node$get_visible()))
  })

  shiny::observeEvent(input$remove_node, {
    shiny::showModal(shiny::modalDialog(
      title = .label_list$confirm_delete,
      footer = shiny::actionButton(
        inputId = ns("confirm_remove"),
        label = .label_list$confirm_delete
      ),
      easyClose = TRUE
    ))
  })

  shiny::observeEvent(input$confirm_remove, {
    shiny::removeModal()

    target_node <- .explorer_rvs$contextmenued_node

    explorer_class <- .explorer_classes[[target_node$get_explorer_class_id()]]

    explorer_class$on_remove(target_node)

    .explorer_rvs$current_node$remove_child(
      id = .explorer_rvs$contextmenued_node$get_id()
    )
  })

  shiny::observeEvent(input$selector_table_row_dblclicked, {
    shiny::req(length(input$selector_table_row_dblclicked) > 0)
    # If "No data available in table", nothing shall happen
    id <- shiny::req(input$selector_table_row_dblclicked$data[[1]])

    explorer_class_id <- .root_node_r()$get_node(id)$get_explorer_class_id()

    explorer_class_return <- .explorer_class_returns[[explorer_class_id]]

    # Only nodes of group explorer class may have children
    if (explorer_class_return$is_group_r()) {
      # First column of selector table contains child node's id
      .explorer_rvs$current_node <- .explorer_rvs$current_node$get_child(id)
    }
  })

  return_list <- list(
    selected_node_r = shiny::reactive({
      row_index <- shiny::req(input$selector_table_cell_clicked$row)

      id <- selector_table_r()[row_index, 1, drop = TRUE]

      .explorer_rvs$current_node$get_child(shiny::req(id))
    })
  )

  return(return_list)
}
