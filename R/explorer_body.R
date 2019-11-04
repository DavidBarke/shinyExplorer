explorer_body_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::div(
    DT::dataTableOutput(
      outputId = ns("selector_table")
    )
  )
}

explorer_body <- function(
  input, output, session, .values, .parent, .children_r, .root_node_r,
  .explorer_rvs, .addable_explorer_classes_r, .visible_explorer_classes_r
) {

  ns <- session$ns

  self <- QWUtils::Node$new(ns("explorer_body"), .parent, session)

  rvs <- shiny::reactiveValues(
    show_contextmenu = FALSE,
    node_open = NULL
  )

  selector_table_r <- shiny::reactive({
    # Determine for every node whether it is visible defined by the rules written
    # in the documentation of explorer
    is_visible <- purrr::map_lgl(.children_r(), function(child_node) {
      if (child_node$is_group_node()) {
        return(TRUE)
      }

      if (child_node$get_explorer_class()$id %in% .visible_explorer_classes_r()) {
        return(TRUE)
      }

      return(FALSE)
    })

    visible_children <- .children_r()[is_visible]

    id_col <- purrr::map_chr(visible_children, function(child_node) {
      child_node$get_id()
    })

    name_col <- purrr::map_chr(visible_children, function(child_node) {
      child_object <- child_node$get_object()
      child_object$get_name()
    })

    icon_col <- purrr::map_chr(visible_children, function(child_node) {
      child_explorer_class <- child_node$get_explorer_class()

      # If child node is group, the node has explorer_class NULL
      if (child_node$is_group_node()) {
        return(as.character(shiny::icon("folder")))
      }

      # Else the explorer_class' server function might return the reactive
      # icon_r, which returns an icon
      if (purrr::is_null(child_explorer_class$server_return$icon_r)) {
        return("")
      } else {
        return(as.character(child_explorer_class$server_return$icon_r()))
      }
    })

    data <- tibble::tibble(
      id = id_col,
      icon = icon_col,
      name = name_col
    )

    names(data) <- QWUtils::label_lang(
      de = c("id", "", "Name"),
      en = c("id", "", "Name")
    )

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
    } else if (node$is_group_node()) {
      # Display group specific contextmenu_items
      class_specific_contextmenu_items <- group_node_specific_contextmenu_items_ui(
        id = ns("id_group_node")
      )
    } else {
      explorer_class <- node$get_explorer_class()

      class_specific_contextmenu_items <- htmltools::tagList(
        explorer_class$ui$specific_contextmenu_items_ui(
          id = .explorer_rvs$module_ids[explorer_class$id]
        )
      )
    }

    if (purrr::is_null(node)) {
      remove_contextmenu_item <- NULL
    } else {
      remove_contextmenu_item <- contextmenu_item(
        inputId = ns("remove_node"),
        label = QWUtils::label_lang(
          de = "Löschen",
          en = "Delete"
        )
      )
    }

    add_group_contextmenu_item <- contextmenu_item(
      inputId = ns("add_group"),
      label = QWUtils::label_lang(
        de = "Neue Gruppe",
        en = "New group"
      ),
      icon = shiny::icon("folder")
    )

    add_explorer_class_contextmenu_items <- purrr::map(
      .addable_explorer_classes_r(),
      function(explorer_class_id) {
        explorer_class <- .values$explorer_classes[[explorer_class_id]]
        explorer_class$ui$contextmenu_item_ui(
          id = .explorer_rvs$module_ids[explorer_class$id]
        )
      }
    )

    show_contextmenu(
      contextmenu(
        x = input$selector_table_row_contextmenued$mouse$x,
        y = input$selector_table_row_contextmenued$mouse$y,
        class_specific_contextmenu_items,
        remove_contextmenu_item,
        contextmenu_hr(),
        add_group_contextmenu_item,
        add_explorer_class_contextmenu_items
      )
    )
  })

  shiny::observeEvent(input$add_group, {
    .explorer_rvs$current_node$add_child(
      is_group_node = TRUE,
      explorer_class = NULL,
      object = GroupObject$new(
        name = QWUtils::label_lang(
          de = "Neue Gruppe",
          en = "New group"
        )
      )
    )
  })

  shiny::observeEvent(input$remove_node, {
    shiny::showModal(shiny::modalDialog(
      title = QWUtils::label_lang(
        de = "Sind Sie sicher, dass Sie das gewählte Element entfernen möchten?",
        en = "Are you sure you want to remove the selected item?"
      ),
      footer = QWUtils::actionButtonQW(
        inputId = ns("confirm_remove"),
        label = QWUtils::label_lang(
          de = "Bestätigen",
          en = "Confirm"
        )
      )
    ))
  })

  shiny::observeEvent(input$confirm_remove, {
    .explorer_rvs$current_node$remove_child(
      id = .explorer_rvs$contextmenued_node$get_id()
    )
    shiny::removeModal()
  })

  shiny::observeEvent(input$selector_table_row_dblclicked, {
    req(length(input$selector_table_row_dblclicked) > 0)
    # If "No data available in table", nothing shall happen
    id <- req(input$selector_table_row_dblclicked$data[[1]])

    # Only group nodes can have children
    if (.root_node_r()$get_node(id)$is_group_node()) {
      # First column of selector table contains child node's id
      rvs$node_open <- list(
        node = .explorer_rvs$current_node$get_child(id),
        # Also trigger, if data[[1]] did not change (same row is dblclicked twice
        # in a row)
        rnd = runif(1)
      )
    }
  })

  shiny::observeEvent(group_node_return$open_group_r(), {
    req(length(input$selector_table_row_contextmenued) > 0)
    # If "No data available in table", nothing shall happen
    id <- req(input$selector_table_row_contextmenued$data[[1]])

    # Only group nodes can have children
    if (.root_node_r()$get_node(id)$is_group_node()) {
      # First column of selector table contains child node's id
      rvs$node_open <- list(
        node = .explorer_rvs$current_node$get_child(id),
        # Also trigger, if data[[1]] did not change (same row is dblclicked twice
        # in a row)
        rnd = runif(1)
      )
    }
  })

  group_node_return <- shiny::callModule(
    module = group_node,
    id = "id_group_node",
    .values = .values,
    .parent = self,
    .explorer_rvs = .explorer_rvs
  )

  return_list <- list(
    node_open_r = shiny::reactive({
      rvs$node_open
    }),
    selected_node_r = shiny::reactive({
      row_index <- req(input$selector_table_cell_clicked$row)

      id <- selector_table_r()[row_index, 1, drop = TRUE]

      .explorer_rvs$current_node$get_child(id)
    })
  )

  return(return_list)
}
