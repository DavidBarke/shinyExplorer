explorer_header_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::uiOutput(
      outputId = ns("header")
    )
  )
}

explorer_header <- function(
  input, output, session, .values, .explorer_classes, .explorer_class_returns,
  .explorer_rvs, .root_node_r
) {

  ns <- session$ns

  rvs <- shiny::reactiveValues(
    ancestor_counter = -1,
    siblings_contextmenu_item_used_ids = list()
  )

  node_to_name <- function(node) {
    node$get_object()$get_name()
  }

  # The generation of the current node is the number of ancestor nodes including
  # the root node the current node has
  generation_r <- shiny::reactive({
    get_node_distance(.explorer_rvs$current_node, .root_node_r())
  })

  # Header contains links to all direct descendents of the current displayed
  # children in the body (like Windows Explorer). After a link is clicked, a
  # context menu opens up, in which links to all siblings of this descendent are
  # displayed
  output$header <- shiny::renderUI({
    indices <- c(0, seq_len(generation_r()))

    ancestor_list <- purrr::map(indices, function(i) {
      node <- get_ancestor_node(.explorer_rvs$current_node, i)

      if (rvs$ancestor_counter < i) {
        rvs$ancestor_counter <- rvs$ancestor_counter + 1
        # Initialise character vector, which stores all ids of siblings, which
        # are observed by a context menu item
        rvs$siblings_contextmenu_item_used_ids[[i + 1]] <- character()

        shiny::observeEvent(input[["child_link" %_% i]], {
          .explorer_rvs$current_node <- get_ancestor_node(.explorer_rvs$current_node, i)
        })

        shiny::observeEvent(input[["siblings_link" %_% i]], {
          shiny::req(input[["siblings_link" %_% i]] > 0)

          node <- get_ancestor_node(.explorer_rvs$current_node, i)

          siblings <- node$siblings()$get_objects()

          is_group_node <- purrr::map_lgl(siblings, function(node) {
            explorer_class_id <- node$get_explorer_class_id()
            .explorer_class_returns[[explorer_class_id]]$is_group_r()
          })

          sibling_group_nodes <- siblings[is_group_node]

          # Only create contextmenu_items for group nodes
          contextmenu_items <- purrr::map(sibling_group_nodes, function(node) {
            node_id <- node$get_id()

            node_object <- node$get_object()

            node_object_class <- class(node_object)

            if ("GroupObject" %in% node_object_class) {
              icon <- shiny::icon("folder")
            } else if ("DatasetObject" %in% node_object_class) {
              icon <- shiny::icon("table")
            } else {
              icon <- shiny::icon("plus")
            }

            # Use of i + 1, since index starts with zero
            if (!(node_id %in% rvs$siblings_contextmenu_item_used_ids[[i + 1]])) {
              rvs$siblings_contextmenu_item_used_ids[[i + 1]] <- c(
                rvs$siblings_contextmenu_item_used_ids[[i + 1]],
                node_id
              )

              shiny::observeEvent(
                input[["siblings_contextmenu" %_% i %_% "item" %_% node_id]],
                {
                  .explorer_rvs$current_node <- .root_node_r()$get_node(node_id)
                }
              )
            }

            # Return context menu item. If clicked, the current node is set
            # to the sibling node represented by this item.
            contextmenu_item(
              inputId = ns("siblings_contextmenu" %_% i %_% "item" %_% node_id),
              label = node_to_name(node),
              icon = icon
            )
          })

          show_contextmenu(
            contextmenu(
              x = input[["siblings_link" %_% i %_% "position"]]$left,
              y = input[["siblings_link" %_% i %_% "position"]]$bottom,
              contextmenu_items
            ),
            session = session
          )
        })
      }

      # Display is in opposite direction due to direction:
      htmltools::tags$li(
        htmltools::div(
          class = "explorer-text",
          shiny::actionLink(
            inputId = ns("child_link" %_% i),
            label = node_to_name(node)
          )
        ),
        htmltools::span(
          class = "explorer-vr"
        ),
        htmltools::div(
          class = "wide-icon explorer-angle",
          positional_input(
            inputId = ns("siblings_link" %_% i),
            label = "",
            icon = shiny::icon("angle-right")
          )
        )
      )
    })

    ui <- htmltools::tagList(
      htmltools::tags$ul(
        class = "explorer-ancestor-list inner-box",
        # Revert, as the most remote ancestor has to be on the left side
        rev(ancestor_list)
      )
    )

    ui
  })
}
