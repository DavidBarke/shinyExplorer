#' @param id Module id.
#'
#' @name explorer_selector
#'
#' @export
explorer_selector_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::uiOutput(
    outputId = ns("module_ui")
  )
}

#' Explorer Selector
#'
#' Use the \code{\link{explorer}} module to select a node.
#'
#' @param input,output,session Called by \code{\link[shiny:callModule]{callModule}}.
#' @param selectable_r A \code{\link[shiny:reactive]{reactive}}
#' returning a \code{\link[base:character]{character}} vector containing labels
#' of selectable explorer_classes.
#' @inheritParams explorer
#' @param ui If \code{"default"}, the UI consists of an actionButton for
#' selecting an element and an \code{\link{explorer}}, in which the selected
#' element is shown. If \code{"minimal"}, only an actionButton showing the
#' name of the selected element as label is shown.
#' @param mode If \code{"modal"} a modal is opened for selecting an element. If
#' \code{"in_place"} an explorer is opened in place for selecting an element.
#' @param action_button_fun A \code{\link[base]{function}} returning an HTML button,
#' which is connected with shiny.
#'
#' @return The \code{explorer_selector} module returns a list containing the following
#' reactives:
#' \tabular{ll}{
#'   \code{selected_node_r} \tab An object of class \code{\link{ExplorerNode}},
#'   which has been selected by the user. If no node is selected, the computation
#'   is stopped by \code{\link[shiny]{req}}. \cr
#'   \code{selected_node_or_null_r} \tab An object of class \code{\link{ExplorerNode}},
#'   which has been selected by the user or \code{\link[base]{NULL}}, if no node
#'   is selected.
#' }
#'
#' @export
explorer_selector <- function(
  input, output, session, .values, .root_node_r, .explorer_classes = list(),
  selectable_r = shiny::reactive(character()),
  addable_r = shiny::reactive(character()),
  visible_r = shiny::reactive(character()),
  .label_list = label_explorer_selector(), ui = c("default", "minimal"),
  mode = c("modal", "in_place"), action_button_fun = shiny::actionButton
) {
  if (!shiny::is.reactive(.root_node_r)) {
    stop(".root_node_r must be a reactive.")
  }

  if (!all(c(
    shiny::is.reactive(selectable_r),
    shiny::is.reactive(addable_r),
    shiny::is.reactive(visible_r)
  ))) {
    stop("At least one of [selectable|addable|visible]_r is not a reactive.")
  }

  ui <- match.arg(ui)
  mode <- match.arg(mode)

  ns <- session$ns

  rvs <- shiny::reactiveValues(
    selected_node = NULL,
    show_in_place = FALSE
  )

  output$module_ui <- shiny::renderUI({
    if (ui == "default") {
      ui <- shiny::uiOutput(
        outputId = ns("default_ui")
      )
    } else {
      ui <- shiny::uiOutput(
        outputId = ns("minimal_ui")
      )
    }

    ui <- htmltools::tagList(
      ui,
      shiny::uiOutput(
        outputId = ns("in_place_ui")
      )
    )
  })

  # IN PLACE UI
  output$in_place_ui <- shiny::renderUI({
    if (rvs$show_in_place) {
      ui <- htmltools::tagList(
        explorer_ui(
          id = ns("id_explorer")
        ),
        shiny::uiOutput(
          outputId = ns("confirm_in_place_selection")
        )
      )
    } else {
      ui <- NULL
    }
    ui
  })

  output$confirm_in_place_selection <- shiny::renderUI({
    if (is_selectable_r()) {
      ui <- action_button_fun(
        inputId = ns("confirm_selection"),
        label = .label_list$confirm_selection
      )
    } else {
      ui <- NULL
    }
    ui
  })

  # MINIMAL UI -----------------------------------------------------------------
  output$default_ui <- shiny::renderUI({
    htmltools::tagList(
      shiny::uiOutput(
        outputId = ns("caption")
      ),
      DT::dataTableOutput(
        outputId = ns("selected_node")
      )
    )
  })

  # DEFAULT UI -----------------------------------------------------------------
  output$minimal_ui <- shiny::renderUI({
    if (purrr::is_null(rvs$selected_node)) {
      label <- .label_list$select_element
    } else {
      label <- rvs$selected_node$get_object()$get_name()
    }

    action_button_fun(
      inputId = ns("select_node"),
      label = label,
      icon = shiny::icon("search")
    )
  })

  output$caption <- shiny::renderUI({
    if (purrr::is_null(rvs$selected_node)) {
      text <- .label_list$select_element
    } else {
      text <- .label_list$selected_element
    }

    ui <- htmltools::tagList(
      htmltools::tags$span(text),
      action_button_fun(
        inputId = ns("select_node"),
        label = NULL,
        icon = shiny::icon("search"),
        tooltip = .label_list$select_element
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

  selected_node_datatable <- shiny::reactive({
    if (purrr::is_null(rvs$selected_node)) {
      data <- tibble::tibble(id = character(), icon = character(), name = character())
      names(data) <- c("id", "", "Name")
      return(data)
    }

    id_col <- rvs$selected_node$get_id()
    name_col <- rvs$selected_node$get_object()$get_name()

    explorer_class <- .explorer_classes[[rvs$selected_node$get_explorer_class_id()]]
    id <- explorer_class$id
    explorer_class_return <- explorer_return$explorer_class_returns[[id]]

    icon_col <- if (explorer_class_return$is_group_r()) {
      as.character(shiny::icon("folder"))
    } else if (purrr::is_null(explorer_class_return$icon_r)) {
      ""
    } else {
      as.character(explorer_class_return$icon_r())
    }

    data <- tibble::tibble(
      id = id_col,
      icon = icon_col,
      name = name_col
    )

    names(data) <- c("id", "", "Name")

    data
  })

  # CREATION OF CONTEXTMENU ----------------------------------------------------
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
    # in explorer_classes. If the table is empty, the contextmenued node is set
    # to NULL
    explorer_return$rvs$contextmenued_node <- node

    if (purrr::is_null(node)) {
      # No specific contextmenu_items to display
      class_specific_contextmenu_items <- NULL
    } else {
      explorer_class <- .explorer_classes[[node$get_explorer_class_id()]]

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

  shiny::observeEvent(input$select_node, {
    if (mode == "modal") {
      shiny::showModal(shiny::modalDialog(
        easyClose = TRUE,
        explorer_ui(
          id = ns("id_explorer")
        ),
        footer = shiny::uiOutput(
          outputId = ns("footer")
        )
      ))
    } else {
      rvs$show_in_place = !rvs$show_in_place
    }
  })

  is_selectable_r <- shiny::reactive({
    selected_child_node <- shiny::req(explorer_return$selected_child_node_r())

    current_explorer_class_id <- selected_child_node$get_explorer_class_id()

    if (current_explorer_class_id %in% selectable_r()) {
      return(TRUE)
    }

    return(FALSE)
  })

  output$footer <- shiny::renderUI({
    if (is_selectable_r()) {
      ui <- htmltools::tagList(
        shiny::actionButton(
          inputId = ns("confirm_selection"),
          label = .label_list$confirm_selection
        )
      )
    } else {
      ui <- NULL
    }

    ui
  })

  shiny::observeEvent(input$confirm_selection, {
    if (mode == "modal") {
      shiny::removeModal()
    } else {
      rvs$show_in_place <- FALSE
    }

    rvs$selected_node <- explorer_return$selected_child_node_r()
  })

  # CALL MODULES ---------------------------------------------------------------

  explorer_return <- shiny::callModule(
    module = explorer,
    id = "id_explorer",
    .values = .values,
    .root_node_r = .root_node_r,
    .explorer_classes = .explorer_classes,
    addable_r = addable_r,
    visible_r = visible_r,
    .label_list = .label_list$explorer_label
  )

  return_list <- list(
    selected_node_r = shiny::reactive({
      shiny::req(rvs$selected_node)
    }),
    selected_node_or_null_r = shiny::reactive(rvs$selected_node)
  )

  return(return_list)
}



