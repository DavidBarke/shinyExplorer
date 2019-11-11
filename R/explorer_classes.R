#' Group Explorer Class
#'
#' @param .label_list A \code{\link[base]{list}} created with
#' \code{\link{label_group_explorer_class}} containing labels for all buttons
#' used inside the group_explorer_class.
#'
#' @export
group_explorer_class <- function(.label_list = label_group_explorer_class()) {
  ExplorerClass$new(
    id = "__group__",
    ui = list(
      contextmenu_item_ui = contextmenu_item_ui_factory(.label_list),
      specific_contextmenu_items_ui = group_node_specific_contextmenu_items_ui_factory(.label_list)
    ),
    server = group_node,
    is_group = TRUE
  )
}
