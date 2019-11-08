#' Create Labels for Explorer
#'
#' @param label_group_node Output of \code{\link{label_group_node}}.
#' @param add_group,ask_delete,confirm_delete,delete_node,new_group_name
#' \code{\link[base:character]{Character}}
#' string that will appear on the corresponding labels.
#'
#' @export
label_explorer <- function(
  label_group_node = label_group_node(),
  add_group = "New group",
  ask_delete = "Are you sure you want to delete the selected item?",
  confirm_delete = "Confirm",
  delete_node = "Delete",
  new_group_name = "Neue Gruppe"
) {
  list(
    label_group_node = label_group_node,
    add_group = add_group,
    ask_delete = ask_delete,
    confirm_delete = confirm_delete,
    delete_node = delete_node,
    new_group_name = new_group_name
  )
}

#' Create Labels for Explorer Selector
#'
#' @param label_explorer Output of \code{\link{label_explorer}}. \code{\link{explorer}} is used
#' internally by \code{explorer_selector}.
#' @param confirm_selection,select_element,selected_element
#' \code{\link[base:character]{Character}} string, that will appear on the corresponding
#' labels.
#'
#' @export
label_explorer_selector <- function(
  label_explorer = label_explorer(),
  confirm_selection = "Confirm selection",
  select_element = "Select element",
  selected_element = "Selected element"

) {
  list(
    label_explorer = label_explorer,
    confirm_selection = confirm_selection,
    select_element = select_element,
    selected_element = selected_element
  )
}

#' Create Labels for Group Node Explorer Class
#'
#' @param confirm_rename,open,rename \code{\link[base:character]{Character}}
#' string that will appear on the corresponding labels.
#'
#' @export
label_group_node <- function(
  confirm_rename = "Rename",
  open = "Open",
  rename = "Rename"
) {
  list(
    confirm_rename = confirm_rename,
    open = open,
    rename = rename
  )
}


