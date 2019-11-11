#' Create Labels for Explorer
#'
#' @param add_group,ask_delete,confirm_delete,delete_node
#' \code{\link[base:character]{Character}}
#' string that will appear on the corresponding labels.
#'
#' @export
label_explorer <- function(
  add_group = "New group",
  ask_delete = "Are you sure you want to delete the selected item?",
  confirm_delete = "Confirm",
  delete_node = "Delete"
) {
  list(
    add_group = add_group,
    ask_delete = ask_delete,
    confirm_delete = confirm_delete,
    delete_node = delete_node
  )
}

#' Create Labels for Explorer Selector
#'
#' @param explorer_label Output of \code{\link{label_explorer}}. \code{\link{explorer}} is used
#' internally by \code{explorer_selector}.
#' @param confirm_selection,select_element,selected_element
#' \code{\link[base:character]{Character}} string, that will appear on the corresponding
#' labels.
#'
#' @export
label_explorer_selector <- function(
  explorer_label = label_explorer(),
  confirm_selection = "Confirm selection",
  select_element = "Select element",
  selected_element = "Selected element"

) {
  list(
    explorer_label = explorer_label,
    confirm_selection = confirm_selection,
    select_element = select_element,
    selected_element = selected_element
  )
}

#' Create Labels for Group Node Explorer Class
#'
#' @param add,confirm_rename,new_group_name,open,rename \code{\link[base:character]{Character}}
#' string that will appear on the corresponding labels.
#'
#' @export
label_group_explorer_class <- function(
  add = "New group",
  confirm_rename = "Rename",
  new_group_name = "New group",
  open = "Open",
  rename = "Rename"
) {
  list(
    add = add,
    confirm_rename = confirm_rename,
    new_group_name = new_group_name,
    open = open,
    rename = rename
  )
}


