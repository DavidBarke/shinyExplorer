#' Create Labels for Explorer
#'
#' @export
label_explorer <- function(
  add_group = "New group",
  ask_delete = "Are you sure you want to delete the selected item?",
  confirm_delete = "Confirm",
  delete_node = "Delete"
) {
  formals_to_list(label_explorer)
}


formals_to_list <- function(fun) {
  l <- purrr::map(formalArgs(fun), function(arg) {
    # n = 3 due to mapping
    get(arg, envir = parent.frame(n = 3))
  })

  names(l) <- formalArgs(fun)

  l
}
