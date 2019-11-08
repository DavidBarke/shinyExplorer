ExplorerTree <- R6::R6Class(
  classname = "ExplorerTree",
  public = list(
    initialize = function(root_id = NULL) {
      private$root <- ExplorerNode$new(
        id = root_id
      )
    },

    get_root = function() {
      private$root
    }

    get_node = function(id) {

    }
  ),
  private = list(
  )
)
