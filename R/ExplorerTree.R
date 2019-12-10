#' Explorer Tree
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(id, root_id = NULL)}}{Initialize a new explorer tree object.
#'     \tabular{ll}{
#'       \code{id} \tab Unique \code{\link[base]{character}} string. \cr
#'       \code{root_id} \tab Either unique \code{\link[base]{character}} string within this
#'       explorer tree or \code{\link[base]{NULL}} for the root node of the explorer tree.
#'       If \code{NULL} an id is chosen internally. You don't need to worry about the uniqueness
#'       of \code{root_id} since you are just creating the first node of the tree.
#'     }
#'     Return: Object of class \code{ExplorerTree}.
#'   }
#'   \item{\code{get_id()}}{Get the explorer tree's id.
#'   }
#'   \item{\code{get_node(id)}}{Get the node of the explorer tree with \code{id == id}.
#'   }
#'   \item{\code{get_node_ids()}}{Get the ids of all nodes of the explorer tree as a
#'     \code{\link[base]{character}} vector.
#'   }
#'   \item{\code{get_root_node()}}{Get the root node of the explorer tree, which is equivalent
#'     to calling \code{this$get_node(root_id)}, where \code{root_id} is the id, which was
#'     passed to \code{this$new}.
#'   }
#' }
#'
#' @name ExplorerTree
NULL

#' @export
ExplorerTree <- R6::R6Class(
  classname = "ExplorerTree",
  public = list(
    initialize = function(id = "defunct", root_id = NULL, root_object = GroupObject$new("root")) {
      private$node_storage <- ObjectStorage$new(allowed_classes = "ExplorerNode")

      private$root <- ExplorerNode$new(
        id = root_id,
        node_storage = private$node_storage,
        object = root_object
      )

      private$node_storage$add_object(private$root)

      return(self)
    },

    get_node = function(id) {
      if (!(id %in% private$node_storage$get_ids())) {
        stop(
          paste(
            "ExplorerTree: There is no node with id", id,
            "in the explorer tree with id", private$id
          )
        )
      }

      private$node_storage$get_object(id)
    },

    get_node_ids = function() {
      private$node_storage$get_ids()
    },

    get_root_node = function() {
      private$root
    }
  ),
  private = list(
    node_storage = NULL,
    root = NULL
  )
)
