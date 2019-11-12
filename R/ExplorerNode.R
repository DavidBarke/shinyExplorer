#' ExplorerNode
#'
#' \code{\link[R6]{R6Class}} representing a node in an explorer tree which can
#' be traversed using the \code{explorer} module. Each node has an
#' \code{explorer_class} attached representing the general behaviour of
#' nodes with this \code{explorer_class} in the explorer. Furthermore an
#' object is attached representing the special behaviour and data associated with
#' this particular node.
#'
#' @section Important:
#' You usually don't want to call explicitly \code{ExplorerNode$new}.
#' Instead you should create a new explorer tree with \code{\link{ExplorerTree}},
#' take its root node with \code{root <- explorer_tree$get_root_node()} and
#' afterwards call \code{root$add_child()}.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(id = NULL, node_storage = NULL, parent = NULL,
#'   explorer_class_id = "__group__", object = NULL, removable = TRUE)}}{
#'   Initialize a new node object.
#'     \tabular{ll}{
#'       \code{id} \tab Unique identifier of the node. If \code{\link[base:NULL]{NULL}},
#'         this identifier is created internally. \cr
#'       \code{node_storage} \tab Node storage of an object of class \code{\link{ExplorerTree}}.
#'         This argument is the reason, you should not call \code{ExplorerNode$new} explicitly. \cr
#'       \code{parent} \tab The parent node of this node, or
#'         \code{\link[base:NULL]{NULL}}, if node is root node. \cr
#'       \code{explorer_class_id} \tab Id of an object of class
#'         \code{\link{ExplorerClass}}, which defines the behaviour of this node
#'         in the \code{\link{explorer}}. This object must be passed to
#'         \code{explorer} as an element of the \code{.explorer_classes} list.
#'       \cr
#'       \code{object} \tab An arbitrary object for storing information about the
#'         node. \cr
#'       \code{removable} \tab If \code{\link[base:logical]{TRUE}}, this node is
#'        removable by the user, else not.
#'     }
#'   }
#'   \item{\code{add_child(id = NULL, is_group_node = TRUE,
#'     explorer_class_id = "__group__", object = NULL, removable = TRUE,
#'     return = c("self", "child")}}{Initialize a new node object which is
#'     attached to the current node object as a child, but only if this node is
#'     a group node.
#'     \tabular{ll}{
#'       \code{id} \tab Unique identifier of the node. If \code{\link[base:NULL]{NULL}},
#'         this identifier is created internally. \cr
#'       \code{explorer_class_id} \tab Id of an object of class
#'         \code{\link{ExplorerClass}}, which defines the behaviour of the child
#'         node in the \code{\link{explorer}}. This object must be passed to
#'         \code{explorer} as an element of the \code{.explorer_classes} list. \cr
#'       \code{object} \tab An arbitrary object for storing information about the
#'         child node.\cr
#'       \code{removable} \tab If \code{\link[base:logical]{TRUE}}, this node is
#'        removable by the user, else not. \cr
#'       \code{return} \tab If \code{"self"}, this method returns the node, which
#'         adds a child; If \code{"child"}, the added node is returned.
#'     }
#'   }
#'   \item{\code{get_children()}}{Get an object of class \code{\link{ObjectStorage}}
#'     containing all children of this node. Each child is an object of class
#'     \code{ExplorerNode}.
#'   }
#'   \item{\code{get_child(id)}}{Get the child node object with \code{id == id}.
#'   }
#'   \item{\code{get_child_ids()}}{Get the ids of all child nodes of this
#'     node object.
#'   }
#'   \item{\code{get_child_objects}}{Get an object of class
#'     \code{\link{ObjectStorage}} containing all objects associated with the
#'     children of this node. These objects are usually heterogeneous, which means
#'     they are instances of different classes.
#'   }
#'   \item{\code{get_explorer_class_id()}}{Get the id of the object of class
#'     \code{explorer_class} associated with this node object.
#'   }
#'   \item{\code{get_id()}}{Get the id of the node object.
#'   }
#'   \item{\code{get_nth_child(n)}}{Get the nth child of this node object. The
#'     first added child is returned for code{n = 1}.
#'   }
#'   \item{\code{get_object()}}{Get the object associated with this node object.
#'   }
#'   \item{\code{is_removable()}}{Returns a \code{\link[base:logical]{logical}}
#'     indicating whether this node is removable or not.
#'   }
#'   \item{\code{remove_child(id)}}{Remove the child with \code{id == id}.
#'   }
#'   \item{\code{set_explorer_class_id(explorer_class_id)}}{Set the id of the
#'     object of class \code{explorer_class} associated with this node object.
#'   }
#'   \item{\code{set_object(object)}}{Set an arbitrary object associated with
#'     this node object.}
#' }
#'
#' @name ExplorerNode
NULL

#' @export
ExplorerNode <- R6::R6Class(
  classname = "ExplorerNode",
  public = list(
    initialize = function(
      id = NULL, node_storage = NULL, parent = NULL, explorer_class_id = "__group__",
      object = GroupObject$new("Group"), removable = TRUE
    ) {
      # Count instances of this class
      if (purrr::is_null(private$static$count)) {
        private$static$count <- 0
      } else {
        private$static$count <- private$static$count + 1
      }

      # Handle id
      if (purrr::is_null(id)) {
        private$id <- ".__id" %_% as.character(private$static$count) %_% "__"
      } else {
        private$id <- as.character(id)
      }

      private$node_storage <- node_storage

      if (private$id %in% private$node_storage$get_ids()) {
        stop("ExplorerNode: id must be unique.")
      }

      private$parent <- parent

      private$children <- ObjectStorage$new("ExplorerNode")

      private$child_objects <- ObjectStorage$new(NULL)

      private$explorer_class_id <- explorer_class_id

      private$object <- object

      private$removable <- removable

      invisible(self)
    },

    add_child = function(
      id = NULL, explorer_class_id = "__group__", object = Object$new(), removable = TRUE,
      return = c("self", "child")
    ) {
      return <- match.arg(return)

      # Create new explorer node
      node <- ExplorerNode$new(
        id = id,
        node_storage = private$node_storage,
        parent = self,
        explorer_class_id = explorer_class_id,
        object = object,
        removable = removable
      )

      private$node_storage$add_object(node)

      # Add node to children of this node
      id <- node$get_id()
      private$children$add_object(node)
      # Add child object tot child objes of this node
      private$child_objects$add_object(node$get_object())

      if (return == "self") {
        return(self)
      }

      if (return == "child") {
        return(node)
      }
    },

    get_children = function() {
      private$children
    },

    get_child = function(id) {
      private$children$get_object(id)
    },

    get_child_ids = function() {
      private$children$get_ids()
    },

    get_child_objects = function() {
      private$child_objects
    },

    get_explorer_class_id = function() {
      private$explorer_class_id
    },

    get_id = function() {
      private$id
    },

    get_name = function() {
      private$id
    },

    # Returns a node of the node tree with id = id, even parent nodes. It could be useful
    # to implement get_descendent, which checks whether this node is a descendent
    # of the current node.
    get_node = function(id) {
      private$node_storage$get_object(id)
    },

    get_nth_child = function(n) {
      private$children$get_nth_object(n)
    },

    get_object = function() {
      private$object
    },

    get_parent_node = function() {
      private$parent
    },

    is_removable = function() {
      private$removable
    },

    siblings = function() {
      if (purrr::is_null(private$parent)) {
        return(list(self))
      } else {
        return(
          private$parent$children()
        )
      }
    },

    remove_child = function(id) {
      child_node <- private$children$get_object(id)
      child_object_id <- child_node$get_object()$get_id()

      private$child_objects$remove_object(child_object_id)
      private$children$remove_object(id)
      private$node_storage$remove_object(id)
    },

    set_explorer_class_id = function(explorer_class_id) {
      private$explorer_class_id(explorer_class_id)
    },

    set_object = function(object) {
      private$object(object)
    }
  ),
  private = list(
    child_objects = NULL,
    children = NULL,
    explorer_class_id = "__group__",
    id = character(),
    node_storage = NULL,
    object = NULL,
    parent = NULL,
    removable = TRUE,
    static = new.env()
  )
)
