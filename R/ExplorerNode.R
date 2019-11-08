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
#'   \item{\code{new(id = NULL, node_storage = NULL, parent = NULL, is_group_node = TRUE,
#'   explorer_class = NULL, object = NULL)}}{
#'   Initialize a new node object.
#'     \tabular{ll}{
#'       \code{id} \tab Unique identifier of the node. If \code{\link[base:NULL]{NULL}},
#'         this identifier is created internally. \cr
#'       \code{node_storage} \tab Node storage of an object of class \code{\link{ExplorerTree}}.
#'         This argument is the reason, you should not call \code{ExplorerNode$new} explicitly. \cr
#'       \code{parent} \tab The parent node of this node, or
#'         \code{\link[base:NULL]{NULL}}, if node is root node. \cr
#'       \code{is_group_node} \tab \code{\link[base:logical]{Logical}} indicating
#'         whether this node is a group node or not. Only group nodes can have
#'         child nodes. \cr
#'       \code{explorer_class} \tab Object of class \code{\link{ExplorerClass}},
#'         which defines the behaviour of this node in the \code{\link{explorer}}.
#'       \cr
#'       \code{object} \tab An arbitrary object for storing information about the
#'         node. \cr
#'     }
#'   }
#'   \item{\code{add_child(id = NULL, is_group_node = TRUE, explorer_class = NULL,
#'   object = NULL, return = c("self", "child")}}{Initialize a
#'   new node object which is attached to the current node object as a child, but
#'   only if this node is a group node.
#'     \tabular{ll}{
#'       \code{group_node} \tab \code{\link[base:logical]{Logical}} indicating,
#'         whether the child node will be a group node or not. \cr
#'       \code{explorer_class} \tab Object of class \code{\link{ExplorerClass}},
#'         which defines the behaviour of the child node in the
#'         \code{\link{explorer}}.\cr
#'       \code{object} \tab An arbitrary object for storing information about the
#'         child node.\cr
#'       \code{return} \tab If \code{"self"}, this method returns the node, which
#'         adds a child; If \code{"child"}, the added node is returned.
#'     }
#'   }
#'   \item{\code{children()}}{Get a list of all child nodes of this node
#'   object.
#'   }
#'   \item{\code{get_child(id)}}{Get the child node object with \code{id == id}.
#'   }
#'   \item{\code{get_child_ids()}}{Get the ids of all child nodes of this
#'   node object.
#'   }
#'   \item{\code{get_explorer_class()}}{Get the \code{explorer_class}
#'   associated with this node object.
#'   }
#'   \item{\code{get_id()}}{Get the id of the node object.
#'   }
#'   \item{\code{get_nth_child(n)}}{Get the nth child of this node object. The
#'   first added child is returned for code{n = 1}.
#'   }
#'   \item{\code{get_object()}}{Get the object associated with this node object.
#'   }
#'   \item{\code{is_group_node()}}{Returns a \code{\link[base:logical]{logical}}
#'   indicating whether this node is a group node or not.
#'   }
#'   \item{\code{remove_child(id)}}{Remove the child with \code{id == id}.
#'   }
#'   \item{\code{set_explorer_class(explorer_class)}}{Set the
#'   \code{explorer_class} associated with this node object.
#'   }
#'   \item{\code{set_object(object)}}{Set an arbitrary object associated with
#'   this node object.}
#' }
#'
#' @name ExplorerNode
NULL

#' @export
ExplorerNode <- R6::R6Class(
  classname = "ExplorerNode",
  public = list(
    initialize = function(
      id = NULL, node_storage = NULL, parent = NULL, is_group_node = TRUE,
      explorer_class = NULL, object = GroupObject$new("Group")
    ) {
      # Count instances of this class
      if (purrr::is_null(private$static$count)) {
        private$static$count <- 0
      } else {
        private$static$count <- private$static$count + 1
      }

      # Store ids in static variable
      if (purrr::is_null(private$static$ids)) {
        private$static$ids <- character()
      }

      # Handle id
      if (purrr::is_null(id)) {
        private$id <- ".__id" %_% as.character(private$static$count) %_% "__"
      } else {
        private$id <- as.character(id)
      }

      if (private$id %in% private$static$ids) {
        stop("ExplorerNode: id must be unique.")
      } else {
        private$static$ids <- c(private$static$ids, private$id)
      }

      private$parent <- parent

      private$rvs <- shiny::reactiveValues(
        children = list()
      )

      private$.is_group_node <- is_group_node

      private$explorer_class <- explorer_class

      private$object <- object

      private$node_storage <- node_storage

      invisible(self)
    },

    add_child = function(
      id = NULL, is_group_node = TRUE, explorer_class = NULL, object = NULL,
      return = c("self", "child")
    ) {
      return <- match.arg(return)

      if (!private$.is_group_node) {
        stop(
          "ExplorerNode: add_child was called, but node is not a group node."
        )
      }

      # Create new explorer node
      node <- ExplorerNode$new(
        id = id,
        node_storage = private$node_storage,
        parent = self,
        is_group_node = is_group_node,
        explorer_class = explorer_class,
        object = object
      )

      private$node_storage$add_object(node)

      # Add node to children of this node
      id <- node$get_id()
      private$rvs$children[[id]] <- node

      if (return == "self") {
        return(self)
      }

      if (return == "child") {
        return(node)
      }
    },

    children = function() {
      private$rvs$children
    },

    get_child = function(id) {
      private$rvs$children[[id]]
    },

    get_child_ids = function() {
      names(private$rvs$children)
    },

    get_explorer_class = function() {
      private$explorer_class()
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
      private$rvs$children[[n]]
    },

    get_object = function() {
      private$object()
    },

    get_parent_node = function() {
      private$parent
    },

    is_group_node = function() {
      private$.is_group_node
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
      private$rvs$children[[id]] <- NULL
      private$node_storage$remove_object(id)
    },

    set_explorer_class = function(explorer_class) {
      private$explorer_class(explorer_class)
    },

    set_object = function(object) {
      private$object(object)
    }
  ),
  private = list(
    id = character(),
    .is_group_node = TRUE,
    explorer_class = NULL,
    node_storage = NULL,
    object = NULL,
    parent = NULL,
    rvs = NULL,
    static = new.env()
  )
)
