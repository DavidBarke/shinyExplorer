#' ObjectStorage
#'
#' R6Class for storing similar R6 objects. These objects all need to implement
#' the method get_id(). The id of an object is unique in a storage.
#'
#' @section Usage:
#' \preformatted{storage = ObjectStorage$new(allowed_classes = NULL)
#'
#' storage$add_object(object)
#'
#' storage$get_object(id)
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(allowed_classes = NULL)}}{Initialize the storage.
#'     \tabular{ll}{
#'       \code{allowed_classes} \tab Character vector. \code{class(object)} has to
#'       return at least one of these classes for being added to the storage.
#'     }
#'   }
#'   \item{\code{add_object(object)}}{Add an object to the storage:
#'     \tabular{ll}{
#'       \code{object} \tab R6 object with public methods \code{get_id()},
#'         and \code{get_name()}.
#'     }
#'   }
#'   \item{\code{get_ids()}}{Get the ids of the stored objects as a character
#'     vector named with the object's names.
#'   }
#'   \item{\code{get_length()}}{Get the length of the storage.
#'   }
#'   \item{\code{get_nth_object(n)}}{Get the object with index \code{n} going
#'   from \code{1} to \code{this$get_length()}.
#'   }
#'   \item{\code{get_object(id)}}{Get an object from the storage
#'     with \code{object$get_id() == id}.
#'     \tabular{ll}{
#'       \code{id} \tab id of an R6 object.
#'     }
#'   }
#'   \item{\code{get_objects(ids)}}{Get a list of objects from the storage
#'     with \code{object$get_id() \%in\% ids}. If \code{ids} is missing get
#'     a list with all objects.
#'     \tabular{ll}{
#'       \code{ids} \tab \code{\link[base:character]{Character}} vector. Each
#'       element has to be a id of an object in the storage.
#'     }
#'   }
#' }
#'
#' @name ObjectStorage
NULL

#' @export
ObjectStorage <- R6::R6Class(
  classname = "ObjectStorage",
  public = list(
    initialize = function(allowed_classes = NULL) {
      named_list <- list()
      names(named_list) <- character()
      private$storage <- shiny::reactiveVal(named_list)

      private$length <- shiny::reactive({
        length(private$storage())
      })

      private$allowed_classes <- allowed_classes

      invisible(self)
    },

    add_object = function(object) {
      if (!exists(
        x = "get_id",
        where = object
      )) {
        stop(
          "ObjectStorage: object has to have a method with name \"get_id\""
        )
      }

      if (!is.null(private$allowed_classes)) {
        stopifnot(any(private$allowed_classes %in% class(object)))
      }
      storage <- private$storage()
      storage[[object$get_id()]] <- object
      private$storage(storage)
      invisible(self)
    },

    get_ids = function() {
      # The names of private$storage correspond to the ids of the stored objects
      ids <- names(private$storage())

      ids
    },

    get_length = function() {
      length(private$storage())
    },

    get_nth_object = function(n) {
      private$storage()[[n]]
    },

    get_object = function(id) {
      if (!(id %in% self$get_ids())) {
        stop(paste0("There are either no or multiple objects with id ", id))
      }
      private$storage()[[id]]
    },

    get_objects = function(ids) {
      if (missing(ids)) {
        return(private$storage())
      }

      objects <- map(ids, function(id) {
        self$get_object(id)
      })

      names(objects) <- ids

      objects
    },

    remove_object = function(id) {
      storage <- private$storage()
      storage[[id]] <- NULL
      private$storage(storage)
      invisible(self)
    },

    remove_objects = function(ids) {
      storage <- private$storage()
      storage[ids] <- NULL
      private$storage(storage)
      invisible(self)
    }
  ),
  private = list(
    allowed_classes = NULL,
    length = NULL,
    storage = NULL
  )
)