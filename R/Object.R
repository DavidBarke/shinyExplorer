#' Object
#'
#' \code{\link[R6]{R6Class}} representing the most primitive object possessing
#' only an id and a name.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(name = "")}}{Initialize a new object with name \code{name}.
#'   }
#'   \item{\code{get_id()}}{Get the object's id.
#'   }
#'   \item{\code{get_name()}}{Get the object's name.
#'   }
#'   \item{\code{set_name(name)}}{Set the object's name. Only applicable if
#'     \code{name} is not a \code{\link[shiny]{reactive}}.
#'   }
#' }
#'
#' @name Object
NULL

#' @export
Object <- R6::R6Class(
  classname = "Object",
  public = list(
    initialize = function(name = "Object") {
      private$id <- stringi::stri_rand_strings(1, 8)

      private$name <- QWUtils::reactive_member(name)
    },

    get_id = function() {
      private$id
    },

    get_name = function() {
      private$name()
    },

    set_name = function(name) {
      private$name(name)
    }
  ),
  private = list(
    id = character(),
    name = NULL,
    static = new.env()
  )
)
