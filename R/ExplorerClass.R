#' Explorer Class
#'
#' \code{\link[R6]{R6Class}}, whose instantiations represent individual behaviour
#' of a specific group of elements inside the \code{\link{explorer}} module. For
#' creating your own explorer class see the explorer class vignette:
#' \code{vignette("explorer_class", package = "shinyExplorer")}.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(id, ui, server)}}{Initialise a new object
#'     of class \code{ExplorerClass}.
#'     \tabular{ll}{
#'       \code{id} \tab Unique id. \cr
#'       \code{ui} \tab A list of UI functions. See 'UI list' for details. \cr
#'       \code{server} \tab Module server function. See 'Server function' for return list
#'         elements that get handled by \code{\link{explorer}} and \code{\link{explorer_body}}.
#'         \cr
#'     }
#'   }
#' }
#'
#' @section UI list:
#' The argument \code{ui} receives a named list of ui functions - these are
#' functions taking at least an argument \code{id} and returning pseudo-HTML.
#' The following names are handled by \code{\link{explorer}}:
#' \tabular{ll}{
#'   \code{contextmenu_item_ui} \tab Function returning a \code{\link{contextmenu_item}},
#'   which is displayed in the contextmenu, if this explorer_class is addable
#'   in the \code{explorer} or \code{explorer_selector}. Clicking on this
#'   contextmenu_item usually adds a new node of this explorer class as a child
#'   node to the contextmenued node. \cr
#'   \code{specific_contextmenu_items_ui} \tab Function returning a
#'   \code{\link[htmltools:tag]{tagList}} of \code{\link[shinyExplorer:contextmenu_item]{contextmenu_items}}.
#'   These contextmenu items are part of the contextmenu which is displayed upon
#'   contextmenuing a node of this explorer class.
#' }
#' These functions only provide the layout of the items. The functionality must
#' be implemented in the server function.
#'
#' @section Server function:
#' The following (and only these) return list elements are handled by
#' \code{\link{explorer}} and \code{\link{explorer_body}}:
#' \tabular{ll}{
#'   \code{selectable_classes_r} \tab \code{\link[shiny:reactive]{Reactive}}
#'   returning a character vector containing the \code{id}s of \code{explorer_classes}
#'   who can be added to the \code{explorer}, if the current \code{explorer_class}
#'   has \code{id = id}. Returning \code{\link[base:NULL]{NULL}} results in no
#'   possible selection. Returning of \code{""} results in possible selection of
#'   all \code{explorer_classes}. If the server function does not return this
#'   reactive, all \code{explorer_classes} are selectable. \cr
#'   \code{icon_r} \tab \code{\link[shiny:reactive]{Reactive}} returning a
#'   \code{\link[shiny:icon]{icon}}, which is displayed in the \code{explorer_body}. \cr
#'   \code{on_remove} \tab \code{\link[base:function]{Function}}, which is called
#'   just before a node of this explorer class is removed from a tree. This
#'   function receives the target node as the only argument.
#' }
#'
#' @name ExplorerClass
NULL

#' @export
ExplorerClass <- R6::R6Class(
  classname = "ExplorerClass",
  public = list(
    initialize = function(id, ui, server, on_remove = function(node) NULL) {
      stopifnot(length(id) == 1, purrr::is_list(ui), purrr::is_function(server))

      self$id <- id
      self$ui <- ui
      self$server <- server
      self$on_remove <- on_remove
    },
    id = character(),
    module_id = character(),
    on_remove = NULL,
    server = NULL,
    server_return = list(),
    ui = list()
  )
)
