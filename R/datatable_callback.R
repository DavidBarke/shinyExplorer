#' Callback that adds shiny inputs for specific events
#'
#' The output of this function may be used as the \code{callback} argument to
#' \code{\link[DT]{datatable}}. Event listeners for \code{dblclick}
#' and \code{contextmenu} are added to the datatable. These event listeners
#' set a shiny input, so that you can listen to these events on the R-side
#' with \code{\link[shiny]{observeEvent}}. See 'Details' for the properties of
#' the added inputs.
#'
#' @param inputId Typically the \code{outputId} used for displaying the corresponding
#' \code{\link[DT]{datatable}}.
#'
#' @details
#'
#' The callback adds the following inputs: \code{input$<inputId>_row_dblclicked}
#' and \code{input$<inputId>_row_contextmenued}, where \code{<inputId>} is
#' replaced by the parameter \code{inputId} passed to this function. These inputs
#' have the following properties:
#' \describe{
#'   \item{\code{<inputId>_row_dblclicked}}{
#'     \tabular{ll}{
#'       \code{row} \tab The index of the dblclicked row. \cr
#'       \code{id} \tab The value of the first column of the dblclicked row. It is
#'       convenient to create unique ids for every row in the first column of a
#'       table passed to \code{\link[DT]{datatable}} and hiding this column via
#'       the \code{options} argument. \cr
#'       \code{data} \tab The data of the dblclicked row. \cr
#'       \code{rnd} \tab A random number. \cr
#'     }
#'   }
#'   \item{\code{<inputId>_row_contextmenued}}{
#'     \tabular{ll}{
#'       \code{row} \tab The index of the dblclicked row. \cr
#'       \code{id} \tab The value of the first column of the dblclicked row. It is
#'       convenient to create unique ids for every row in the first column of a
#'       table passed to \code{\link[DT]{datatable}} and hiding this column via
#'       the \code{options} argument. \cr
#'       \code{data} \tab The data of the dblclicked row. \cr
#'       \code{mouse} \tab A list with elements named \code{x} and \code{y}
#'       holding the position of the mouse in px, when the datatable was
#'       contextmenued. \cr
#'       \code{rnd} \tab A random number. \cr
#'     }
#'   }
#' }
datatable_callback <- function(inputId) {
  # Notice special handling of { due to use of glue::glue
  code <- glue::glue("
    var rnd = Math.random();

    function getMousePosition(event) {{
      return({{
        x: event.clientX,
        y: event.clientY
      }});
    }}

    table.on('dblclick.dt', 'td', function() {{
      var cell = table.cell( this );
      // if (cell.index().column === 2) {{
      if (true) {{
        var row = table.row( cell.index().row );

        var data = row.data();

        Shiny.setInputValue('{inputId %_% 'row_dblclicked'}', {{
          row: row.index(),
          id: data[0],
          data: data,
          rnd: rnd
        }});
      }};
    }})

    table.on('contextmenu.dt', 'tr', function(event) {{
      var mouse = getMousePosition(event);
      var row = table.row( this );
      var data = row.data();
      var id;
      if (data !== undefined) {{
        id = data[0];
      }} else {{
        id = '';
      }}

      Shiny.setInputValue('{inputId %_% 'row_contextmenued'}', {{
        row: row.index(),
        id: id,
        data: data,
        mouse: mouse,
        rnd: rnd
      }});
      return( false );
    }})
  ")
  DT::JS(code)
}
