# Callback that adds shiny inputs for specific events
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
  JS(code)
}
