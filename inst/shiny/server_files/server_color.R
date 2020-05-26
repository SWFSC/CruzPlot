# Update select dropdown menus for greyscale vs normal colors

observeEvent(input$color_style, {
  if (!cruz.load.color$load.flag) {
    if (col.style == 1) {
      palette("default")
      updateSelectInput(session, "color_land", choices = cruz.palette.color, selected = "bisque1")
      updateSelectInput(session, "color_water", choices = cruz.palette.color, selected = "white")
      updateSelectInput(session, "grid_line_color", choices = cruz.palette.color, selected = "black")
      updateSelectizeInput(session, "das_symbol_color", choices = cruz.palette.color, selected = "black")
      updateSelectInput(session, "das_effort_lineCol", choices = cruz.palette.color, selected = "black")
      updateSelectizeInput(session, "das_effort_det_bft", choices = cruz.palette.color, selected = "black")
      updateSelectInput(session, "ndas_line_col", choices = cruz.palette.color, selected = "black")
      updateSelectInput(session, "ndas_pt_col", choices = cruz.palette.color, selected = "black")

    } else if (col.style == 2) {
      palette(gray(0:5/5))
      updateSelectInput(session, "color_land", choices = cruz.palette.gray, selected = 4)
      updateSelectInput(session, "color_water", choices = cruz.palette.gray, selected = 0)
      updateSelectInput(session, "grid_line_color", choices = cruz.palette.gray, selected = 1)
      updateSelectizeInput(session, "das_symbol_color", choices = cruz.palette.gray, selected = 1)
      updateSelectInput(session, "das_effort_lineCol", choices = cruz.palette.gray, selected = 1)
      updateSelectizeInput(session, "das_effort_det_bft", choices = cruz.palette.gray, selected = 1)
      updateSelectInput(session, "ndas_line_col", choices = cruz.palette.gray, selected = 1)
      updateSelectInput(session, "ndas_pt_col", choices = cruz.palette.gray, selected = 1)
    }
  }

  cruz.load.color$load.flag <- FALSE
}, ignoreInit = TRUE)
