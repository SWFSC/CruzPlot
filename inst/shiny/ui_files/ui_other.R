### ui_other
## UI code for CruzPlot's disply tabs

ui.dispColor <- function() {
  # Display color/Format options
  tabItem(
    tabName = "dispColor",
    fluidRow(
      box(
        title = "Color/Format Options", status = "primary", solidHeader = TRUE,  width = 12,
        plotOutput("plotDisplay")
      )
    )
  )
}

ui.dispSp <- function() {
  # Display species codes and names
  tabItem(
    tabName = "dispSp",
    fluidRow(
      box(
        title = "Species Information", status = "primary", solidHeader = TRUE,  width = 12,
        radioButtons("sp_type", "Select species codes to display",
                     choices = list("Mammals" = 1, "Turtles" = 2, "All" = 3)),
        conditionalPanel(condition = "input.sp_type == 1", dataTableOutput("sp1")),
        conditionalPanel(condition = "input.sp_type == 2", dataTableOutput("sp2")),
        conditionalPanel(condition = "input.sp_type == 3", dataTableOutput("sp3"))
      )
    )
  )
}

ui.dispManual <- function() {
  # Display CruzPlot manual
  tabItem(
    tabName = "dispManual",
    helpText("Click 'Open in Browser' at top of the app in order to display manual in-app"),
    tags$iframe(style="height:850px; width:100%; scrolling=yes", src="CruzPlot_Manual_app.pdf")
    # uiOutput("manual_pdf") #output$manual_pdf is in 'server.R'
  )
}
