# UI code for plotting non-das data

ui.nonDasPlot <- function() {
  tabItem(
    tabName = "nonDASplot",
    fluidRow(
      box(status = "primary", width = 6, plotOutput("plot7")),
      tabBox(
        title = "", id = "tabset2", width = 6,
        tabPanel(
          title = "Non-DAS data",
          fluidRow(
            box(
              title = "Loaded data", status = "warning", solidHeader = FALSE, collapsible = TRUE, width = 12,
              DT::dataTableOutput("cruzNonDasLoaded"),
              ui.new.line(),
              column(4, uiOutput("ndas_remove_execute")),
              column(3, textOutput("cruzNonDasRemove_text"))
            )
          ),
          fluidRow(
            box(
              title = "Load data", status = "warning", solidHeader = FALSE, collapsible = TRUE, width = 6,
              helpText("Longitudes must be in -180 to 180 range.",
                       "See the manual for longitude and latitude column naming requirements"),

              fluidRow(
                column(12, fileInput("ndas.file", label = h5("Load non-DAS .csv file")))
              ),
              textOutput("cruzNonDasFile_LonLat_text"),
              conditionalPanel(
                condition = "output.cruzNonDasFile_Conditional",
                radioButtons("ndas_plot_type", label = h5("Type of data"), choices = list("Line" = 1, "Point" = 2), selected = 1),

                conditionalPanel(
                  condition = "input.ndas_plot_type==1",
                  fluidRow(
                    column(
                      width = 6,
                      selectInput("ndas.line.lty", label = h5("Line type"), choices = cruz.line.type, selected = 1),
                      numericInput("ndas.line.lwd", label = h5("Line width"), value = 1, min = 1, max = 6, step = 1)
                    ),
                    column(6, selectInput("ndas.line.col", label = h5("Line color"), choices = cruz.palette.color,  selected = "black"))
                  )
                ),
                conditionalPanel(
                  condition = "input.ndas_plot_type==2",
                  fluidRow(
                    column(
                      width = 6,
                      selectInput("ndas.pt.pch", label = h5("Point type"), choices = cruz.symbol.type, selected = 1),
                      numericInput("ndas.pt.cex", label = h5("Point size"), value = 1, min = 0.1, max = 5, step = 0.1)
                    ),
                    column(
                      width = 6,
                      selectInput("ndas.pt.col", label = h5("Point color"), choices = cruz.palette.color, selected = "black"),
                      numericInput("ndas.pt.lwd", label = h5("Point line width"), value = 1, min = 1, max = 6, step = 1)
                    )
                  )
                ),
                actionButton("ndas_load_execute", "Add non-DAS data to CruzPlot"),
                textOutput("cruzNonDasAdd_text")
              )
            ),
            box(
              title = "Plot data", status = "warning", solidHeader = FALSE, collapsible = TRUE, width = 6,
              checkboxInput("ndas_plot", label = h5("Plot loaded non-DAS data"), value = FALSE),
              conditionalPanel(
                condition = "input.ndas_plot",
                ui.selectize.instructions(),
                uiOutput("ndas_toplot_uiOut_select")
              )
            )
          )
        )
      )
    )
  )
}
