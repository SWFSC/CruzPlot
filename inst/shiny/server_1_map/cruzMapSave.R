# saveMap for CruzPlot by Sam Woodman

plotDownload <- function() {
  plotMap()()
  plotInteractive()()
}

output$downloadMap_button <- renderUI({
  validate(
    need(!is.na(input$download.res), "Please enter a valid resolution") %then%
      need(input$download.res > 0, "Please enter a valid resolution")
  )

  downloadButton("downloadMap", label = "Download map")
})

output$downloadMap <- downloadHandler(
  filename = function() {
    file.ext <- switch(
      input$download.format, "1" = ".jpeg", "2" = ".pdf", "3" = ".png"
    )

    paste0(
      "cruzPlot_",
      cruz.map.range$lon.range[1], "_", cruz.map.range$lon.range[2], "_",
      cruz.map.range$lat.range[1], "_", cruz.map.range$lat.range[2],
      file.ext
    )
  },

  content = function(file) {
    if(input$download.format == 1) {
      jpeg(file, width = 10, height = 10, units = "in", res = input$download.res)
      plotDownload()
      dev.off()
    }
    if(input$download.format == 2) {
      pdf(file, width = 10, height = 10)
      plotDownload()
      dev.off()
    }
    if(input$download.format == 3) {
      png(file, width = 10, height = 10, units = "in", res = input$download.res)
      plotDownload()
      dev.off()
    }
  }
)
