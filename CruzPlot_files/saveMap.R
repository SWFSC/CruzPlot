# saveMap for CruzPlot by Sam Woodman

plotDownload <- function() {
  plotMap()()
  plotInteractive()()
}

output$downloadMap <- downloadHandler(
  filename = function() {
    file.ext <- switch(input$download.format, "1" = ".jpeg", "2" = ".pdf", "3" = ".png")
    
    paste0(
      "cruzPlot_", 
      cruz.map.range$lon.range[1], "_", cruz.map.range$lon.range[2], "_", 
      cruz.map.range$lat.range[1], "_", cruz.map.range$lat.range[2], 
      file.ext
    ) 
  },
  content = function(file) {
    file.res <- switch(input$download.res, "1" = 200, "2" = 72)
    
    if(input$download.format == 1) {
      jpeg(file, width = 10, height = 10, units = "in", res = file.res)
      plotDownload()
      dev.off()
    }
    if(input$download.format == 2) {
      pdf(file, width = 10, height = 10)
      plotDownload()
      dev.off()
    }
    if(input$download.format == 3) {
      png(file, width = 10, height = 10, units = "in", res = file.res)
      plotDownload()
      dev.off()
    }
  }
)