### drawData_setVals
## Set values and call reactive functions for drawData.R



### Call reactive functions and set values
# Non-DAS
data.ndas <- NULL
if(input$ndas_plot) data.ndas <- cruzNonDas()

# DAS
if(!is.null(cruz.list$das.data)) {
  req(input$das.sight.dateRange)
  req(input$das.effort.dateRange)
  
  # Sightings
  if(input$das_sightings) {
    source("errorDasSight.R", local = TRUE, chdir = FALSE) 
    # Other specific checks in cruzDasSight... functions
    data.list <- cruzDasSightRange()

    data.sight <- data.list$data.sight
    sight.type <- data.list$sight.type
    data.sight.symbol <- cruzDasSightSymbol()
    if(input$das_legend) data.sight.legend <- cruzDasSightLegend()
  }
  
  # Effort
  if(as.numeric(input$das_effort) != 1) {
    data.effort.list <- cruzDasEffort()
    
    data.effort <- data.effort.list$data.effort
    eff.ndx.R <- data.effort.list$ndx.R
    eff.ndx.E <- data.effort.list$ndx.E
    eff.bft <- data.effort.list$data.effort$Bft[eff.ndx.R]
    
    eff.col <- cruzDasEffortLines()$eff.col
    eff.lwd <- cruzDasEffortLines()$eff.lwd
    if(input$eff_legend) data.eff.legend <- cruzDasEffortLegend()
    
    
    ### Adjust data.effort$Lon points as needed for world/world2
    lon.curr <- data.effort$Lon
    
    # If world2 then convert lons to 0 to 360 range
    #   assumes there won't be any weird 0/360 overlap business
    if(world2) data.effort$Lon <- ifelse(lon.curr < 0, lon.curr + 360, lon.curr)
    
    # If not world2, convert to -180 to 180 range
    #   then see if there's any weird Pacific overlap business
    if(!world2) {
      lon.fix <- ifelse(lon.curr > 180, lon.curr - 360, lon.curr)

      # Semi-arbitrary cutoffs to determine if transect lines are in the Pacific rather than Atlantic
      if(!(all(lon.fix < 0) | all(lon.fix > 0)) &
         any(lon.fix > 130) & any(lon.fix < -100)
      ) {
        lon.fix <- ifelse(lon.fix > 0, lon.fix - 360, lon.fix)
      }
      
      data.effort$Lon <- lon.fix
    }
  }
}
