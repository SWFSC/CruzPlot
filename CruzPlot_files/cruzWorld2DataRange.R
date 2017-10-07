### cruzWorld2DataRange
## Adjust longitudes of applicable data as necessary


### Non-DAS data
observe({
  req(cruz.list$ndas.df) # Use this for req since it goes back to NULL
  world2 <- cruz.map.range$world2
  # browser()
  # Probabaly doesn't actually need to be isolated?
  isolate({
    data.curr <- cruz.list$ndas.data
    
    # If world2 then convert lons to 0 to 360 range
    #   assumes there won't be any weird 0/360 overlap business
    if(world2) {
      cruz.list$ndas.data <- lapply(data.curr, function(list.curr) {
        lon <- list.curr$x
        list.curr$x <- ifelse(lon < 0, lon + 360, lon)
        list.curr
      })
    }
    
    # If not world2, convert to -180 to 180 range
    #   then see if there's any weird Pacific overlap business
    if(!world2) {
      ndas.data.curr <- lapply(data.curr, function(list.curr) {
        lon <- list.curr$x
        list.curr$x <- ifelse(lon > 180, lon - 360, lon)
        list.curr
      })
      
      # Semi-arbitrary cutoffs to determine if transect lines are 
      #   in the Pacific rather than Atlantic
      # lon.all <- lapply(ndas.data.curr, function(i) i$x)
      # if(!(all(sapply(lon.all, function(i) all(i < 0))) | 
      #      all(sapply(lon.all, function(i) all(i > 0)))) &
      #    any(sapply(lon.all, function(i) any(i > 130))) & 
      #    any(sapply(lon.all, function(i) any(i < -100)))
      # ) {
      #   ndas.data.curr <- lapply(data.curr, function(list.curr) {
      #     lon <- list.curr$x
      #     list.curr$x <- ifelse(lon > 0, lon - 360, lon)
      #     list.curr
      #   })
      # }
      
      ndas.data.curr <- lapply(ndas.data.curr, function(list.curr) {
        lon.curr <- list.curr$x
        
        if(!(all(sapply(lon.curr, function(i) all(i < 0))) | 
             all(sapply(lon.curr, function(i) all(i > 0)))) &
           any(sapply(lon.curr, function(i) any(i > 130))) & 
           any(sapply(lon.curr, function(i) any(i < -100)))
        ) {
          list.curr$x <- ifelse(lon.curr > 0, lon.curr - 360, lon.curr)
        }
        
        list.curr
      })
      
      cruz.list$ndas.data <- ndas.data.curr
    }
  })
})


### Planned transects
observe({
  req(cruz.list$planned.transects)
  world2 <- cruz.map.range$world2
  
  # Probabaly doesn't actually need to be isolated?
  isolate({
    data.curr <- cruz.list$planned.transects
    
    # If world2 then convert lons to 0 to 360 range
    #   assumes there won't be any weird 0/360 overlap business
    if(world2) {
      l1 <- data.curr$lon1
      l2 <- data.curr$lon2
      
      cruz.list$planned.transects$lon1 <- ifelse(l1 < 0, l1 + 360, l1)
      cruz.list$planned.transects$lon2 <- ifelse(l2 < 0, l2 + 360, l2)
    }
    
    # If not world2, convert to -180 to 180 range
    #   then see if there's any weird Pacific overlap business
    if(!world2) {
      l1 <- data.curr$lon1
      l2 <- data.curr$lon2
      
      l1.fix <- ifelse(l1 > 180, l1 - 360, l1)
      l2.fix <- ifelse(l2 > 180, l2 - 360, l2)
      
      # Semi-arbitrary cutoffs to determine if transect lines are in the Pacific rather than Atlantic
      lon.all <- c(l1.fix, l2.fix)
      if(!(all(lon.all < 0) | all(lon.all > 0)) &
         any(lon.all > 130) & any(lon.all < -100)
      ) {
        l1.fix <- ifelse(l1.fix > 0, l1.fix - 360, l1.fix)
        l2.fix <- ifelse(l2.fix > 0, l2.fix - 360, l2.fix)
      }
      
      cruz.list$planned.transects$lon1 <- l1.fix
      cruz.list$planned.transects$lon2 <- l2.fix
    }
  })
})


# # Coastline?
# observe({
#   world2 <- cruzMapRangeWorld2()
# })