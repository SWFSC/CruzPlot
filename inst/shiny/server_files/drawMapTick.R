# drawMapTick for CruzPlot by Sam Woodman
#   Draw tick marks and labels


# Draw major and minor tick marks
if(tick.lon.bool$bot[1]) {
  axis(1, at = tick.lon$maj, labels = FALSE, tick = TRUE, lwd = 0, lwd.ticks = 1, 
       tcl = par("tcl") *tick.param$len, cex.axis = tick.param$scale, family = tick.param$font)
  axis(1, at = tick.lon$min, labels = FALSE, lwd = 0, lwd.ticks = 1, 
       tcl = par("tcl") *0.4*tick.param$len)
  }
if(tick.lat.bool$left[1]) {
  axis(2, at = tick.lat$maj, labels = FALSE, tick = TRUE, lwd = 0, lwd.ticks = 1, 
       tcl = par("tcl") *tick.param$len, cex.axis = tick.param$scale, family = tick.param$font)
  axis(2, at = tick.lat$min, labels = FALSE, lwd = 0, lwd.ticks = 1, 
       tcl = par("tcl") * 0.4*tick.param$len)
  }
if(tick.lon.bool$top[1]) {
  axis(3, at = tick.lon$maj, labels = FALSE, tick = TRUE, lwd = 0, lwd.ticks = 1, 
       tcl = par("tcl") *tick.param$len, cex.axis = tick.param$scale, family = tick.param$font)
  axis(3, at = tick.lon$min, labels = FALSE, lwd = 0,  lwd.ticks = 1,
       tcl = par("tcl") * 0.4*tick.param$len)
  }
if(tick.lat.bool$right[1]) {
  axis(4, at = tick.lat$maj, labels = FALSE, tick = TRUE, lwd = 0, lwd.ticks = 1, 
       tcl = par("tcl") *tick.param$len, cex.axis = tick.param$scale, family = tick.param$font)
  axis(4, at = tick.lat$min, labels = FALSE, lwd = 0, lwd.ticks = 1, 
       tcl = par("tcl") * 0.4*tick.param$len)
  }

# Draw tick labels
if(tick.lon.bool$bot[2]) axis(1, at = tick.lon$label.loc, labels = tick.lon$label, tick = FALSE, 
                              cex.axis = tick.param$scale, family = tick.param$font)
if(tick.lat.bool$left[2]) axis(2, at = tick.lat$label.loc, labels = tick.lat$label, tick = FALSE, 
                               las = 1, cex.axis = tick.param$scale, family = tick.param$font)
if(tick.lon.bool$top[2]) axis(3, at = tick.lon$label.loc, labels = tick.lon$label, tick = FALSE, 
                              cex.axis = tick.param$scale, family = tick.param$font)
if(tick.lat.bool$right[2]) axis(4, at = tick.lat$label.loc, labels = tick.lat$label, tick = FALSE, 
                                las = 1, cex.axis = tick.param$scale, family = tick.param$font)
