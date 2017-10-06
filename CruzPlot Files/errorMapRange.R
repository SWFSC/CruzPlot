# errorMapRange for CruzPlot by Sam Woodman
#   Outputs error messages for numerically invalid lat/lon entries
#   Run within drawMap_setVals.R


validate(
  need(!any(lat.range %in% c("", "-", "+", NA)),
       message = "Please ensure that -180 <= left longitude <= 180"),
  need(!any(lon.range %in% c("", "-", "+", NA)),
       message = "Please ensure that -180 <= right longitude <= 180"), 
  need(!any(world2 %in% c("", "-", "+", NA)),
       message = "Please ensure that map range values are valid")
)

# Lats
validate(
  # lon.range values depend on world2 boolean
  need(-90 <= lat.range[1] & lat.range[1]<= 90,
       message = "Please ensure that -90 <= bottom latitude <= 90"),
  need(-90 <= lat.range[2] & lat.range[2]<= 90,
       message = "Please ensure that -90 <= top latitude <= 90")
)

# Lons
if((0 <= lon.range[1] & 0 <= lon.range[2]) || (lon.range[1] < 0 & lon.range[2] < 0)) {
  validate(
    need(lon.range[1] < lon.range[2],
         message = paste("Left longitude must be less than right longitude unless left longitude", 
                         "is positive and right longitude is negative (Pacific-centered map)"))
  )
}
if(cruz.map.range$world2) {
  validate(
    need(0 <= lon.range[1] & lon.range[1]<= 360,
         message = "Please ensure that -180 <= left longitude <= 180"),
    need(0 <= lon.range[2] & lon.range[2]<= 360,
         message = "Please ensure that -180 <= right longitude <= 180")
  )
}
if(!cruz.map.range$world2)
{
  validate(
    need(-180 <= lon.range[1] & lon.range[1]<= 180,
         message = "Please ensure that -180 <= left longitude <= 180"),
    need(-180 <= lon.range[2] & lon.range[2]<= 180,
         message = "Please ensure that -180 <= right longitude <= 180")
  )
}