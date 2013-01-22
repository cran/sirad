ap <-
function(days,lat,extraT=NULL,A,B,SSD) {
      i <- dayOfYear(days)
      latt <- radians(lat)
      if (is.null(extraT)) extraT <- extrat(i=i,lat=latt)$ExtraTerrestrialSolarRadiationDaily
      ap <- extraT*(A+B*(SSD/dayLength(lat=latt,i=i)))
      ap 
}

