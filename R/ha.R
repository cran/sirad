ha <-
function(days,lat,extraT=NULL,A,B,Tmax,Tmin) {
      i <- dayOfYear(days)
      latt <- radians(lat)
      if (is.null(extraT)) extraT <- extrat(i=i,lat=latt)$ExtraTerrestrialSolarRadiationDaily
      ha <- extraT*A*sqrt(Tmax-Tmin)+B
      ha 
}

