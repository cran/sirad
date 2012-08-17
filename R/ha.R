ha <-
function(days,lat,A,B,Tmax,Tmin) {
      i <- dayOfYear(days)
      latt <- radians(lat)
      Sd <- extrat(i=i,lat=latt)$ExtraTerrestrialSolarRadiationDaily
      ha <- Sd*A*sqrt(Tmax-Tmin)+B
      ha 
}

