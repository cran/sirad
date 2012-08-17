su <-
function(days,lat,A,B,C,tmax,tmin,CC) {
      i <- dayOfYear(days)
      latt <- radians(lat)
      Sd <- extrat(i=i,lat=latt)$ExtraTerrestrialSolarRadiationDaily
      su <- Sd*(A*sqrt(tmax-tmin)+B*sqrt(1-CC/8)) + C
      su 
}

