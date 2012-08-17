ap <-
function(days,lat,A,B,SSD) {
      i <- dayOfYear(days)
      latt <- radians(lat)
      Sd <- extrat(i=i,lat=latt)$ExtraTerrestrialSolarRadiationDaily
      ap <- Sd*(A+B*(SSD/dayLength(lat=latt,i=i)))
      ap 
}

