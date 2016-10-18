exh <- function (i, lat, hr = NA, Con = 4.921)
{
  rval <- c()
  for (j in i)
  {
    if (is.na(hr)) {
      vshr <- vector()
      for (ho in 0:23) {
        vshr <- c(vshr, Con * corrEarthSunDist(j) * cos(solarZenithAngle(lat, ho, j)))
      }
      shr <- vshr
    }
    if (is.numeric(hr)) {
      shr <- Con * corrEarthSunDist(j) * cos(solarZenithAngle(lat, hr, j))
    }
    rval <- c(rval, shr)
    
  }
  rval[rval < 0] <- 0
  rval
  
}