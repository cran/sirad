dayLength <-
function(lat,i) {
    if (abs(degrees(lat)) < 66.5) {DL <-  24*daylightTimeFactor(lat,i)/pi}
    if (abs(degrees(lat)) >= 66.5) {DL <- length(which(exh(i=i,lat=lat)>0))}
    DL 
}

