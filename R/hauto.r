hauto <- 
function (lat, lon, days, Tmax, Tmin, tal, Ha_guess = 0.16, Hb_guess = 0.1, epsilon=0.5, 
    perce = NA) 
{
    if (is.na(perce)) {
    require(raster)
    p <- extract(CFC, matrix(c(lon,lat),1,2))
    perce <- -35*log(p)+0.54*p+112
    if (perce > 30) perce <- 30
    if (perce < 0.5) perce <- 0.5
    if (is.na(perce)) { perce <- 1
    warning("Lat/lon outside the Cloud Fraction Cover map. Deault perce=1 is used")
    }
    }
    latt <- radians(lat)
    i <- dayOfYear(days)
    Sd <- extrat(i = i, lat = latt)$ExtraTerrestrialSolarRadiationDaily
    dtemp <- sqrt(Tmax - Tmin)
    rv_ha <- Sd * Ha_guess * dtemp + Hb_guess
    pot <- Sd * tal
    #dif <- pot - rv_ha
    dif <- abs(1 - pot/rv_ha)
    nwh <- round(length(dif) * (perce/100))
    if (nwh<4) nwh <- 4
    wh <- which(dif < sort(dif)[nwh])
    dif <- dif[wh]
    dtemp <- dtemp[wh]
    Sd <- Sd[wh]
    rad_mea <- Sd * tal - epsilon
    m <- lm(rad_mea ~ I(Sd * dtemp))
    rval <- c(m$coefficients[c(2, 1)], summary(m)$r.squared)
    names(rval) <- c("Ha_auto", "Hb_auto", "Hr2_auto")
    rval
}
