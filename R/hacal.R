hacal <-
function(lat,days,rad_mea,tmax,tmin) {
    
        i <- dayOfYear(days)
        DL <- extrat(lat=radians(lat),i)$DayLength   #[hours]
        Sd <- extrat(lat=radians(lat),i)$ExtraTerrestrialSolarRadiationDaily  # [MJ]
        dtemp <- sqrt(tmax-tmin) 
        m <- lm(rad_mea ~ I(Sd*dtemp))
        rval <- c(m$coefficients[c(2,1)],summary(m)$r.squared)
        names(rval) <- c("Ha","Hb","Hr2")
        rval
}

