sucal <-
function(days,lat,rad_mea,tmax,tmin,cc) {
    
        i <- dayOfYear(days)
        latt <- radians(lat)
        Sd <- extrat(lat=latt,i)$ExtraTerrestrialSolarRadiationDaily  # [MJ]

        Y <- rad_mea/Sd          
        dtemp <- sqrt(tmax-tmin)  
        cl <- sqrt(1-cc/8)   
        m <- lm(I(Y) ~ dtemp + cl + I(1/Sd) - 1)
        rval <- c(m$coefficients,summary(m)$r.squared)
        names(rval) <- c("Sa","Sb","Sc","Sr2")
        rval
}

