apcal <-
function(lat,days,rad_mea,SSD) {
        i <- dayOfYear(days)
        DL <- extrat(lat=radians(lat),i)$DayLength   #[hours]
        Sd <- extrat(lat=radians(lat),i)$ExtraTerrestrialSolarRadiationDaily  # [MJ]
        Y <- rad_mea/Sd      
        X <- SSD/DL
        m <- lm(Y ~ X)
        rval <- c(m$coefficients,summary(m)$r.squared)
        names(rval) <- c("APa","APb","APr2")
        rval
}

