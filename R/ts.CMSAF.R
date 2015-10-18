ts.CMSAF <- 
  function(files, latlon) {  
    if(ncol(latlon)!=2) stop("'latlon' does not contain two columns with latitude and longitude!") 
    okayfiles <- file.exists(files) 
    if(length(files)!=length(okayfiles[okayfiles==TRUE])) stop("Some NetCDF files cannot be read!")      
    
    latlon.ts <- c()
    dat.ts <- c()
    val.ts <- c()
    for (i in 1:length(files)) {
      nc <- open.ncdf(files[i])      
      varname.short <- nc$var[[1]]$name #e.g "SIS"
      varname.long <- nc$var[[1]]$longname #e.g "Solar Surface Irradiance, daily mean"
      varunits <- nc$var[[1]]$units
      missval <- att.get.ncdf(nc,varname.short,"_FillValue")$value
      if (nc$var[[1]]$hasScaleFact) scale.factor <- nc$var[[1]]$scaleFact else scale.factor <- 1
      if (nc$var[[1]]$hasAddOffset) add.offset <- nc$var[[1]]$addOffset else add.offset <- 0
      
      timedim <- nc$dim[["time"]]
      time.unit <- timedim$units
      time <- timedim$vals

      date.origin <- strptime(gsub("days since ", "", time.unit), format="%Y-%m-%d %H:%M:%S", tz="GMT")
      
      if (substr(time.unit, 1, 4)=="days") 
      {dat <- date.origin+time*24*3600} else 
      {if (substr(time.unit, 1, 5)=="hours") 
      {dat <- date.origin+time*3600} else 
      {dat <- NA}}
      
      dat.ts <- c(dat.ts,rep(dat,nrow(latlon)))
      latlon.ts <- rbind(latlon.ts,latlon)
      
      latitudes <- nc$dim$lat$vals
      longitudes <- nc$dim$lon$vals 
      
      for (ll in 1:nrow(latlon)) {      
        if (min(abs(latitudes - latlon[ll,1])) < 0.5 | min(abs(latitudes - latlon[ll,2])) < 0.5) {
          wh_lat <- which(abs(latitudes - latlon[ll,1])==min(abs(latitudes - latlon[ll,1])))
          wh_lon <- which(abs(longitudes - latlon[ll,2])==min(abs(longitudes - latlon[ll,2])))
        } else
          stop("The specified location is outside the image file!") 
        val <- get.var.ncdf(nc, varid=varname.short, start=c(wh_lon,wh_lat,1), count=c(1,1,1))
        val[val == (missval*scale.factor + add.offset)]  <- NA 
        val.ts <- c(val.ts,val) 
      }
      close.ncdf(nc)
    }
    
    rval <- as.data.frame(cbind(latlon.ts,val.ts))
    names(rval) <- c("Lat","Lon",varname.short)
    
    rval$date <- as.POSIXct(dat.ts,origin="1970-01-01",tz="GMT")
        
    rval                    
  }