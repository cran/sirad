ts.CMSAF <- 
function(files, latlon) {    
      latlon <- matrix(latlon,ncol=2)      
      okayfiles <- file.exists(files) 
      if(length(files)!=length(okayfiles[okayfiles==TRUE])) stop("Some NetCDF files cannot be read!")      
      var.ts <- c()
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
      dat <- as.Date(utcal.nc(time.unit,time,type="s")) 
      latitudes <- nc$dim$lat$vals
      longitudes <- nc$dim$lon$vals      
      var.value <- dat
      for (ll in 1:nrow(latlon)) {      
      if (min(abs(latitudes - latlon[ll,1])) < 0.5 | min(abs(latitudes - latlon[ll,2])) < 0.5) {
      wh_lat <- which(abs(latitudes - latlon[ll,1])==min(abs(latitudes - latlon[ll,1])))
      wh_lon <- which(abs(longitudes - latlon[ll,2])==min(abs(longitudes - latlon[ll,2])))
      } else
      stop("The specified location is outside the image file!") 
      val <- get.var.ncdf(nc, varid=varname.short, start=c(wh_lon,wh_lat,1), count=c(1,1,1))
      val[val == (missval*scale.factor + add.offset)]  <- NA
      var.value <- c(var.value,val)      
      }
      close.ncdf(nc)
      var.ts <- rbind(var.ts,var.value)
      }
      var.ts[var.ts<0] <- NA
      var.zoo <- zoo(var.ts[,-1],order.by=as.Date(var.ts[,1]))
      var.zoo                    
      }