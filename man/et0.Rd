\name{et0}
\alias{et0}

\title{
FAO Penman-Monteith evapotranspiration equation 
}
\description{
'et0' estimates evapotranspiration based on FAO Penman-Monteith equation
}
\usage{
et0(Tmax,Tmin, vap_pres,sol_rad,tal,z,uz,meah=10,extraT=NA,days=NA,lat=NA)
}

\arguments{
  \item{Tmax}{
Vector of length n containing daily maximum temperature [C].
}
  \item{Tmin}{
Vector of length n containing daily minumum temperature [C].
}
  \item{vap_pres}{
Vector of length n of mean daily vapour pressure [kPa]. 
}
  \item{sol_rad}{
Vector of length n of daily solar radiation [MJm-2d-1].
}
  \item{tal}{
Clear sky transmissivity [0-1].
}
  \item{z}{
Altitude above the sea level [m].
}
  \item{uz}{
Wind speed measured at heith 'meah' [ms-1].
}
  \item{meah}{
The height (above the ground level) of the wind speed measurement [m].
}
  \item{extraT}{
Optional. Vector of length n of extraterrestrial solar radiation [MJm-2d-1]. If 'NA' then it is calculated by the function. Providing extraterrestrial solar radiation speeds up the computation. 
}
  \item{days}{
Required only if extraT=NA. Vector of class 'Date' of length n.
}
  \item{lat}{
Required only if extraT=NA. Latitude in decimal degrees.
}
}

%\details{}

\value{
Vector of length n of daily reference evapotranspiration. [mmd-1] }
\references{
Allen, R.G., L.S. Pereira, D. Raes, and M. Smith. 1998. Crop Evapotranspiration: Guidelines for computing crop water requirements. Irrigation and Drainage Paper 56, Food and Agriculture Organization of the United Nations, Rome, pp. 300.
}
\author{
Jedrzej S. Bojanowski
}

%\note{}



%\seealso{}

\examples{
data(Metdata)
tmax <- Metdata$meteo$TEMP_MAX
tmin <- Metdata$meteo$TEMP_MIN
vpres <- Metdata$meteo$VAP_PRES
days <- Metdata$meteo$DAY
lat <- Metdata$LATITUDE
rad_mea <- Metdata$meteo$RAD_MEA
z <- Metdata$ALTITUDE
wind <-  Metdata$meteo$WIND_10

tal <- cst(rad_mea,dayOfYear(Metdata$meteo$DAY),radians(Metdata$LATITUDE)) 

et0(Tmax=tmax,Tmin=tmin, vap_pres=vpres,sol_rad=rad_mea,tal=tal,z=Metdata$ALTITUDE,
uz=wind,meah=10,extraT=NA,days=days,lat=lat)
}

%\keyword{}

