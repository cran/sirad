\name{sirad-package}
\alias{sirad-package}
\alias{sirad}
\docType{package}
\title{
Functions for calculating daily solar radiation and avapotranspiration}
\description{
Calculates daily solar radiation at horizontal surface using several well-known models (Bristow-Campbell, Hargreaves, Supit-Van Kappel, Mahmood-Hubbard, Angrstrom-Prescott). It also includes functions for model calibration based on groud-truth data as well as a function for auto-calibration. The FAO Penmann-Monteith equation to calculate evapotranspiration is also included.
}
\details{
\tabular{ll}{
Package: \tab sirad\cr
Type: \tab Package\cr
Version: \tab 2.3-1\cr
Date: \tab 2015-12-12\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
}
\author{
Jedrzej S. Bojanowski

Maintainer: Jedrzej S. Bojanowski <jedrzej.bojanowski@gmail.com>

}

%\references{}

\keyword{ package }

%\seealso{}

\examples{
require(zoo)
data(Metdata)
A <- 0.21
B <- 0.57
sunshine <- Metdata$meteo$SUNSHINE
days <- Metdata$meteo$DAY
lat <- Metdata$LATITUDE
lon <- Metdata$LONGITUDE
plot(zoo(ap(days=days,lat=lat,lon=lon,extraT=NULL, A=A,B=B,SSD=sunshine),order.by=days))
}
