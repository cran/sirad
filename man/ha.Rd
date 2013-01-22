\name{ha}
\alias{ha}

\title{
Hargreaves solar radiation model
}
\description{
'ha()' calculates daily solar radiation based on daily temperature range using Hargreaves model.
}
\usage{
ha(days, lat, extraT=NULL, A, B, Tmax, Tmin)
}
\arguments{
  \item{days}{
Vector of class 'Date' of length n.
}
  \item{lat}{
Latitude in decimal degrees.
}
  \item{extraT}{
Optional. Vector of length n of extraterrestrial solar radiation [MJm-2]. If 'NULL' then it is calculated by the function. Providing extraterrestrial solar radiation speeds up the computation 
}
  \item{A}{
Hargreaves model coefficient 'A'.
}
  \item{B}{
Hargreaves model coefficient 'B'.
}
  \item{Tmax}{
Vector of length n containing daily maximum temperature [C].
}
  \item{Tmin}{
Vector of length n containing daily minumum temperature [C].
}
}
\details{
Hargreaves proposed a method for estimating solar radiation from air temperature measurements. 
}
\value{
Vector of length n of daily solar radiation [MJm-2].
}
\references{
Hargreaves, G.H., Samini, Z.A.. 1892. Estimating potential evapotranspiration. J. Irrig. Darin. Eng., ASCE 108 (3), 225-230.
}
\author{
Jedrzej S. Bojanowski
}
\note{
'Tmax', 'Tmin' can contain NA's, but length of vectors 'Tmax', 'Tmin' and 'days' has to be the same.
}

\seealso{
'hacal' to calibrate model using reference data, 'bc' to use Bristow-Campbell model to calculate solar radiation based on temperature range. 
}
\examples{
require(zoo)
data(Metdata)
tmax <- Metdata$meteo$TEMP_MAX
tmin <- Metdata$meteo$TEMP_MIN
days <- Metdata$meteo$DAY
lat <- Metdata$LATITUDE
plot(zoo(ha(days=days, lat=lat, extraT=NULL,A=0.17, B=0, Tmax=tmax, Tmin=tmin),order.by=days))
}

%\keyword{}

