\name{hacal}
\alias{hacal}
\title{
Calibrate Hargreaves model
}
\description{
Function estimates Hargreaves model coefficients 'A' and 'B' based on reference data
}
\usage{
hacal(lat, days, rad_mea, tmax, tmin)
}
\arguments{
  \item{lat}{
Latitude in decimal degrees.
}
  \item{days}{
Vector of class 'Date' of length n.
}
  \item{rad_mea}{
Vector of length n containing reference (e.g. measured) solar radiation [MJm-2].
}
  \item{tmax}{
Vector of length n containing daily maximum temperature [C].
}
  \item{tmin}{
Vector of length n containing daily minumum temperature [C].
}
}
\details{
Function estimates Hargreaves model coefficients 'A' and 'B' based on reference (e.g. measured) solar radiation data. It performs a linear regression.
}
\value{
Vector of length 3 containing:
\item{Ha}{Hargreaves 'A' coefficient}
\item{Hb}{Hargreaves 'B' coefficient}
\item{Hr2}{Coefficient of determination of performed linear regression}
}
\references{
Hargreaves, G.H., Samini, Z.A. 1892. Estimating potential evapotranspiration. J. Irrig. Darin. Eng., ASCE 108 (3), 225-230.
}
\author{
Jedrzej S. Bojanowski
}

\seealso{
'ha'
}
\examples{
data(Metdata)
tmax <- Metdata$meteo$TEMP_MAX
tmin <- Metdata$meteo$TEMP_MIN
days <- Metdata$meteo$DAY
lat <- Metdata$LATITUDE
rad_mea <- Metdata$meteo$RAD_MEA
hacal(lat=lat,days=days,rad_mea,tmax=tmax, tmin=tmin)
}


