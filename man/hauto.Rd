\name{hauto}
\alias{hauto}
\title{
Auto-calibrate Hargreaves model
}
\description{
Function estimates Hargreaves model coefficients 'A' and 'B' based on autocalibration procedure
}
\usage{
hauto(lat, lon, days,extraT = NULL, Tmax, Tmin, tal, Ha_guess = 0.16, Hb_guess = 0.1, epsilon=0.5, perce = NA) 
}
\arguments{
  \item{lat}{
Latitude in decimal degrees.
}
  \item{lon}{
Longitude in decimal degrees.
}
  \item{days}{
Vector of class 'Date' of length n.
}
  \item{extraT}{
Optional. Vector of length n of extraterrestrial solar radiation [MJm-2]. If 'NULL' then it is calculated by the function. Providing extraterrestrial solar radiation speeds up the computation 
}
  \item{Tmax}{
Vector of length n containing daily maximum temperature [C].
}
  \item{Tmin}{
Vector of length n containing daily minumum temperature [C].
}
  \item{tal}{
Clear sky transmissivity.
}
  \item{Ha_guess}{
Assumption of Hargreaves Ha coefficient. Default set to 0.16.
}
  \item{Hb_guess}{
Assumption of Hargreaves Hb coefficient. Default set to 0.1.
}
  \item{epsilon}{
A value of which potential radiation is decreased. See "details".
}
  \item{perce}{
Percent of clear days. Default set to 1.
}
  }
\details{
The auto-calibration method bases on the assumption that on the clear-sky days model should not overpredict potential values. To define those clear-sky days, we estimate daily solar radiation using Hargreaves model with default values of A = 0.16, B = 0.1 and tal = 0.72 and we select those days for which estimated daily solar radiation is the closest to the potential values (extra-terrestrial*tal). The number of clear-sky days is estimated based on the mean Cloud Fraction Cover map. Next, based on selected clear-sky days, we perform a non-linear least squares regression to derive A and B coefficients treating potential values decreased by 'epsilon' as a reference solar radiation values. The analysis of auto-calibration results showed clear correlation between optimal 'epsilon' and distance to the coast. We proposed simplified method in which 'epsilon' is equal to 0.1 MJm-2 or to 0.5 MJm-2 when distance to the coast is smaller or bigger than 15 km respectively.
}
\value{
Vector of length 3 containing:
\item{Ha}{Hargreaves 'A' coefficient}
\item{Hb}{Hargreaves 'B' coefficient}
\item{Hr2}{Coefficient of determination of performed linear regression}
}
\references{
Hargreaves, G.H., Samani, Z.A. 1892. Estimating potential evapotranspiration. J. Irrig. Darin. Eng., ASCE 108 (3), 225-230.
}
\author{
Jedrzej S. Bojanowski
}

\seealso{
'hacal'
}
\examples{
data(Metdata)
Tmax <- Metdata$meteo$TEMP_MAX
Tmin <- Metdata$meteo$TEMP_MIN
days <- Metdata$meteo$DAY
lat <- Metdata$LATITUDE
lon <- Metdata$LONGITUDE
hauto(lat,lon,days,extraT=NULL,Tmax,Tmin,tal=0.76,Ha_guess=0.16,Hb_guess=0.1,epsilon=0.5,perce=NA)
}


