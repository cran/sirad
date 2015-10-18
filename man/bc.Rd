\name{bc}
\alias{bc}

\title{
Bristow-Campbell model
}
\description{
'bc' calculates daily solar radiation based on daily temperature range using Bristow-Campbell model.
}
\usage{
bc(days, lat, BCb,extraT=NULL, Tmax, Tmin, BCc = 2, tal)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{days}{
Vector of class 'Date' of length n. 
}
  \item{lat}{
Latitude in decimal degrees.
}
  \item{BCb}{
Bristow-Campbell model coefficient 'B'.
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
  \item{BCc}{
Bristow-Campbell model coefficient 'C' usually equaled to 2.
}
  \item{tal}{
Clear sky transmissivity.
}
}
\details{
Bristow and Campbell proposed a method for estimating solar radiation from air temperature measurements. They developed an empirical relationship to express the daily total atmospheric transmittance as a function of daily range in air temperature.
}
\value{
Vector of length n of daily solar radiation [MJm-2].
}
\references{
Bristow, K.L., Campbell, G.S. 1984. On the relationship between incoming solar radiation and daily maximum and minimum temperature. Agriculture and Forest Meteorology, 31:159-166.
}
\author{
Jedrzej S. Bojanowski
}
\note{
'Tmax', 'Tmin' can contain NA's, but length of vectors 'Tmax', 'Tmin' and 'days' has to be the same.
}

\seealso{
'bccal' to calibrate model using reference data, 'bcauto' to perform auto-calibration, and 'ha' to use Hargreaves model to calculate solar radiation based on temperature range. 
}
\examples{
require(zoo)
data(Metdata)
B <- 0.11
tmax <- Metdata$meteo$TEMP_MAX
tmin <- Metdata$meteo$TEMP_MIN
days <- Metdata$meteo$DAY
lat <- Metdata$LATITUDE
plot(zoo(bc(days, lat, BCb=B,extraT=NULL, tmax, tmin, BCc=2, tal=0.76),order.by=days))
}

%\keyword{}

