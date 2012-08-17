\name{bccal}
\alias{bccal}

\title{
Calibrate Bristow-Campbell model
}
\description{
Function estimates Bristow-Campbell model coefficient 'B' based on reference data
}
\usage{
bccal(lat, days, rad_mea, Tmax, Tmin, tal)
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
  \item{Tmax}{
Vector of length n containing daily maximum temperature [C]. 
}
  \item{Tmin}{
Vector of length n containing daily minumum temperature [C].
}
  \item{tal}{
Clear sky transmissivity.
}
}
\details{
Function estimates Bristow-Campbell model coefficient 'B' based on reference (e.g. measured) solar radiation data. It performs a non-linear least squeres regression.
}
\value{
\item{BCb}{Bristow-Campbell 'B' coefficient}
}
\references{
Bristow, K.L., and G.S. Campbell. 1984. On the relationship between incoming solar radiation and daily maximum and minimum temperature. Agriculture and Forest Meteorology, 31:159-166.
}
\author{
Jedrzej S. Bojanowski
}

%\note{}



\seealso{
'bc', and 'bcauto' to perform auto-calibration
}
\examples{
data(Metdata)
tmax <- Metdata$meteo$TEMP_MAX
tmin <- Metdata$meteo$TEMP_MIN
days <- Metdata$meteo$DAY
lat <- Metdata$LATITUDE
rad_mea <- Metdata$meteo$RAD_MEA
bccal(lat,days,rad_mea,tmax,tmin, tal=0.76)
}

%\keyword{}

