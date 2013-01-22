\name{bcauto}
\alias{bcauto}

\title{
Auto-calibrate Bristow-Campbell model
}
\description{
Function estimates Bristow-Campbell model coefficient 'B' based on auto-calibration procedure
}
\usage{
bcauto(lat,lon,days,extraT=NULL,Tmax,Tmin,tal,BCb_guess=0.13,epsilon=0.5,perce=NA,dcoast=NA)
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
  \item{BCb_guess}{
Assumption of Bristow-Campbell coefficient. Default set to 0.13.
}
  \item{epsilon}{
A value of which potential radiation is decreased. See "details".
}
  \item{perce}{
Percent of clear days. In 'NA' then perce is estimated based on the Cloud Fraction Cover map.
}
  \item{dcoast}{
Distance to the coast [km].
}
}
\details{
The auto-calibration method bases on the assumption that on the clear-sky days model should not overpredict potential values. To define those clear-sky days, we estimate daily solar radiation using Bristow and Campbell model with default values of B = 0.13 and tal = 0.72 and we select those days for which estimated daily solar radiation is the closest to the potential values (extraterrestrial*tal). The number of clear-sky days is estimated based on the mean Cloud Fraction Cover map. Next, based on selected clear-sky days, we perform a non-linear least squares regression to derive B coefficient treating potential values decreased by 'epsilon' as a reference solar radiation values. The analysis of auto-calibration results showed clear correlation between optimal 'epsilon' and distance to the coast. We proposed simplified method in which 'epsilon' is equal to 0.1 MJm-2 or to 0.5 MJm-2 when distance to the coast is smaller or bigger than 15 km respectively. 
}
\value{
\item{BCb}{Bristow-Campbell 'B' coefficient}
}
\references{
Bojanowski, J., Donatelli, M., 2012, Auto-calibration of the Bristow and Campbell solar radiation model, Environmental Modelling and Software. [in prep.]
}
\author{
  Jedrzej S. Bojanowski
}

%\note{}


\seealso{
'bc' to use Bristow-Campbell model, and 'bccal' to perform calibration based on reference data.
}
\examples{
data(Metdata)
tmax <- Metdata$meteo$TEMP_MAX
tmin <- Metdata$meteo$TEMP_MIN
days <- Metdata$meteo$DAY
lat <- Metdata$LATITUDE
lon <- Metdata$LONGITUDE
rad_mea <- Metdata$meteo$RAD_MEA
dcoast <- Metdata$DCOAST
bcauto(lat,lon,days,extraT=NULL,tmax,tmin,tal=0.76,BCb_guess=0.13,epsilon=0.5,perce=NA,dcoast)
}
%\keyword{}

