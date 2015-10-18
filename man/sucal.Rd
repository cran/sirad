\name{sucal}
\alias{sucal}

\title{
Calibrate Supit-Van Kappel model
}
\description{
Function estimates Supit-Van Kappel model coefficients 'A', 'B' and 'C' based on reference data
}
\usage{
sucal(days, lat, rad_mea, extraT=NULL, tmax, tmin, cc)
}

\arguments{
  \item{days}{
Vector of class 'Date' of length n.
}
  \item{lat}{
Latitude in decimal degrees.
}
  \item{rad_mea}{
Vector of length n containing reference (e.g. measured) solar radiation [MJm-2].
}
  \item{extraT}{
Optional. Vector of length n of extraterrestrial solar radiation [MJm-2]. If 'NULL' then it is calculated by the function. Providing extraterrestrial solar radiation speeds up the computation 
}
  \item{tmax}{
Vector of length n containing daily maximum temperature [C].
}
  \item{tmin}{
Vector of length n containing daily minumum temperature [C].
}
  \item{cc}{
Vector of length n containing daily cloud coverage [octas].
}
}
\details{
Function estimates Supit-Van Kappel model coefficients 'A', 'B' and 'C' based on reference (e.g. measured) solar radiation data. It performs a linear regression.
}
\value{
Vector of length 3:
\item{Sa}{Supit-Van Kappel 'A' coefficient}
\item{Sb}{Supit-Van Kappel 'B' coefficient}
\item{Sc}{Supit-Van Kappel 'C' coefficient}
\item{Sr2}{Coefficient of determination of performed linear regression}
}
\references{
Supit, I. 1994. Global radiation. Publication EUR 15745 EN of the Office for Official Publications of the EU, Luxembourg. \cr
Supit, I., Kappel, R.R. van, 1998. A simple method to estimate global radiation. Solar Energy, 63:147-160. 
}
\author{
Jedrzej S. Bojanowski
}

%\note{}



\seealso{
'su'.
}
\examples{
data(Metdata)
tmax <- Metdata$meteo$TEMP_MAX
tmin <- Metdata$meteo$TEMP_MIN
days <- Metdata$meteo$DAY
lat <- Metdata$LATITUDE
rad_mea <- Metdata$meteo$RAD_MEA
CC <- Metdata$meteo$CLOUD_DAYTIME_TOTAL
sucal(lat=lat,days=days,rad_mea, extraT=NULL,tmax=tmax, tmin=tmin,cc=CC)
}

%\keyword{}

