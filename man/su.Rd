\name{su}
\alias{su}

\title{
Supit-Van Kappel solar radiation model
}
\description{
'su()' calculates daily solar radiation based on daily cloud coverage and temperature range using Supit-Van Kappel model.
}
\usage{
su(days, lat, extraT=NULL, A, B, C, tmax, tmin, CC)
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
Supit-Van Kappel model coefficient 'A'.
}
  \item{B}{
Supit-Van Kappel model coefficient 'B'.
}
  \item{C}{
Supit-Van Kappel model coefficient 'C'.
}
  \item{tmax}{
Vector of length n containing daily maximum temperature [C].
}
  \item{tmin}{
Vector of length n containing daily minumum temperature [C].
}
  \item{CC}{
Vector of length n containing daily cloud coverage [octas].
}
}
\details{
Supit and Van Kappel proposed a method for estimating solar radiation from daily cloud coverage and temperature range. 
}
\value{
Vector of length n of daily solar radiation [MJm-2].
}
\references{
Supit, I. 1994. Global radiation. Publication EUR 15745 EN of the Office for Official Publications of the EU, Luxembourg. \cr
Supit, I., Kappel, R.R. van, 1998. A simple method to estimate global radiation. Solar Energy, 63:147-160. 
}
\author{
Jedrzej S. Bojanowski
}
\note{
'CC', 'Tmax', 'Tmin' can contain NA's, but length of vectors 'CC', 'Tmax', 'Tmin' and 'days' has to be the identical.
}



\seealso{
'sucal' to calibrate the model.
}
\examples{
require(zoo)
data(Metdata)
tmax <- Metdata$meteo$TEMP_MAX
tmin <- Metdata$meteo$TEMP_MIN
cc <- Metdata$meteo$CLOUD_DAYTIME_TOTAL
days <- Metdata$meteo$DAY
lat <- Metdata$LATITUDE
plot(zoo(su(days=days, lat=lat, extraT=NULL, A=0.07, B=0.54, C=-0.2, tmax=tmax, tmin=tmin, CC=cc),order.by=days))
}


%\keyword{}

