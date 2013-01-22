\name{ap}
\alias{ap}

\title{
Angstrom-Prescott solar radiation model
}
\description{
Angstrom-Prescott model is used to calculate daily global irradiance for a horizontal surface based on sunshine duration.
}
\usage{
ap(days, lat, extraT=NULL, A, B, SSD)
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
Angstrom-Prescott model 'A' coefficient.
}
  \item{B}{
Angstrom-Prescott model 'B' coefficient.
}
  \item{SSD}{
Vector of length n containing sunshine duration [in hours]. 
}
}
\details{
Model proposed by Angstrom (1924) and modified by Prescott (1940) assumed linear relationship between: (1) a proportion of bright sunshine hours and astronomical day length and (2) proportion of incoming daily global solar radiation and daily extra-terrestrial radiation. This linear relationship is described by empirical model coefficients: A - intercept, B - slope. Both astronomical day length and daily extra-terrestrial radiation are calculated within this function based on location and time.
}
\value{
Vector of length n of daily solar radiation [MJm-2].
}
\references{
Angstrom, A., 1924. Solar and terrestrial radiation. Quarterly Journal of the Royal Meteorological Society, 50:121-125. \cr
Prescott, J.A., 1940. Evaporation from a water surface in relation to solar radiation. Transactions of the Royal Society of South Australia, 64:114-118.  
}
\author{
Jedrzej S. Bojanowski
}
\note{
SSD input can contain NA's, but length of vectors 'SSD' and 'days' has to be the identical.
}

\seealso{
'apcal' to calibrate the model
}
\examples{

require(zoo)
data(Metdata)
A <- 0.21
B <- 0.57
sunshine <- Metdata$meteo$SUNSHINE
days <- Metdata$meteo$DAY
lat <- Metdata$LATITUDE
plot(zoo(ap(days,lat,extraT=NULL,A,B,sunshine),order.by=days))
}

%\keyword{}

