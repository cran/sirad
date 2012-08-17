\name{apcal}
\alias{apcal}

\title{
Calibrate Angstrom-Prescott model
}
\description{
Function estimates Angstrom-Prescott model coefficients 'A' and 'B' based on reference data  
}
\usage{
apcal(lat, days, rad_mea, SSD)
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
  \item{SSD}{
Vector of length n containing sunshine duration [in hours]. 
}
}
\details{
Function estimates Angstrom-Prescott model coefficients 'A' and 'B' based on reference (e.g. measured) solar radiation data. It performs a linear regression in which 'rad_mea' is dependent variable and a proporsion of 'SSD' and astronomical day length is an independent variable.
}
\value{
Vector containing:
\item{APa}{Angstom-Prescott 'A' coefficient}
\item{APb}{Angstom-Prescott 'B' coefficient}
\item{APr2}{Coefficient of determination of performed linear regression}
}
\references{
Angstrom, A., 1924. Solar and terrestrial radiation. Quarterly Journal of the Royal Meteorological Society, 50:121-125. \cr
Prescott, J.A., 1940. Evaporation from a water surface in relation to solar radiation. Transactions of the Royal Society of South Australia, 64:114-118.  
}

\author{
Jedrzej S. Bojanowski
}

%\note{}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
'ap' to use Angstrom-Prescott model
}
\examples{
## Calibrate the model based on measured data
data(Metdata)
sunshine <- Metdata$meteo$SUNSHINE
days <- Metdata$meteo$DAY
lat <- Metdata$LATITUDE
rad_mea <- Metdata$meteo$RAD_MEA
apcal(lat=lat,days=days,rad_mea,SSD=sunshine)
}

%\keyword{}

