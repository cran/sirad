\name{cst}
\alias{cst}

\title{
Estimate clear sky transmissivity
}
\description{
Function estimates a clear sky transmissivity based on reference data (e.g. measured)
}
\usage{
cst(RefRad, days, lat, extraT=NULL, perce = 3, sepYear = FALSE, stat='median')
}

\arguments{
  \item{RefRad}{
Vector of length n of reference solar radiation data [MJm-2]
}
  \item{days}{
Vector of class 'Date' of length n.
}
  \item{lat}{
Latitude in radians
}
  \item{extraT}{
Optional. Vector of length n of extraterrestrial solar radiation [MJm-2]. If 'NULL' then it is calculated by the function. Providing extraterrestrial solar radiation speeds up the computation 
}
  \item{perce}{
Percent of days to be chosen as clear days
}
  \item{sepYear}{
Logical value. If 'TRUE' percent of days given by 'perce' of every single year are taken for calculation. If 'FALSE' percent of days given by 'perce' of all years are taken for calculation
}
  \item{stat}{
Method used to estimate final value of the clear sky transmissivity from the values derived from selected clear-sky days. Default is 'median' which is more conservative, while alternative 'max' is sensitive to outliers. If 'max' is used the value of 'perce' is not important. If 'stat' is numeric then (instead of 'median' or 'max') 'quantile' is used. 'Stat' is sent as quantile's 'probs' parameter. See ?quantile for details   
}
}

%\details{}

\value{
Numeric. Clear sky transmissivity.}
%\references{}
\author{
Jedrzej S. Bojanowski
}

%\note{}

%\seealso{}

\examples{

data(Metdata)
ref <- Metdata$meteo$RAD_MEA
i <- dayOfYear(Metdata$meteo$DAY)
latr <- radians(Metdata$LATITUDE)
cst(ref,i,latr) 
}

%\keyword{}

