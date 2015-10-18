\name{deltaVP}
\alias{deltaVP}

\title{
Slope of  saturation vapour pressure curve
}
\description{
'deltaVP' estimates the slope of  saturation vapour pressure curve
}
\usage{
deltaVP(Tmax,Tmin)
}

\arguments{
  \item{Tmax}{
Vector of length n containing daily maximum temperature [C].
}
  \item{Tmin}{
Vector of length n containing daily minumum temperature [C].
}
}

%\details{}

\value{
Slope of  saturation vapour pressure curve [kPaC-1] }
\references{
Allen, R.G., L.S. Pereira, D. Raes, and M. Smith. 1998. Crop Evapotranspiration: Guidelines for computing crop water requirements. Irrigation and Drainage Paper 56, Food and Agriculture Organization of the United Nations, Rome, pp. 300.
}
\author{
Jedrzej S. Bojanowski
}

%\note{}

%\seealso{}

\examples{
deltaVP(Tmax=17,Tmin=16)
}

%\keyword{}

