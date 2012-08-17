\name{psychC}
\alias{psychC}

\title{
Psychrometric constant
}
\description{
'psychC' estimates the psychrometric constant.
}
\usage{
psychC(Tmax,Tmin,z)
}

\arguments{
  \item{Tmax}{
Vector of length n containing daily maximum temperature [C].
}
  \item{Tmin}{
Vector of length n containing daily minumum temperature [C].
}
  \item{z}{
Altitude above the sea level [m].
}
}

%\details{}

\value{
Psychrometric constant [kPaC-1] }
\references{
Allen, R.G., L.S. Pereira, D. Raes, and M. Smith. 1998. Crop Evapotranspiration: Guidelines for computing crop water requirements. Irrigation and Drainage Paper 56, Food and Agriculture Organization of the United Nations, Rome, pp. 300.
}
\author{
Jedrzej S. Bojanowski
}

%\note{}

%\seealso{}

\examples{
psychC(17,16,1800)
}

%\keyword{}

