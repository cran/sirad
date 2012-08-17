\name{es}
\alias{es}

\title{
Mean saturation vapour pressure
}
\description{
'es' calculates mean saturation vapour pressure based on air temperature.
}
\usage{
es(Tmax,Tmin)
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
Vector of length n of mean saturation vapour pressure [kPa] }
\references{
Allen, R.G., L.S. Pereira, D. Raes, and M. Smith. 1998. Crop Evapotranspiration: Guidelines for computing crop water requirements. Irrigation and Drainage Paper 56, Food and Agriculture Organization of the United Nations, Rome, pp. 300.
}
\author{
Jedrzej S. Bojanowski
}

%\note{}

%\seealso{}

\examples{
es(Tmax=25.1,Tmin=19.1)
}

%\keyword{}

