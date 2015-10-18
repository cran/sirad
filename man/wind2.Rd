\name{wind2}
\alias{wind2}

\title{
Convert wind speed measured at a certain height to the wind speed at 2 meters 
}
\description{
'wind2' converts a wind speed measured at a certain height 'z' above the ground level to the wind speed at the standard height (2 meters)
}
\usage{
wind2(uz,meah)
}

\arguments{
  \item{uz}{
Wind speed measured at heith 'z' [ms-1].
}
  \item{meah}{
The height (above the ground level) of the wind speed measurement [m].
}
}
\details{
Wind speed is slowest at the surface and increases with height. The measurements taken at different heights avove the ground level must be standardized to 2 meters (default in agrometeorology).
}
\value{
Wind speed at standard 2 meters. [ms-1] }
\references{
Allen, R.G., L.S. Pereira, D. Raes, and M. Smith. 1998. Crop Evapotranspiration: Guidelines for computing crop water requirements. Irrigation and Drainage Paper 56, Food and Agriculture Organization of the United Nations, Rome, pp. 300.
}
\author{
Jedrzej S. Bojanowski
}

%\note{}

%\seealso{}

\examples{
wind2(uz=5,meah=10)
}

%\keyword{}

