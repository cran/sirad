\name{rns}
\alias{rns}

\title{
Net shortwave radiation 
}
\description{
'rns' computes daily the net shortwave radiation. resulting from the balance between incoming and reflected solar radiation. 
}
\usage{
rns(sol_rad,albedo=0.23)
}

\arguments{

  \item{sol_rad}{
Vector of length n of daily solar radiation [MJm-2d-1].
}
  \item{albedo}{
Albedo or canopy reflection coefficient, which is 0.23 for the hypothetical grass reference crop [dimensionless].
}
}
\details{
Daily net shortwave radiation results from the balance between incoming and reflected solar radiation.
}
\value{
Vector of length n of daily net shortwave radiation. [MJm-2d-1] }
\references{
Allen, R.G., L.S. Pereira, D. Raes, and M. Smith. 1998. Crop Evapotranspiration: Guidelines for computing crop water requirements. Irrigation and Drainage Paper 56, Food and Agriculture Organization of the United Nations, Rome, pp. 300.
}
\author{
Jedrzej S. Bojanowski
}

%\note{}

%\seealso{}

\examples{
rns(sol_rad=14.5)
}

%\keyword{}

