\name{rnl}
\alias{rnl}

\title{
Net longwave radiation 
}
\description{
'rnl' computes daily net energy flux emitted by the Earth's surface.
}
\usage{
rnl(Tmax,Tmin,sol_rad,vap_pres,extraT,tal)
}

\arguments{
  \item{Tmax}{
Vector of length n containing daily maximum temperature [C].
}
  \item{Tmin}{
Vector of length n containing daily minumum temperature [C].
}
  \item{sol_rad}{
Vector of length n of daily solar radiation [MJm-2d-1].
}
  \item{vap_pres}{
Vector of length n of mean daily vapour pressure [kPa]. 
}
  \item{extraT}{
Vector of length n of extraterrestrial solar radiation [MJm-2d-1]. 
}
  \item{tal}{
Clear sky transmissivity.
}
}
\details{
According to the Stefan-Boltzmann law, the longwave energy emission is proportional to the absolute temperature of the surface raised to the fourth power. This longwave energy is corrected by two factors: humidity ('ea') and cloudiness (estimated based on relation of actual and potential solar radiation.  See Allen et al. (1998) for details.
}
\value{
Vector of length n of daily net longwave radiation. [MJm-2d-1] }
\references{
Allen, R.G., L.S. Pereira, D. Raes, and M. Smith. 1998. Crop Evapotranspiration: Guidelines for computing crop water requirements. Irrigation and Drainage Paper 56, Food and Agriculture Organization of the United Nations, Rome, pp. 300.
}
\author{
Jedrzej S. Bojanowski
}

%\note{}

\seealso{
See 'ea', 'extrat' and 'cst' to calculate necessary input data.
}
\examples{
rnl(Tmax=25.1,Tmin=19.1,sol_rad=14.5,vap_pres=2.1,extraT=23.5,tal=0.8)
}

%\keyword{}

