\name{CFC}
\alias{CFC}
\docType{data}
\title{
Annual mean of cloud fraction cover
}
\description{
This dataset contains a raster of annual mean of cloud fraction cover
}
\usage{
data(CFC)
}
%\format{ }
\details{
Annual mean of cloud fraction cover is used as a proxy of a number of potential
clear-sky days at given location.
}

\source{EUMETSAT's Satellite Application Facility on Climate Monitoring}

\references{Derrien, M., LeGleau, H., 2005. MSG/SEVIRI cloud mask and type from SAFNWC. International Journal of Remote Sensing, 26, 4707-4732.}

\examples{
if(require(raster))
{
  data(CFC)
  # str(CFC)
  show(CFC)
}
}
\keyword{datasets}
