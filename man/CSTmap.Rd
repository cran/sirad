\name{CSTmap}
\alias{CSTmap}
\docType{data}
\title{
Clear sky transmissivity map
}
\description{
This dataset contains a raster of clear sky transmissivity 
}
\usage{
data(CSTmap)
}
%\format{ }
\details{
The map of clear sky transmissivity was generated using 'cst' function based on Meteosat Second Generation solar radiation.
}

%\source{}

%\references{}

\examples{
if(require(raster))
{
  data(CSTmap)
  # str(CFC)
  show(CSTmap)
}
}
\keyword{datasets}
