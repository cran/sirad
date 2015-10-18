\name{ts.CMSAF}
\alias{ts.CMSAF}

\title{
Extract punctual data from CM SAF data.
}
\description{
Function extracts time series for given locations from a set of CM SAF netcdf files.
}
\usage{
ts.CMSAF(files,latlon)
}
\arguments{
  \item{files}{
The vector of file names which the data are to be read from. 
}
  \item{latlon}{
A 2-column matrix with latitude(s), longitude(s) of the location(s) in decimal degrees. First column: latitude; second column: longitude.
}
}
\details{
CM SAF delivers datasets in the NetCDF format. 'ts.CMSAF' allows to extract the time series from a set of those NetCDF files for the specified locations. 
}
\value{
4-column data frame: lat, lon, cmsaf value, date.
}

%\references{}

\author{
Jedrzej S. Bojanowski
}

%\note{}

%\seealso{}

\examples{
\dontrun{ts.CMSAF(files,latlon)}
}

%\keyword{}

