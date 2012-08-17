\name{extrat}
\alias{extrat}

\title{
Calculate extraterrestrial solar radiation
}
\description{
'extrat' calculates hourly and daily extraterrestrial solar radiation for a given time and location.
}
\usage{
extrat(i, lat)
}

\arguments{
  \item{i}{
day number in the year (julian day)
}
  \item{lat}{
latitude in radians
}
}
\details{
Solar radiation outside of the earth's atmosphere is called extraterrestrial solar radiation. It can be calculated based on solar geometry.
}
\value{
List of 3 elements:
\item{ExtraTerrestrialSolarRadiationDaily}{daily sum of extraterrestrial radiation}  
\item{TerrestrialSolarRadiationHourly}{vector of length 24 of hourly sums of extraterrestrial radiation}
\item{DayLength}{day length in hours}

}

%\references{}

\author{
Jedrzej S. Bojanowski
}

%\note{}

%\seealso{}

\examples{
## extraterrestrial radiation and daylength for 1 January and latitude 55 degrees
extrat(dayOfYear("2011-01-01"), radians(55))
}

%\keyword{}

