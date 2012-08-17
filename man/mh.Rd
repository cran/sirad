\name{mh}
\alias{mh}

\title{
Mahmood-Hubbard solar radiation model
}
\description{
'mh()' calculates daily solar radiation based on daily temperature range using Mahmood-Hubbard model.
}
\usage{
mh(days, lat, Tmax, Tmin)
}

\arguments{
  \item{days}{
Vector of class 'Date' of length n.
}
  \item{lat}{
Latitude in decimal degrees.
}
  \item{Tmax}{
Vector of length n containing daily maximum temperature [C].
}
  \item{Tmin}{
Vector of length n containing daily minumum temperature [C].
}
}
\details{
Mahmood and Hubbard proposed a method for estimating solar radiation from air temperature measurements without a need of calibraing empirical coefficients.
}
\value{
Vector of length n of daily solar radiation [MJm-2].
}
\references{
Mahmood, R., and K.G. Hubbard. 2002. Effect of time of temperature observation and estimation of daily solar radiation for the Northern Great Plains, USA. Agron. J., 94:723-733. 
}
\author{
Jedrzej S. Bojanowski
}

%\note{}

\seealso{
'bc' and 'ha' to calculate solar radiation based on temperature range using different models.
}
\examples{
require(zoo)
data(Metdata)
tmax <- Metdata$meteo$TEMP_MAX
tmin <- Metdata$meteo$TEMP_MIN
days <- Metdata$meteo$DAY
lat <- Metdata$LATITUDE
plot(zoo(mh(days=days, lat=lat, Tmax=tmax, Tmin=tmin),order.by=days))

}

%\keyword{}

