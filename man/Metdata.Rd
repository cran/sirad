\name{Metdata}
\alias{Metdata}
\docType{data}
\title{
Weather data
}
\description{
This dataset contains two years of daily data of sunshine hours, solar radiation, minimum temperature, maximum temperature, cloud coverage, vapour pressure, and wind speed. 
}
\usage{data(Metdata)}
\format{
\tabular{llll}{
          \tab NAME    \tab chr \tab Name \cr
          \tab LATITUDE    \tab numeric \tab Latitude (decimal degree) \cr
          \tab LONGITUDE    \tab numeric \tab Longitude (decimal degree) \cr
          \tab DCOAST    \tab numeric \tab Distance to the coast (km) \cr
          \tab ALTITUDE    \tab numeric \tab Altitude above the sea level (m) \cr
          \cr
          \tab DAY    \tab Date \tab Date \cr
          \tab SUNSHINE  \tab numeric \tab Sunshine (hours)\cr
          \tab RAD_MEA     \tab numeric \tab Solar radiation (MJm-2)\cr
          \tab TEMP_MIN    \tab numeric \tab Minimum temperature (degrees C)\cr
          \tab TEMP_MAX   \tab numeric \tab Maximum temperature (degrees C)\cr
          \tab CLOUD_DAYTIME_TOTAL     \tab numeric \tab Cloud coverage (octas) \cr
          \tab VAP_PRES     \tab numeric \tab Vapour pressure (kPa) \cr
          \tab WIND_10     \tab numeric \tab Wind speed at 10 m height (ms-1)
       }       

}
%\details{}

%\source{}

%\references{}

\examples{
data(Metdata)
str(Metdata)

}
%\keyword{datasets}
