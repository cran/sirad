\name{modeval}
\alias{modeval}

\title{
Estimators of the model performance.
}
\description{
Function estimates several statistics comparing modelled and reference (measured) values.
}
\usage{
modeval(calculated,measured)
}

\arguments{
  \item{calculated}{
Vector of length n of the calculated (modelled) values.
}
  \item{measured}{
Vector of length n of the reference (measured) values.
}

}
\details{
The two input vectors can include NA's. Only non-NA calculated-mesured pairs are used. See 'na.omit' for details.  
}
\value{
List of 13 statistics:
\item{N}{number of observations}  
\item{person}{Pearson's Correlation Coefficient}
\item{MBE}{Mean (Bias) Error}
\item{MAE}{Mean Absolute Error}
\item{RMSE}{Root Mean Square Error}
\item{RRMSE}{Relative Root Mean Square Error}
\item{R2}{Coefficient of determination from linear model}
\item{slope}{Slope from linear model}
\item{intercept}{Intercept from linear model}
\item{EF}{Modelling Efficiency}
\item{SD}{Standard deviation of differences}
\item{CRM}{Coefficient of Residual Mass}
\item{MPE}{Mean Percentage Error}
\item{AC}{Agreement Coefficient}
}

%\references{}

\author{
Jedrzej S. Bojanowski
}

%\note{}

%\seealso{}

\examples{

data(Metdata)
B <- 0.11
tmax <- Metdata$meteo$TEMP_MAX
tmin <- Metdata$meteo$TEMP_MIN
days <- Metdata$meteo$DAY
lat <- Metdata$LATITUDE
solrad_measured <- Metdata$meteo$RAD_MEA
solrad_BC <- bc(days, lat, BCb=B, tmax, tmin, BCc=2, tal=0.76) 

modeval(solrad_BC,solrad_measured)
}

%\keyword{}

