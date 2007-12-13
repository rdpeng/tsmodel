\name{fitCitySeason}
\alias{fitCitySeason}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fit seasonally varying coefficient model}
\description{
  Fit a seasonally varying coefficient model to NMMAPS data
}
\usage{
fitCitySeason(data, pollutant = "l1pm10tmean", cause = "death",
              season = c("none", "periodic", "factor2"),
              tempModel = c("default", "rm7", "tempInt", "SeasonInt"),
              dfyr.Time = 7, pdfyr.time = 0.15, df.Temp = 6, df.Dew = 3,
              df.Season = 1, obsThreshold = 50, extractors = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{data frame for an NMMAPS city}
  \item{pollutant}{name of pollutant variable}
  \item{cause}{name of cause of death; choices are "death" (all
    non-accidental), "cvd" (cardiovascular), or "resp" (respiratory)}
  \item{season}{type of seasonal model to fit}
  \item{tempModel}{type of temperature model to use}
  \item{dfyr.Time}{number of degrees of freedom per calendar year of
    data to use in the smooth function of time}
  \item{pdfyr.time}{fraction of the degrees of freedom used in the
    overall smooth function of time to use in the
    smooth function of time specific to the older age category}
  \item{df.Temp}{degrees of freedom to use in the smooth function of
    temperature}
  \item{df.Dew}{degrees of freedom to use in the smooth function of dew
    point temperature}
  \item{df.Season}{number of sine/cosine pairs to include in the
    "periodic" model}
  \item{obsThreshold}{minimum number of observations required before a
    model can be fit to the data}
  \item{extractors}{a list of functions which extract elements of the
    \code{"glm"} object.}
}
\details{
  This function fits a seasonally varying coefficient model to data from
  the NMMAPS study using two different seasonal models.  The first is a
  step function model ("factor2") which uses indicators for the
  seasons.  The second is a hamonic model ("periodic") which uses sines
  and cosines.
}
\value{
  An object of class \code{"glm"} is returned.
}
\references{
    Peng RD, Dominici F, Pastor-Barriuso R, Zeger SL, Samet JM
    (2005). \dQuote{Seasonal Analyses of Air Pollution and Mortality in 100 US
    Cities,} American Journal of Epidemiology, 161 (6), 585--595.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
