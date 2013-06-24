\name{seasonal}
\alias{seasonal}
\alias{extractBetaCov}
\alias{gibbspool}
\alias{LouisFormat}
\alias{multiDFFit}
\alias{poolCoef}
\alias{pooling}
\alias{coefSeasonal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Reproducible Seasonal Analysis of Air Pollution and Mortality}
\description{
  Functions for conducting a reproducible seasonal analysis of air
  pollution and mortality in the United States.
}
\usage{
coefSeasonal(results, pollutant, method = "factor2",
             Seasons = c("Winter", "Spring", "Summer", "Fall"))
multiDFFit(dfVec, city, ...)
poolCoef(b, cov = NULL, w = NULL, method = c("tlnise", "fixed"),
         extractors = NULL, ...)
extractBetaCov(results, pollutant)
LouisFormat(x, type = c("stderr", "confint"), digits = 2)
gibbspool(b, v, scale = 1, maxiter = 2000, burn = 500,
          pScale = 1e-5)
pooling(estimate, var)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{results}{list of individual city-specific regression fits}
  \item{pollutant}{name of pollutant of interest}
  \item{method}{type of seasonal model, or method of pooling}
  \item{Seasons}{character vector of season names}
  \item{dfVec}{vector of degrees of freedom per year to use in the
    smooth function of time}
  \item{city}{name of a city in the NMMAPS database}
  \item{b}{a vector of N coefficients or an N x p matrix of coefficient
    vectors}
  \item{cov,v}{a vector of N variances or a p x p x N array of covariances
    matrices}
  \item{w}{second-stage covariates to include in the hierarchical model}
  \item{extractors}{a list of functions}
  \item{x}{a numeric matrix}
  \item{type}{type of output to produce}
  \item{digits}{number of digits of output to use}
  \item{scale}{prior standard deviation of the national average}
  \item{maxiter}{maximum number of iterations in the Gibbs sampler}
  \item{burn}{number of iterations to discard}
  \item{pScale}{scaling coefficient for the prior variance of the
    heterogeneity parameter}
  \item{estimate}{a vector of coefficients}
  \item{var}{a vector of coefficient variances}
  \item{...}{other arguments passed to \code{fitCitySeason}}
}
\details{
  These functions are used to reproduce a seasonal analysis of air
  pollution and mortality in the United States (see the reference
  below).
}

\references{
  Peng RD, Dominic F (2008). \emph{Statistical Methods for Environmental
    Epidemiology in R: A Case Study in Air Pollution and Health}, Springer.
}
\author{Roger D. Peng \email{rpeng@jhsph.edu}}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
