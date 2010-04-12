\name{spatialgibbs}
\alias{spatialgibbs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fit Hierarchical Model with Spatial Covariance}
\description{
  This function fits a Normal hierarchical model with a spatial
  covariance structure via MCMC.
}
\usage{
spatialgibbs(b, v, x, y, phi = 0.1, scale = 1, maxiter = 1000, burn = 500, a0 = 10, b0 = 100000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{b}{a vector of regression coefficients}
  \item{v}{a vector of regression coefficient variances}
  \item{x}{a vector of x-coordinates}
  \item{y}{a vector of y-coordinates}
  \item{phi}{scale parameter for exponential covariance function}
  \item{scale}{scaling parameter for the prior variance of the national
    average estimate}
  \item{maxiter}{maximum number of iterations in the Gibbs sampler}
  \item{burn}{number of iterations to discard}
  \item{a0}{parameter for Gamma prior on heterogeneity variance}
  \item{b0}{parameter for Gamma prior on heterogeneity variance}
}
\details{
  This function is used to produce pooled national average estimates of
  air pollution risks taking into account potential spatial correlation
  between the risks.  The function uses a Markov chain Monte Carlo
  sampler to produce the
  posterior distribution of the national average estimate and the
  heterogeneity variance.  See the reference below for more details.
}

\references{
  Peng RD, Dominic F (2008). \emph{Statistical Methods for Environmental
    Epidemiology in R: A Case Study in Air Pollution and Health}, Springer.
}
\author{Roger D. Peng \email{rpeng@jhsph.edu}}
\keyword{models}
