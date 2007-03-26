\name{ModelTerms}
\alias{ModelTerms}
\alias{Lag}
\alias{dLag}
\alias{xLag}
\alias{runMean}
\alias{harmonic}

\title{Model terms for time series models}
\description{
  Tools for creating model/formula terms in time series models
}
\usage{
Lag(v, k, group = NULL)
dLag(v, k, group = NULL)
xLag(v, k, group = NULL)
runMean(v, lags = 0, group = NULL, filter = NULL)
harmonic(x, nfreq, period, intercept = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v,x}{a numeric vector}
  \item{k,lags}{an integer vector giving lag numbers}
  \item{group}{a factor or a list of factors defining groups of observations}
  \item{filter}{a vector specifying a linear filter}
  \item{nfreq}{number of sine/cosine pairs to include}
  \item{period}{period}
  \item{intercept}{should basis matrix include a column of 1s?}
}
\details{
  \code{dLag} is similar to \code{Lag} except that it produces lags of
  the vector \code{v} that are mutually orthogonal.  The first lag
  (i.e. the first lag specified in \code{k}) is always unchanged.

  \code{xLag} returns a matrix of interactions.
}
\value{
  \code{Lag} and \code{dLag} return a \code{length(v)} by \code{length(k)} matrix of
  lagged variables.  \code{runMean} returns a numeric vector of length
  \code{length(v)}.  \code{harmonic} returns a matrix of sine/cosine
  basis functions.
}

\author{Roger D. Peng}
\seealso{}
\examples{
## Ten day "time series"
x <- rnorm(10)

## Lag 1 of `x'
Lag(x, 1)

## Lag 0, 1, and 2 of `x'
Lag(x, 0:2)

## Lag 0, 1, and 2 orthogonalized
dLag(x, 0:2)

## Running mean of lag 0, 1, and 2
runMean(x, 0:2)

}

\keyword{models}
