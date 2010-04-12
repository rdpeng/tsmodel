\name{tsdecomp}
\alias{tsdecomp}

\title{Time scale decomposition}
\description{
  Decompose a vector into frequency components
}
\usage{
tsdecomp(x, breaks)
}

\arguments{
  \item{x}{a numeric vector with no missing data}
  \item{breaks}{a numeric constant or a vector of break points into which
    \code{x} should be broken. If breaks is a constant then \code{x} will be broken
    into that number of frequncies. This argument is passed directly to
    \code{cut} to determine the break points. See \code{cut} for more details.} 
}
\value{
  A matrix with dimension n x m where n is the length of \code{x} and m
  is the number of break categories.
}

\references{
  Dominici FD, McDermott A, Zeger SL, Samet JM (2003).  \dQuote{Airborne
    particulate matter and mortality:  Timescale effects in four US
    cities}, American Journal of Epidemiology, 157 (12), 1055--1065.
}
\author{Original by Aidan McDermott; revised by Roger Peng
  \email{rpeng@jhsph.edu}}

\examples{
x <- rnorm(101)
freq.x <- tsdecomp(x, c(1, 10, 30, 80))

## decompose x into 3 frequency categories.
## x[,1] represents from 1 to 9 cycles in 101 data points
## x[,2] represents from 10 to 29 cycles in 101 data points
## x[,3] represents from 30 to 50 cycles in 101 data points
## you can only have up to 50 cycles in 101 data points.
}
\keyword{models}% at least one, from doc/KEYWORDS

