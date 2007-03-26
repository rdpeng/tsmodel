\name{lspline}
\alias{lspline}

\title{Linear spline basis}
\description{
  Create a linear spline basis matrix
}
\usage{
lspline(x, df = NULL, knots = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{predictor variable}
  \item{df}{degrees of freedom}
  \item{knots}{internal breakpoints that define the spline.  If
    \code{NULL}, equally spaced quantiles are used}
}
\value{
  A matrix of dimension \code{length(x) * df}.
}
\seealso{\code{\link{ns}}, \code{\link{bs}}}
\keyword{smooth}% at least one, from doc/KEYWORDS

