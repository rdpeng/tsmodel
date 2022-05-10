######################################################################
## Decompose a vector into a matrix of "fourier" components
##                                                          
## Author:  Aidan McDermott (AMcD), modified by Roger Peng <rpeng@jhsph.edu>
## Date  :  Dec 8, 2000
## Revised: 2005-09-29 by Roger Peng <rpeng@jhsph.edu>
######################################################################

#' Time Scale Decomposition
#' 
#' Decompose a vector into frequency components
#' 
#' @param x a numeric vector with no missing data
#' @param breaks a numeric constant or a vector of break points into which \code{x} should be broken. If breaks is a constant then \code{x} will be broken into that number of frequncies. This argument is passed directly to \code{cut} to determine the break points. See \code{cut} for more details.
#' 
#' @returns A matrix with dimension n x m where n is the length of \code{x} and m is the number of break categories.
#' 
#' @references Dominici FD, McDermott A, Zeger SL, Samet JM (2003).  \dQuote{Airborne particulate matter and mortality:  Timescale effects in four US cities}, American Journal of Epidemiology, 157 (12), 1055--1065.
#' @author Original by Aidan McDermott; revised by Roger Peng \email{rpeng@jhsph.edu}
#' 
#' @examples 
#' x <- rnorm(101)
#' freq.x <- tsdecomp(x, c(1, 10, 30, 80))
#' 
#' ## decompose x into 3 frequency categories.
#' ## x[,1] represents from 1 to 9 cycles in 101 data points
#' ## x[,2] represents from 10 to 29 cycles in 101 data points
#' ## x[,3] represents from 30 to 50 cycles in 101 data points
#' ## you can only have up to 50 cycles in 101 data points.
#' 
#' @export
#' @importFrom stats fft
tsdecomp <- function(x, breaks) {
        ## Check for missing values
        nax <- is.na(x)
        if(nas <- any(nax))
                x <- x[!nax]
        
        ## Need to be careful if length(x) is even or odd
        is.even <- !length(x) %% 2
        
        xf  <- fft(x) / length(x)
        xf1 <- xf[1]   # first bit is the sum of x
        xf.first <- xf[2:(1 + floor(length(xf) / 2))]
        
        ## Break xf.first into various components
        cuts  <- cut(seq(length(xf.first)), breaks, include.lowest = TRUE) 
        lcuts <- levels(cuts)
        ncuts <- length(lcuts)
        
        mat <- matrix(0, nrow = length(x), ncol = ncuts)
        
        for(i in 1:ncuts) {
                xf.temp <- rep(0, length(xf.first))
                xf.temp[cuts == lcuts[i]] <- xf.first[cuts == lcuts[i]]

                d <- if(is.even) 
                        c(xf1 / ncuts, xf.temp, rev(Conj(xf.temp[-length(xf.temp)])))
                else 
                        c(xf1 / ncuts, xf.temp, rev(Conj(xf.temp)))
                mat[, i] <- Re(fft(d, inverse = TRUE))
        }
        if(nas) {
                nmat <- matrix(NA, length(nax), NCOL(mat))
                nmat[!nax, ] <- mat
                mat <- nmat
        }
        structure(mat, breaks = breaks, class = c("tsdecomp", "matrix"))
}

#' @importFrom graphics par
#' @exportS3Method 
plot.tsdecomp <- function(x, y, xlab = "", ylab = "", ...) {
        breaks <- attr(x, "breaks")
        xpts <- seq(NROW(x))

        op <- par(no.readonly = TRUE)
        on.exit(par(op))

        par(mfrow = c(NCOL(x), 1), mar = c(3, 4, 2, 2))

        for(i in seq(NCOL(x))) {
                b1 <- breaks[i]
                b2 <- breaks[i+1] - 1
                main <- if(b1 == b2) {
                        if(b1 == 1)
                                paste(b1, "cycle")
                        else
                                paste(b1, "cycles")
                }
                else
                        paste(b1, "to", b2, "cycles")
                plot(xpts, x[, i], type = "l", xlab = xlab, ylab = ylab,
                     main = main, ...)
        }
        invisible()
}
