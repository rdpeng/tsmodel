distmat <- function(x, y, phi) {
        dmat <- dist(cbind(x, y))
        dmat <- as.matrix(dmat)
        exp(-phi * abs(dmat))
}

#' Fit Hierarchical Model with Spatial Covariance
#' 
#' This function fits a Normal hierarchical model with a spatial covariance structure via MCMC.
#' 
#' @param b a vector of regression coefficients
#' @param v {a vector of regression coefficient variances
#' @param x a vector of x-coordinates
#' @param y a vector of y-coordinates
#' @param phi scale parameter for exponential covariance function
#' @param scale scaling parameter for the prior variance of the national average estimate
#' @param maxiter maximum number of iterations in the Gibbs sampler
#' @param burn number of iterations to discard
#' @param a0 parameter for Gamma prior on heterogeneity variance
#' @param b0 parameter for Gamma prior on heterogeneity variance
#' 
#' @details   This function is used to produce pooled national average estimates
#'   of air pollution risks taking into account potential spatial correlation
#'   between the risks.  The function uses a Markov chain Monte Carlo sampler to
#'   produce the posterior distribution of the national average estimate and the
#'   heterogeneity variance.  See the reference below for more details.
#' 
#' @references Peng RD, Dominic F (2008). \emph{Statistical Methods for Environmental Epidemiology in R: A Case Study in Air Pollution and Health}, Springer.
#' @author Roger D. Peng \email{rpeng@jhsph.edu}
#' @importFrom stats rnorm rgamma mahalanobis
#' @export
spatialgibbs <- function(b, v, x, y, phi = 0.1, scale = 1, maxiter = 1000,
                         burn = 500, a0 = 10, b0 = 100000) {
        d <- 1 * scale^2
        D <- diag(d, length(b))
	mu <- 0
        sigma2 <- mean(v)
        
	if(maxiter < burn)
		burn <- 0
        results <- matrix(nrow = maxiter, ncol = 2)
        H <- distmat(x, y, phi)
        R <- chol(H)
        Hinv <- solve(H)
        V <- diag(v)
        I <- diag(1, length(b))
	
	for(i in seq_len(maxiter)) {
                message(i)
                Ainv <- solve(V + sigma2 * H)
                sumAinv <- sum(Ainv)
                
		## Sample mu
                m <- (1/(sumAinv + 1/d)) * sum(Ainv %*% b)
                s2 <- 1/(sumAinv + 1/d)
                mu <- rnorm(1, m, sqrt(s2))
                
		## Sample sigma
                fullc <- makeSigma2FC(b, V, H, mu, a0, b0)
                prop <- 1/rgamma(1, shape = a0, scale = b0)
                
                lr <- fullc(prop) - fullc(sigma2)
                u <- log(runif(1))

                if(is.finite(lr) && !is.na(lr) && u < lr)
                        sigma2 <- prop

                results[i, ] <- c(mu, sigma2)
	}
	if(burn > 0)
		results <- results[-seq_len(burn), ]
        results
}

makeSigma2FC <- function(b, V, H, mu, a0, b0) {
        function(sigma2) {
                (ldmvnorm(b, rep(mu, length(b)), V + sigma2 * H))
                 ## -(a0 + 1) * log(sigma2) - b0 / sigma2)
        }
}
                         
ldmvnorm <- function(x, mean, sigma) {
        if(is.vector(x))
                x <- matrix(x, ncol = length(x))

        sigmaI <- solve(qr(sigma, LAPACK = TRUE))
        distval <- mahalanobis(x, center = mean, cov = sigmaI, inverted = TRUE)
        logdet <- as.numeric(determinant(sigma, logarithm = TRUE)$modulus)
        logretval <- -(ncol(x) * log(2 * pi) + logdet + distval) / 2
        logretval
}

