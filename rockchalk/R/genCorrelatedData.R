##' Generates a data frame (x1,x2,y) with user-specified correlation between x1 and x2
##'
##' The vector (x1,x2) is drawn from a multivariate normal
##' distribution in which the expected value (mean) is the
##' parameter \code{means} and the var/covar matrix is
##' built from the assumed standard deviations \code{sds}
##' and the correlation between x1 and x2 (\code{rho}).
##' It is also necessary to specify the standard deviation
##' of the error term (\code{stde}) and the coefficients
##' of the regression equation (\code{beta}).
##' The y (output) variable is created according to the
##' equation
##' y = b1 + b2 * x1 + b3 * x2 + b4 * x1 * x2 + e
##' @param N Number of cases desired
##' @param means 2-vector of means for x1 and x2
##' @param sds 2-vector of standard deviations for x1 and x2
##' @param rho Correlation coefficient for x1 and x2
##' @param stde standard deviation of the error term in the data generating equation
##' @param beta beta vector of at most 4 coefficients for intercept, slopes, and interaction
##' @import MASS
##' @export genCorrelatedData
##' @examples
##' ## 1000 observations of uncorrelated x1 and x2 with no
##' ## interaction between x1 and x2
##' dat <- genCorrelatedData(N=1000, rho=0, beta=c(1, 1.0, -1.1, 0.0))
##' mcGraph1(dat$x1, dat$x2, dat$y, theta=20, phi=8, ticktype="detailed", nticks=10)
##' m1 <- lm(y ~ x1 + x2, data = dat)
##' plotPlane(m1, plotx1 = "x1", plotx2 = "x2")
genCorrelatedData <- function(N = 100, means = c(50,50), sds = c(10,10), rho = 0.0, stde = 1, beta = c(0, 0.2, 0.2, 0.0)){ 
  require(MASS)
  if (length(beta)> 4) stop("beta vector can have at most 4 values")
  corr.mat <- matrix(c(1,rho,rho,1), nrow = 2)
  sigma <- diag(sds) %*% corr.mat %*% diag(sds)
  x.mat <-  mvrnorm(n = N, mu = means, Sigma = sigma)
  y = beta[1] + beta[2] * x.mat[,1] + beta[3] * x.mat[,2] + beta[4] * x.mat[,1] * x.mat[ ,2] +  stde*rnorm (N, mean = 0, sd = 1)
  dat <- data.frame(x.mat, y)
  names(dat) <- c("x1", "x2", "y")
  dat
}
  
