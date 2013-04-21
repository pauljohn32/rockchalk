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
NULL

##' Generates a data frame (y, x1, x2, ..., xp) with user-specified coefficients and
##' inter-correlations, allowing for interaction effects.
##'
##' The matrix X is drawn from a multivariate normal
##' distribution, the expected value vector (mu vector) is given by
##' the \code{means} and the var/covar matrix (Sigma) is
##' built from user supplied standard deviations \code{sds}
##' and the correlations between the columns of X, given by \code{rho}.
##' The user can also set the standard deviation
##' of the error term (\code{stde}) and the coefficients
##' of the regression equation (\code{beta}).
##'
##' If called with no arguments, this creates a data frame
##' X ~ MVN(mu = c(50,50,50), Sigma = diag(c(10,10,10))).
##' y = X %*% c(0.15, 0.1, -0.1) + error, where error
##' is N(m =0, sd = 200). All of these details can be
##' changed by altering the arguments.
##'
##' The y (output) variable is created according to the
##' equation
##'
##' y = b1 + b2 * x1 + ...+ bk * xk + b[k+1] * x1 * ...interactions.. + e
##'
##' It appears the tricky part will be the user specification of the vector
##' beta. That vector has to provide one coefficient for the intercept
##' and one for each column of the X matrix. Additional values in the
##' beta vector are interpreted as interaction effects, beginning
##' with variable one. Suppose there are 4 columns in X. Then a beta vector
##' like beta = c(0, 1, 2, 3, 4, 5, 6, 7, 8) would amount to asking for
##'
##' y = 0 + 1 x1 + 2 x2 +3 x3 + 4 x4 + 5 *x1^1 + 6 x1 x2 + 8 x1 x3 + 8 x1 x4 + error
##'
##' If beta supplies more coefficients, they are interpeted as additional
##' interactions. It is still necessary to work out the details on how a very
##' long string of betas is sorted without "double-counting" some interactions.
##'
##' @param N Number of cases desired
##' @param means P-vector of means for X
##' @param sds P-vector of standard deviations for X
##' @param rho Correlation coefficient for X. Several input formats
##' are allowed (see \code{lazyCor}). Can be a single number (common
##' correlation among all variables), a full matrix of correlations
##' among all variables, or a vector that is interpreted as the
##' strictly lower triangle (commonly called a vech).
##' @param stde standard deviation of the error term in the data generating equation
##' @param beta beta vector of coefficients for intercept, slopes, and interaction.
##' It needs to have at least (P+1) values, for an intercept and each column of X. Beyond that, I'm still deciding what to do (2013-04-18)
##' @import MASS
##' @export
##' @examples
##' ## 1000 observations of uncorrelated X with no interactions
##' set.seed(234234)
##'  dat <- genCorrelatedData2(N=10, rho=c(0.0), beta=c(1, 2, 1, 1), means = c(0,0,0), sds = c(1,1,1), stde = 0)
##' summarize(dat)
##' ## The perfect regression!
##' m1 <- lm(y ~ x1 + x2 + x3, data = dat)
##' summary(m1)
##'
##' dat <- genCorrelatedData2(N = 1000, rho = 0, beta = c(1, 0.2, -3.3, 1.1), stde = 50)
##' m1 <- lm(y ~ x1 + x2 + x3, data = dat)
##' summary(m1)
##' predictOMatic(m1)
##' plotCurves(m1, plotx = "x2")
##'
##' ## interaction between x1 and x2
##' dat <- genCorrelatedData2(N=1000, rho=c(0.2), beta=c(1, 1.0, -1.1, 0.1, 0.0, 0.16), stde = 1)
##' summarize(dat)
##' ## Fit wrong model? get "wrong" result
##' m2w <- lm(y ~ x1 + x2 + x3, data = dat)
##' summary(m2w)
##' ## Include interaction
##' m2 <- lm(y ~ x1 * x2 + x3, data = dat)
##' summary(m2)
##'
##' dat <- genCorrelatedData2(N=1000, rho=c(0.2), beta=c(1, 1.0, -1.1, 0.1, 0.0, 0.16), stde = 100)
##' summarize(dat)
##' m2.2 <- lm(y ~ x1 * x2 + x3, data = dat)
##' summary(m2.2)
##'
##'
##' dat <- genCorrelatedData2(N=1000, rho=c(0.2), beta=c(1, 1.0, -1.1, 0.1, 0.0, 0.16), stde = 200)
##' summarize(dat)
##' m2.3w <- lm(y ~ x1 + x2 + x3, data = dat)
##' summary(m2)
##'
##' m2.3 <- lm(y ~ x1 * x2 + x3, data = dat)
##' summary(m2.3)
##'
##' predictOMatic(m2.3)
##' plotCurves(m2.3, plotx = "x1", modx = "x2", modxVals = "std.dev.", n = 5)
##'
##' simReg <- lapply(1:100, function(x){
##'     dat <- genCorrelatedData2(N=10000, rho=c(0.2), beta=c(1, 1.0, -1.1, 0.1, 0.0, 0.46))
##'     mymod <- lm (y ~ x1 * x2 + x3, data = dat)
##'     summary(mymod)
##' })
##'
##' b5est <- sapply(simReg, function(reg) {coef(reg)[4 ,1] })
##' summarize(b5est)
##' hist(b5est,  breaks = 40)
##'
##' ## No interaction, collinearity
##' dat <- genCorrelatedData2(N=10000, rho=c(0.1, 0.2, 0.7), beta=c(1, 1.0, -1.1, 0.1), stde = 1)
##' m3 <- lm(y ~ x1 + x2 + x3, data = dat)
##' summary(m3)
##'
##' dat <- genCorrelatedData2(N=10000, rho=c(0.1, 0.2, 0.7), beta=c(1, 1.0, -1.1, 0.1), stde = 200)
##' m3 <- lm(y ~ x1 + x2 + x3, data = dat)
##' summary(m3)
##' mcDiagnose(m3)
##'
##' dat <- genCorrelatedData2(N=10000, rho=c(0.9, 0.5, 0.4), beta=c(1, 1.0, -1.1, 0.1), stde = 200)
##' m3b <- lm(y ~ x1 + x2 + x3, data = dat)
##' summary(m3b)
##' mcDiagnose(m3b)
genCorrelatedData2 <-
    function(N = 100, means = c(50,50,50), sds = c(10,10,10), rho = c(0.0, 0.0, 0.0), stde = 200, beta = c(0, 0.15, 0.1, -0.1))
{
    require(MASS)
    ## if (length(beta)> 4) stop("beta vector can have at most 4 values")
    ## corr.mat <- matrix(c(1,rho,rho,1), nrow = 2)
    ## sigma <- diag(sds) %*% corr.mat %*% diag(sds)
    d <- length(means)
    R <- lazyCor(rho, d)
    Sigma <- lazyCov(Rho = R, Sd = sds)
    ##used mvtnorm ## x.mat <-  rmvnorm(n = N, mean = means, sigma = Sigma)
    x.mat <- mvrnorm(N, means, Sigma)
    beta1 <- beta[1:(d+1)]
    beta2 <- beta[-(1:(d+1))]

    ## need this? maybe for visualization
    ## beta2mat <- matrix(c(beta2, rep(0, d*d - length(beta2))), nrow = d, ncol = d)
    ## interaction effect
    intEffect <- 0
    for (i in seq_along(beta2)) {
        ## beta2 is in sections of length d
        j <- 1 + floor(i / (d+1))
        ## j is which column is first interactor
        k <- (i - (j -1) *d) %% (d+1)
        intEffect <- intEffect + beta2[i] * x.mat[ , j] * x.mat[ , k]
    }
    y = cbind(1, x.mat) %*% beta1 + intEffect +  stde*rnorm (N, mean = 0, sd = 1)

    dat <- data.frame(y, x.mat)
    names(dat) <- c("y", paste("x", 1:dim(x.mat)[2] , sep = ""))
    dat
}
NULL


##' Convert the vech (column of strictly lower trianglar values from a matrix) into a correlation matrix.
##'
##' vech2Corr is a convenience function for creating correlation matrices
##' from a vector of the lower triangular values. It checks the arguments
##' to make sure they are consistent with the requirements of a
##' correlation matrix. All values must be in [-1, 1], and the number
##' of values specified must be correct for a lower triangle.
##'
##' Use this in combination with the \code{lazyCov} function to
##' convert a vector of standard deviations and the correlation matrix
##' into a covariance matrix.
##'
##' @export
##' @seealso Similar functions exist in many packages, see  \code{vec2sm} in corpcor, \code{xpnd} in MCMCpack
##' @param vech A vector of values for the strictly lower triangle of
##' a matrix. All values must be in the [0,1] interval (because they
##' are correlations) and the matrix formed must be positive definite.
##' @return A symmetric correlation matrix, with 1's on the diagonal.
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @examples
##' v <- c(0.1, 0.4, -0.5)
##' vech2Corr(v)
##' v <- c(0.1, 0.4, -0.4, 0.4, 0.5, 0.1)
##' vech2Corr(v)
##'
vech2Corr <- function(vech){
    ##compute number of rows from vech. diag not in the vech!
    n <- (sqrt(1 + 8 * length(vech)) + 1)/2
    if (!as.integer(n) == n) stop(deparse(substitute(vech)), " must have the correct number of elelemnts to fill in a strictly lower triangle in a square matrix.")
    if(any(vech > 1 | vech < -1)) stop("All values in ", deparse(substitute(vech)), " must be in the interval [-1,1]")
    X <- matrix(NA, nrow = n, ncol = n)
    X[lower.tri(X, diag = FALSE)] <- vech
    X[upper.tri(X)] <- t(X)[upper.tri(X)]
    diag(X) <- 1
    stopifnot(checkPosDef(X))
    X
}
NULL

##' Convert a half-vector (vech) into a matrix.
##'
##' This may treat the input vech as the fill material for a
##' strictly lower triangle (no diagonal) or it may include
##' the diagonal values, depending on the value of the diag
##' argument. If diag = NULL (default), this will treat vech as if
##' it includes values for the diagonal. If diag is either a number or
##' a vector, this will treat vech as strictly lower triangular and
##' fill the diagonal with the value provided by diag.
##' @param vech A vector
##' @param diag Optional. If supplied, We assume vech is a strictly
##' lower triangluar vech, it does not include diagonal values. diag
##' can be either a single value (to replace all elements along the
##' diagonal) or a vector of the correct length to replace the
##' diagonal.
##' @seealso Similar functions exist in many packages, see
##' \code{vec2sm} in corpcor, \code{xpnd} in MCMCpack
##' @export
##' @examples
##' x <- 1:6
##' vech2mat(x)
##' vech2mat(x, diag = 7)
##' vech2mat(x, diag = c(99, 98, 97, 96))
##'
vech2mat <- function(vech, diag = NULL){
    ## Must calculate correct number of rows from vech, if
    ## vech implies a non-square, stop.
    ## If no diag, then vech provides the diagonal values
    if (!is.null(diag)){
        d <- (sqrt(1 + 8 * length(vech)) + 1)/2
        if (!as.integer(d) == d) stop(deparse(substitute(vech)), " must have the correct number of elements to fill a stricly lower triangle.")
        X <- matrix(NA, nrow = d, ncol = d)
        X[lower.tri(X, diag = FALSE)] <- vech
        diag(X) <- makeVec(diag, d)
        X[upper.tri(X)] <- t(X)[upper.tri(X)]
    } else {
        d <- (sqrt(1 + 8 * length(vech)) - 1)/2
        if (!as.integer(d) == d) stop(paste("You supplied diag. So ", deparse(substitute(vech)), " must have the correct number of elements to fill in a lower triangle, including the diagonal.."))
        X <- matrix(NA, nrow = d, ncol = d)
        X[lower.tri(X, diag = TRUE)] <- vech
        X[upper.tri(X)] <- t(X)[upper.tri(X)]
    }
    X
}
NULL




##' Create covariance matrix from correlation and standard deviation
##' information
##'
##' This is a flexible function that allows lazy R programmers to
##' create covariance matrix. The user may be lazy because the
##' correlation and standard deviation infomation may be supplied in a
##' variety of formats.
##'
##' @param Rho Required. May be a single value (correlation common
##' among all variables), a vector of the lower triangular values
##' (vech) of a correlation matrix, or a symmetric matrix of
##' correlation coefficients.
##' @param Sd Required. May be a single value (standard deviation
##' common among all variables) or a vector of standard deviations,
##' one for each variable.
##' @param d Optional. Number of rows or columns. lazyCov may be able
##' to deduce the required dimension of the final matrix from the
##' input. However, when the user supplies only a single value for
##' both Rho and Sd, d is necessary.
##' @return covariance matrix.
##' @author <pauljohn@@ku.edu>
##' @export
##' @examples
##' ##correlation 0.8 for all pairs, standard deviation 1.0 of each
##' lazyCov(Rho = 0.8, Sd = 1.0, d = 3)
##' ## supply a vech (lower triangular values in a column)
##' lazyCov(Rho = c(0.1, 0.2, 0.3), Sd = 1.0)
##' ## supply vech with different standard deviations
##' lazyCov(Rho = c(0.1, 0.2, 0.3), Sd = c(1.0, 2.2, 3.3))
##' newRho <- lazyCor(c(0.5, 0.6, 0.7, -0.1, 0.1, 0.2))
##' lazyCov(Rho = newRho, Sd = 1.0)
##' lazyCov(Rho = newRho, Sd = c(3, 4, 5, 6))
lazyCov <- function(Rho, Sd, d){
    if (missing(Sd)) stop("lazyCov requires user to specify either a vector or a single common value for all standard deviations")
    if (missing(Rho)) stop("lazyCov requires a symmstric correlation matrix or enough information to create one, either a vech of lower triangular values or a single common correlation value")
    if (!missing(d) && (length(Sd) > 1) && (length(Sd) != d)) stop("lazyCov doesn't require a d argument, but if you provide one, it must be consistent with the length of a supplied Sd vector")
    if (missing(d)){
        if (length(Sd) > 1) d <- length(Sd)
        else if (is.matrix(Rho)) d <- NROW(Rho)
        else if (is.vector(Rho)) {
            d <- (sqrt(1 + 8 * length(Rho)) + 1)/2
            if (!isTRUE(all.equal(as.integer(d)- d, 0))) stop(deparse(substitute(vech)), " must have the correct number of elelemnts to fill in a strictly lower triangle in a square matrix.")
        }
    }
    if (length(Sd) == 1) Sd <- rep(Sd, d)
    Rho <- lazyCor(Rho, d)

    covMat <- diag(Sd) %*% Rho %*% diag(Sd)
    covSVD <- svd(covMat, nu = 0, nv = 0)
    eS <- eigen(covMat, symmetric = TRUE, only.values = TRUE)
    ev <- eS$values
    tol <- 1e-6
    if(!all(ev >= -tol*abs(ev[1L]))) stop("'covMat' is not positive definite")
    covMat
}



##' Create correlation matrices.
##'
##' Use can supply either a single value (the common correlation among
##' all variables), a column of the lower triangular values for a
##' correlation matrix, or a candidate matrix. The function will check
##' X and do the right thing. If X is a matrix, check that it
## is a valid correlation matrix. If its a single value, use that
## to fill up a matrix. If itis a vector, try to use it as a vech
## to fill the lower triangle..
##' @param X Required. May be one value, a vech, or a matrix
##' @param d Optional. The number of rows in the correlation matrix to
##' be created. lazyCor will deduce the desired size from X if
##' possible. If X is a single value, d is a required argument.
##' @return A correlation matrix.
##' @export
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @examples
##' lazyCor(0.5, 8)
##' lazyCor(c(0.1, 0.2, 0.3))
##' lazyCor(c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6))
lazyCor <- function(X, d) {
    if (is.matrix(X)){
        stopifnot (isSymmetric(X))
        if (!dim(X)[1] == d) stop("lazyCor: the dimension of the matrix supplied is inconsistent with the dimension argument d")
    } else if (length(X) == 1) {
        if ( X < -1 | X > 1 ) stop(paste("The value of a correlation should be in [-1,1]"))
        X <- matrix(X, nrow = d, ncol = d)
        diag(X) <- 1.0
    } else if (is.vector(X)){
        X <- vech2Corr(X)
    } else {
        stop(paste("lazyCor cannot understand the value supplied for argument", deparse(substitute(X)),".\n That should be either a", d, " x ", d, "symmetric matrix, \n or a vech of the strictly lower triangular part of a matrix, or \n one single value, which we will use to fill up a matrix."))
    }
    if (!checkPosDef(X)) stop("'correlation matrix' is not positive definite")
    X
}
NULL



##' makeVec for checking or creating vectors
##'
##' This is a convenience for handling function arguments. If x is a
##' single value, it makes a vector of length d in which all values
##' are equal to x. If x is a vector, check that its length is d.
##'
##' @param x A single value or a vector
##' @param d An integer, the desired size of the vector
##' @return A vector of length d
##' @author Paul E. Johnson <pauljohn@@ku.edu>
makeVec <- function(x = NULL, d = NULL){
    if (length(x) == 1) {
        x <- rep(x, d) #assign same for all
    } else if (length(x) != d){
        msg <- paste(deparse(substitute(x)), "has too", ifelse(length(x) > d, "many","few"), "elements. Please specify", d, "elements.  Or just specify 1, we will use that one value to manufacture a vector for you.")
        stop(msg)
    }
    x
}
NULL

##' Create Symmetric Matrices, possibly covariance or correlation matrices, or check a matrix for symmetry and serviceability.
##'
##' Check X and do the right thing. If X is a matrix, check that it is
##' a valid for the intended purpose (symmetric or correlation or
##' covariance).  If X a single value, use that to fill up a
##' matrix. If it is a vector, try to use it as a vech to fill the
##' lower triangle. If d is supplied as an integer, use that as desired size.
##' @param X A single value, a vector (a vech), or a matrix
##' @param d Optional. An integer, the desired number of rows (or columns). Don't specify this argument if X is already a matrix.  Only required if X is an integer and diag is not supplied. Otherwise, the function tries to deduce desired size of output from X (as a vech) and diag.
##' @param diag Values for the diagonal. This is important because it alters the way X is interpreted.  If diag is not provided, then X is understood to include diagonal elements.
##' @param corr TRUE or FALSE: Should we construct a correlation matrix
##' @param cov TRUE or FALSE: Should this be a covariance matrix?
##' @return A d x d matrix
##' @export
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @examples
##' makeSymmetric(X = 3, d = 4)
##' makeSymmetric(X = 3, d = 4, diag = c(99, 98, 97, 96))
##' makeSymmetric(c(1,2,3))
##' makeSymmetric(c(1,2,3), d = 5)
##' makeSymmetric(c(0.8,0.4, 0.2), cov = TRUE)
##' makeSymmetric(c(0.8,0.4, 0.2), cov = TRUE, diag = c(44, 55, 66))
makeSymmetric <- function(X, d = NULL, diag = NULL, corr = FALSE, cov = FALSE) {
    if (is.matrix(X)) {
        dims <- dim(X)
        if (dims[1] != dims[2]) stop("X not square")
        stopifnot (isSymmetric(X))
        if (corr | cov) stopifnot(checkPosDef(X))
    } else if (length(X) == 1) {
        if (is.null(d) & is.null(diag)) stop("One of diag or d is required if X is singleton")
        if (corr & (X < -1 | X > 1)) stop(paste("The value of of a correlation should be in [-1,1]"))
        X <- matrix(X, nrow = d, ncol = d)
        if (corr == TRUE) diag(X) <- 1.0
        if (!is.null(diag)) diag(X) <- diag
    } else if (is.vector(X)) {
        if (corr == TRUE) {
            if(!is.null(diag)) stop("If you want a correlation matrix, just provide X as a strictly lower triangle")
            X <- vech2Corr(X)
            if(!is.null(d)) {
                if (dim(X)[1] != d) stop("d parameter inconsisent with size implied by X as a strictly lower triangular vech")
            }
        } else if (cov == TRUE) {
            X <- vech2mat(X, diag)
            stopifnot(checkPosDef(X))
        } else {
            X <- vech2mat(X, diag)
        }
    }  else {
        stop(paste("makeSquare cannot understand the value supplied for argument", deparse(substitute(X))))
    }
    X
}


##' Check a matrix for positive definitness
##'
##' Uses eigen to check positive definiteness. Follows example used
##' in \code{MASS} package by W. N. Venables and Brian D. Ripley
##'
##' @param X A matrix
##' @param tol Tolerance (closeness to 0 required to declare failure)
##' @return TRUE or FALSE
##' @author Paul E. Johnson <pauljohn@@ku.edu>
checkPosDef <- function(X, tol = 1e-6){
    evalues <- eigen(X, only.values = TRUE)$values
    res <- if(!all(evalues >= -tol*abs(evalues[1L]))) FALSE else TRUE
    res
}
