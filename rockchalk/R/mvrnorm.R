
##' Check a matrix for positive definitness
##'
##' Uses eigen to check positive definiteness. Follows example used
##' in \code{MASS} package by W. N. Venables and Brian D. Ripley
##'
##' @param X A matrix
##' @param tol Tolerance (closeness to 0 required to declare failure)
##' @return TRUE or FALSE
##' @author Paul E. Johnson <pauljohn@@ku.edu>
checkPosDef <-
    function(X, tol = 1e-6)
{
    evalues <- eigen(X, only.values = TRUE)$values
    res <- if(!all(evalues >= -tol*abs(evalues[1L]))) FALSE else TRUE
    res
}
 
NULL



##' Minor revision of mvrnorm (from \code{MASS}) to facilitate replication
##'
##' This is the \code{\link[MASS]{mvrnorm}} function from the MASS
##' package (Venables and Ripley, 2002), with one small modification
##' to facilitate replication of random samples. The aim is to make
##' sure that, after the seed is reset, the first rows of generated
##' data are identical no matter what value is chosen for n.  The one
##' can draw 100 observations, reset the seed, and then draw 110
##' observations, and the first 100 will match exactly. This is done
##' to prevent unexpected and peculiar patterns that are observed
##' when n is altered with MASS package's mvrnorm.
##'
##' To assure replication, only a very small change is made. The code
##' in \code{MASS::mvrnorm} draws a random sample and fills a matrix
##' by column, and that matrix is then decomposed.  The change
##' implemented here fills that matrix by row and the problem is
##' eliminated.
##'
##' Some peculiarities are noticed when the covariance matrix changes
##' from a diagonal matrix to a more general symmetric matrix
##' (non-zero elements off-diagonal).  When the covariance is strictly
##' diagonal, then just one column of the simulated multivariate
##' normal data will be replicated, but the others are not. This has
##' very troublesome implications for simulations that draw samples of
##' various sizes and then base calculations on the separate simulated
##' columns (i.e., some columns are identical, others are completely
##' uncorrelated).
##'
##' @seealso For an alternative multivariate normal generator
##' function, one which has had this fix applied to it,
##' consider using the new versions of \code{\link[mvtnorm]{rmvnorm}} in the
##' package \code{mvtnorm}.
##' @param n the number of samples ("rows" of data) required.
##' @param mu a vector giving the means of the variables.
##' @param Sigma positive-definite symmetric matrix specifying the
##'    covariance matrix of the variables.
##' @param tol tolerance (relative to largest variance) for numerical lack
##'    of positive-definiteness in \code{Sigma}
##' @param empirical logical. If true, mu and Sigma specify the empirical
##'    not population mean and covariance matrix.
##' @import MASS
##' @export
##' @return If \code{n = 1} a vector of the same length as \code{mu}, otherwise an
##'  \code{n} by \code{length(mu)} matrix with one sample in each row.
##' @author Ripley, B.D. with revision by Paul E. Johnson
##' @references
##' Venables, W. N. & Ripley, B. D. (2002) Modern Applied Statistics with
##' S. Fourth Edition. Springer, New York. ISBN 0-387-95457-0
##' @examples
##' library(MASS)
##' library(rockchalk)
##'
##' set.seed(12345)
##' X0 <- MASS::mvrnorm(n=10, mu = c(0,0,0), Sigma = diag(3))
##' ## create a smaller data set, starting at same position
##' set.seed(12345)
##' X1 <- MASS::mvrnorm(n=5, mu = c(0,0,0), Sigma = diag(3))
##' ## Create a larger data set
##' set.seed(12345)
##' X2 <- MASS::mvrnorm(n=15, mu = c(0,0,0), Sigma = diag(3))
##' ## The first 5 rows in X0, X1, and X2 are not the same
##' X0
##' X1
##' X2
##' set.seed(12345)
##' Y0 <- mvrnorm(n=10, mu = c(0,0,0), Sigma = diag(3))
##' set.seed(12345)
##' Y1 <- mvrnorm(n=5, mu = c(0,0,0), Sigma = diag(3))
##' set.seed(12345)
##' Y2 <- mvrnorm(n=15, mu = c(0,0,0), Sigma = diag(3))
##' # note results are the same in the first 5 rows:
##' Y0
##' Y1
##' Y2
##' identical(Y0[1:5, ], Y1[1:5, ])
##' identical(Y1[1:5, ], Y2[1:5, ])
##'
##' myR <- lazyCor(X = 0.3, d = 5)
##' mySD <- c(0.5, 0.5, 0.5, 1.5, 1.5)
##' myCov <- lazyCov(Rho = myR, Sd = mySD)
##'
##' set.seed(12345)
##' X0 <- MASS::mvrnorm(n=10, mu = rep(0, 5), Sigma = myCov)
##' ## create a smaller data set, starting at same position
##' set.seed(12345)
##' X1 <- MASS::mvrnorm(n=5, mu = rep(0, 5), Sigma = myCov)
##' X0
##' X1
##' ##' set.seed(12345)
##' Y0 <- rockchalk::mvrnorm(n=10, mu = rep(0, 5), Sigma = myCov)
##' ## create a smaller data set, starting at same position
##' set.seed(12345)
##' Y1 <- rockchalk::mvrnorm(n=5, mu = rep(0, 5), Sigma = myCov)
##' Y0
##' Y1
##'
mvrnorm <-
    function(n = 1, mu, Sigma, tol=1e-6, empirical = FALSE)
{
    p <- length(mu)
    if(!all(dim(Sigma) == c(p,p))) stop("incompatible arguments")
    eS <- eigen(Sigma, symmetric = TRUE)
    ev <- eS$values
    if(!all(ev >= -tol*abs(ev[1L]))) stop("'Sigma' is not positive definite")
    X <- matrix(rnorm(p * n), n, byrow = TRUE)
    if(empirical) {
        X <- scale(X, TRUE, FALSE) # remove means
        X <- X %*% svd(X, nu = 0)$v # rotate to PCs
        X <- scale(X, FALSE, TRUE) # rescale PCs to unit variance
    }
    X <- drop(mu) + eS$vectors %*% diag(sqrt(pmax(ev, 0)), p) %*% t(X)
    nm <- names(mu)
    if(is.null(nm) && !is.null(dn <- dimnames(Sigma))) nm <- dn[[1L]]
    dimnames(X) <- list(nm, NULL)
    if(n == 1)
        drop(X)
    else t(X)
}
