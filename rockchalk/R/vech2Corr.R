
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
##' @seealso Similar functions exist in many packages, see
##'     \code{vec2sm} in corpcor, \code{xpnd} in MCMCpack
##' @param vech A vector of values for the strictly lower triangle of
##'     a matrix. All values must be in the [0,1] interval (because
##'     they are correlations) and the matrix formed must be positive
##'     definite.
##' @return A symmetric correlation matrix, with 1's on the diagonal.
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @examples
##' v <- c(0.1, 0.4, -0.5)
##' vech2Corr(v)
##' v <- c(0.1, 0.4, -0.4, 0.4, 0.5, 0.1)
##' vech2Corr(v)
vech2Corr <-
    function(vech)
{
    ##compute number of rows from vech. diag not in the vech!
    n <- (sqrt(1 + 8 * length(vech)) + 1)/2
    if (!as.integer(n) == n) stop(deparse(substitute(vech)),
                                  " must have the correct number of elements to fill",
                                  "in a strictly lower triangle in a square matrix.")
    if(any(vech > 1 | vech < -1)) {
        stop(paste("All values in ", deparse(substitute(vech)),
                   " must be in the interval [-1,1]"))
    }
    X <- matrix(NA, nrow = n, ncol = n)
    X[lower.tri(X, diag = FALSE)] <- vech
    X[upper.tri(X)] <- t(X)[upper.tri(X)]
    diag(X) <- 1
    stopifnot(checkPosDef(X))
    X
}
NULL
