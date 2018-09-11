
##' Create Symmetric Matrices, possibly covariance or correlation
##' matrices, or check a matrix for symmetry and serviceability.
##'
##' Check X and do the right thing. If X is a matrix, check that it is
##' a valid for the intended purpose (symmetric or correlation or
##' covariance).  If X a single value, use that to fill up a
##' matrix. If it is a vector, try to use it as a vech to fill the
##' lower triangle. If d is supplied as an integer, use that as desired size.
##'
##' @param X A single value, a vector (a vech), or a matrix
##' @param d Optional. An integer, the desired number of rows (or
##'     columns). Don't specify this argument if X is already a
##'     matrix.  Only required if X is an integer and diag is not
##'     supplied. Otherwise, the function tries to deduce desired size
##'     of output from X (as a vech) and diag.
##' @param diag Values for the diagonal. This is important because it
##'     alters the way X is interpreted.  If diag is not provided,
##'     then X is understood to include diagonal elements.
##' @param corr TRUE or FALSE: Should we construct a correlation
##'     matrix
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
##'
makeSymmetric <-
    function(X, d = NULL, diag = NULL, corr = FALSE, cov = FALSE)
{
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
NULL
