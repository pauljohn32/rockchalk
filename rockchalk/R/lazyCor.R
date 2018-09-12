
##' Create correlation matrices.
##'
##' Use can supply either a single value (the common correlation among
##' all variables), a column of the lower triangular values for a
##' correlation matrix, or a candidate matrix. The function will check
##' X and do the right thing. If X is a matrix, check that it
##' is a valid correlation matrix. If its a single value, use that
##' to fill up a matrix. If itis a vector, try to use it as a vech
##' to fill the lower triangle..
##'
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
lazyCor <-
    function(X, d)
{
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

