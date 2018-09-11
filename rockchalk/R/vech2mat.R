##' Convert a half-vector (vech) into a matrix.
##'
##' Fills a matrix from a vector that represents the lower triangle.
##' If user does not supply a value for diag, then the vech will fill
##' in the diagonal as well as the strictly lower triangle.  If diag
##' is provided (either a number or a vector), then vech is for the
##' strictly lower triangular part.  The default value for lowerOnly
##' is FALSE, which means that a symmetric matrix will be created. See
##' examples for a demonstration of how to fill in the lower triangle
##' and leave the diagonal and the upper triangle empty.
##'
##' @param vech A vector
##' @param diag Optional. A single value or a vector for the
##' diagonal. A vech is a strictly lower triangluar vech, it
##' does not include diagonal values. diag can be either a single
##' value (to replace all elements along the diagonal) or a vector of
##' the correct length to replace the diagonal.
##' @param lowerOnly Default = FALSE. 
##' @seealso Similar functions exist in many packages, see
##' \code{vec2sm} in corpcor, \code{xpnd} in MCMCpack
##' @export
##' @examples
##' x <- 1:6
##' vech2mat(x)
##' vech2mat(x, diag = 7)
##' vech2mat(x, diag = c(99, 98, 97, 96))
##' vech2mat(x, diag = 0, lowerOnly = TRUE)
vech2mat <-
    function(vech, diag = NULL, lowerOnly = FALSE)
{
    ## Must calculate correct number of rows from vech, if
    ## vech implies a non-square, stop.
    ## If no diag, then vech provides the diagonal values
    if (!is.null(diag)){
        d <- (sqrt(1 + 8 * length(vech)) + 1)/2
        if (!as.integer(d) == d)
            stop(deparse(substitute(vech)), " must have the correct number of elements to fill a stricly lower triangle.")
        X <- matrix(0, nrow = d, ncol = d)
        X[lower.tri(X, diag = FALSE)] <- vech
        diag(X) <- makeVec(diag, d)
        if (!lowerOnly) X[upper.tri(X)] <- t(X)[upper.tri(X)]
    } else {
        d <- (sqrt(1 + 8 * length(vech)) - 1)/2
        if (!as.integer(d) == d)
            stop(paste("You supplied diag. So ", deparse(substitute(vech)), " must have the correct number of elements to fill in a lower triangle, including the diagonal.."))
        X <- matrix(0, nrow = d, ncol = d)
        X[lower.tri(X, diag = TRUE)] <- vech
        if (!lowerOnly) X[upper.tri(X)] <- t(X)[upper.tri(X)]
    }
    X
}
NULL
