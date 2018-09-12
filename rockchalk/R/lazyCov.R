
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
lazyCov <-
    function(Rho, Sd, d)
{
    if (missing(Sd)){
        MESSG <- paste("lazyCov requires user to specify either a vector",
                       "or a single common value for all standard deviations")
        stop(MESSG)
    }
    if (missing(Rho)){
        MESSG <- paste("lazyCov requires a symmstric correlation matrix",
                       "or enough information to create one, either a vech",
                       "of lower triangular values or a single common",
                       "correlation value")
        stop(MESSG)
    }
    if (!missing(d) && (length(Sd) > 1) && (length(Sd) != d)){
        MESSG <- paste("lazyCov doesn't require a d argument,",
                       "but if you provide one, it must be consistent",
                       "with the length of a supplied Sd vector")
        stop(MESSG)
    }
    if (missing(d)){
        if (length(Sd) > 1) d <- length(Sd)
        else if (is.matrix(Rho)) d <- NROW(Rho)
        else if (is.vector(Rho)) {
            d <- (sqrt(1 + 8 * length(Rho)) + 1)/2
            if (!isTRUE(all.equal(as.integer(d)- d, 0))){
                MESSG <- paste(deparse(substitute(vech)),
                               " must have the correct number of elelemnts",
                               "to fill in a strictly lower triangle in a",
                               "square matrix.")
                stop(MESSG)
            }
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

NULL


    
