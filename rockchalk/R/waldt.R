##' T-test for the difference in 2 regression parameters
##'
##' This is the one the students call the "fancy t test". It is just
##' the simplest, most easy to use version of the t test to decide if
##' 2 coefficients are equal. It is not as general as other functions
##' in other packages. This is simpler to use for beginners.  The
##' \code{car} package's function \code{linearHypothesis} is more
##' general, but its documentation is much more difficult to understand.
##' It gives statistically identical results, albeit phrased as an F
##' test.
##'
##' I did this because we have trouble understanding terminology in
##' documentation for more abstract functions in other R
##' packages.
##' 
##' It has an additional feature, it can import robust standard errors
##' to conduct the test.
##' 
##' @param parm1 A parameter name, in quotes!
##' @param parm2 Another parameter name, in quotes!
##' @param model A fitted regression model
##' @param model.cov Optional, another covariance matrix to use while
##'     calculating the test. Primarily used for robust (or otherwise
##'     adjusted) standard errors
##' @param digits How many digits to print? This affects only the on-screen
##'     printout. The return object is numeric, full precision.
##' @return A vector with the difference, std. err., t-stat, and p
##'     value. Prints a formatted output statement.
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @export
##' @examples
##' mdat <- data.frame(x1 = rnorm(100), x2 = rnorm(100))
##' stde <- 2
##' mdat$y <- 0.2 * mdat$x1 + 0.24 * mdat$x2 + stde * rnorm(100)
##' m1 <- lm(y ~ x1 + x2, data = mdat)
##' waldt("x1", "x2", m1)
##' waldt("x1", "x2", m1, digits = 2)
##' ## Returned object is not "rounded characters". It is still numbers
##' stillnumeric <-  waldt("x1", "x2", m1, digits = 2)
##' stillnumeric
##' ## Equivalent to car package linearHypothesis:
##' if(require(car)){
##'     linearHypothesis(m1, "x1 = x2")
##' }
##' ## recall t = sqrt(F) for a 1 degree of freedom test.
##' ## If we could understand instructions for car, we probably
##' ## would not need this function, actually.
waldt <- function(parm1, parm2, model, model.cov = NULL, digits = getOption("digits")){
    V <- function(mat, parm1, parm2 = NULL) {
        if(is.null(parm2)) return (mat[parm1, parm1])
        else return(mat[parm1, parm2])
    }
    if(is.null(model.cov)) model.cov <- vcov(model)
    ## following does not succeed with lme4 object
    ## model.coef <- coef(model)[c(parm1, parm2)]
    model.coef <- coef(summary(model))[c(parm1, parm2), "Estimate"]
    se <- sqrt(V(model.cov, parm1) + V(model.cov, parm2) - 2*V(model.cov, parm1, parm2))
    diff <- model.coef %*% c(1, -1)
    t <-  diff/se
    df <- df.residual(model)
    pval <- pt(abs(t), lower.tail = FALSE, df = df) * 2
    reslt <- c(diff = diff, Std.Err. = se, t = t, p = pval)
    print(formatC(reslt, digits = digits))
    invisible(reslt)
}

NULL
