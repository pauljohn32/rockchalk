##' Generates a data frame for regression analysis
##'
##' The output is a data frame (x1, x2, y) with user-specified
##' correlation between x1 and x2. The y (output) variable is created
##' according to the equation\cr
##' \deqn{
##' y = beta1 + beta2 * x1 + beta3 * x2 + beta4 * x1 * x2 + e.
##' }
##' The arguments determine the scales of the X matrix, the random
##' error, and the slope coefficients.
##'
##' The vector (x1,x2) is drawn from a multivariate normal
##' distribution in which the expected value (argument \code{means}).
##' The covariance matrix of X is
##' built from the standard deviations (\code{sds})
##' and the specified correlation between x1 and x2 (\code{rho}).
##' It is also necessary to specify the standard deviation
##' of the error term (\code{stde}) and the coefficients
##' of the regression equation (\code{beta}).
##'
##' @param N Number of cases desired
##' @param means 2-vector of means for x1 and x2
##' @param sds 2-vector of standard deviations for x1 and x2
##' @param rho Correlation coefficient for x1 and x2
##' @param stde standard deviation of the error term in the data
##' generating equation
##' @param beta beta vector of at most 4 coefficients for intercept,
##' slopes, and interaction
##' @export genCorrelatedData
##' @importFrom stats rnorm
##' @examples
##' ## 1000 observations of uncorrelated x1 and x2 with no
##' ## interaction between x1 and x2
##' dat <- genCorrelatedData(N=1000, rho=0, beta=c(1, 1.0, -1.1, 0.0))
##'   mcGraph1(dat$x1, dat$x2, dat$y, theta=20, phi=8,
##'   ticktype="detailed", nticks=10)
##' m1 <- lm(y ~ x1 + x2, data = dat)
##' plotPlane(m1, plotx1 = "x1", plotx2 = "x2")
##'
genCorrelatedData <-
    function(N = 100, means = c(50,50), sds = c(10,10), rho = 0.0, stde = 1,
             beta = c(0, 0.2, 0.2, 0.0))
{
  if (length(beta)> 4) stop("beta vector can have at most 4 values")
  corr.mat <- matrix(c(1,rho,rho,1), nrow = 2)
  sigma <- diag(sds) %*% corr.mat %*% diag(sds)
  x.mat <-  rockchalk::mvrnorm(n = N, mu = means, Sigma = sigma)
  y = beta[1] + beta[2] * x.mat[,1] + beta[3] * x.mat[,2] + beta[4] * x.mat[,1] * x.mat[ ,2] +  stde*rnorm (N, mean = 0, sd = 1)
  dat <- data.frame(x.mat, y)
  names(dat) <- c("x1", "x2", "y")
  dat
}
NULL

##' Generates a data frame for regression analysis.
##'
##' Unlike \code{genCorrelatedData}, this new-and-improved
##' function can generate a data frame with as many predictors
##' as the user requests along with an arbitrarily complicated
##' regression formula.  The result will be a data frame with
##' columns named (y, x1, x2, ..., xp).
##'
##' Arguments supplied must have enough information so that an
##' N x P matrix of predictors can be constructed.
##' The matrix X is drawn from a multivariate normal
##' distribution, the expected value vector (mu vector) is given by
##' the \code{means} and the var/covar matrix (Sigma) is
##' built from user supplied standard deviations \code{sds}
##' and the correlations between the columns of X, given by \code{rho}.
##' The user can also set the standard deviation
##' of the error term (\code{stde}) and the coefficients
##' of the regression equation (\code{beta}).
##'
##' If called with no arguments, this creates a data frame with
##' X ~ MVN(mu = c(50,50,50), Sigma = diag(c(10,10,10))).
##' y = X %*% c(0.15, 0.1, -0.1) + error, where error
##' is N(m = 0, sd = 200). All of these details can be
##' changed by altering the arguments.
##'
##' The y (output) variable is created according to the
##' equation\cr
##' \deqn{
##' y = b1 + b2 * x1 + ...+ bk * xk + b[k+1] * x1 * ...interactions.. + e}
##' \cr
##' For shorthand, I write b1 for beta[1], b2 for beta[2], and so forth.\cr
##'
##' The first P+1 arguments in the argument beta are the coefficients
##' for the intercept and the columns of the X matrix.  Any additional
##' elements in beta are the coefficients for nonlinear and interaction terms.\cr
##'
##' Those additional values in the beta vector are completely
##' optional. Without them, the true model is a linear
##' regression. However, one can incorporate the effect of squared terms
##' (conceptualize that as x1 * x1, for example) or interactions
##' (x1 * x2) easily.  This is easier to illustrate than describe.
##' Suppose there are 4 columns in X. Then a beta
##' vector like beta = c(0, 1, 2, 3, 4, 5, 6, 7, 8) would amount to
##' asking for\cr
##' \deqn{
##' y = 0 + 1 x1 + 2 x2 + 3 x3 + 4 x4 + 5 x1^2 + 6 x1 x2 + 7 x1 x3 + 8 x1 x4 + error
##' }
##' \cr
##' If beta supplies more coefficients, they are interpeted as additional
##' interactions.\cr
##'
##' When there are a many predictors and the beta vector is long, this
##' can become confusing. I think of this as a vech for the lower
##' triangle of a coefficient matrix. In the example with 4
##' predictors, beta[1:5] are used for the intercepts and slopes. The
##' rest of the  beta elements lay in like so:\cr
##'
##'    X1   X2  X3  X4\cr
##' X1 b6   .    .\cr
##' X2 b7   b10  .\cr
##' X3 b8   b11  b13\cr
##' X4 b9   b12  b14 b15\cr
##'
##' If the user only supplies b6 and b7, the rest are assumed  to  be 0.\cr
##'
##' To make this clear, the formula used to calculate y is printed to
##' the console when genCorrelatedData2 is called.
##'
##' @param N Number of cases desired
##' @param means P-vector of means for X. Implicitly sets the dimension of
##' the predictor matrix as N x P.
##' @param sds Values for standard deviations for columns of X. If
##' less than P values are supplied, they will be recycled.
##' @param rho Correlation coefficient for X. Several input formats
##' are allowed (see \code{lazyCor}). This can be a single number (common
##' correlation among all variables), a full matrix of correlations
##' among all variables, or a vector that is interpreted as the
##' strictly lower triangle (a vech).
##' @param stde standard deviation of the error term in the data
##' generating equation
##' @param beta beta vector of coefficients for intercept, slopes, and
##' interaction terma.  The first P+1 values are the
##' intercept and slope coefficients for the predictors. Additional
##' elements in beta are interpreted as coefficients for nonlinear and
##' interaction coefficients.  I have decided to treat these as a
##' column (vech) that fills into a lower triangular matrix. It
##' is easy to see what's going on if you run the examples. There
##' is also explanation in Details.
##' @param intercept Default FALSE. Should the predictors include an
##' intercept?
##' @param verbose TRUE or FALSE. Should information about the data
##' generation be reported to the terminal?
##' @return A data matrix that has columns c(y, x1, x2, ..., xP)
##' @export
##' @examples
##' ## 1000 observations of uncorrelated X with no interactions
##' set.seed(234234)
##' dat <- genCorrelatedData2(N = 10, rho = 0.0, beta = c(1, 2, 1, 1),
##'     means = c(0,0,0), sds = c(1,1,1), stde = 0)
##' summarize(dat)
##' ## The perfect regression!
##' m1 <- lm(y ~ x1 + x2 + x3, data = dat)
##' summary(m1)
##'
##' dat <- genCorrelatedData2(N = 1000, rho = 0,
##'     beta = c(1, 0.2, -3.3, 1.1), stde = 50)
##' m1 <- lm(y ~ x1 + x2 + x3, data = dat)
##' summary(m1)
##' predictOMatic(m1)
##' plotCurves(m1, plotx = "x2")
##'
##' ## interaction between x1 and x2
##' dat <- genCorrelatedData2(N = 1000, rho = 0.2,
##'     beta = c(1, 1.0, -1.1, 0.1, 0.0, 0.16), stde = 1)
##' summarize(dat)
##' ## Fit wrong model? get "wrong" result
##' m2w <- lm(y ~ x1 + x2 + x3, data = dat)
##' summary(m2w)
##' ## Include interaction
##' m2 <- lm(y ~ x1 * x2 + x3, data = dat)
##' summary(m2)
##'
##' dat <- genCorrelatedData2(N = 1000, rho = 0.2,
##'     beta = c(1, 1.0, -1.1, 0.1, 0.0, 0.16), stde = 100)
##' summarize(dat)
##' m2.2 <- lm(y ~ x1 * x2 + x3, data = dat)
##' summary(m2.2)
##'
##' dat <- genCorrelatedData2(N = 1000, means = c(100, 200, 300, 100),
##'     sds = 20,  rho = c(0.2, 0.3, 0.1, 0, 0, 0),
##'     beta = c(1, 1.0, -1.1, 0.1, 0.0, 0.16, 0, 0, 0.2, 0, 0, 1.1, 0, 0, 0.1),
##'     stde = 200)
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
##'     dat <- genCorrelatedData2(N = 1000, rho = c(0.2),
##'         beta = c(1, 1.0, -1.1, 0.1, 0.0, 0.46), verbose = FALSE)
##'     mymod <- lm (y ~ x1 * x2 + x3, data = dat)
##'     summary(mymod)
##' })
##'
##' x3est <- sapply(simReg, function(reg) {coef(reg)[4 ,1] })
##' summarize(x3est)
##' hist(x3est,  breaks = 40, prob = TRUE,
##'     xlab = "Estimated Coefficients for column x3")
##'
##' r2est <- sapply(simReg, function(reg) {reg$r.square})
##' summarize(r2est)
##' hist(r2est,  breaks = 40, prob = TRUE, xlab = "Estimates of R-square")
##'
##' ## No interaction, collinearity
##' dat <- genCorrelatedData2(N = 1000, rho = c(0.1, 0.2, 0.7),
##'     beta = c(1, 1.0, -1.1, 0.1), stde = 1)
##' m3 <- lm(y ~ x1 + x2 + x3, data = dat)
##' summary(m3)
##'
##' dat <- genCorrelatedData2(N=1000, rho=c(0.1, 0.2, 0.7),
##'     beta = c(1, 1.0, -1.1, 0.1), stde = 200)
##' m3 <- lm(y ~ x1 + x2 + x3, data = dat)
##' summary(m3)
##' mcDiagnose(m3)
##'
##' dat <- genCorrelatedData2(N = 1000, rho = c(0.9, 0.5, 0.4),
##'     beta = c(1, 1.0, -1.1, 0.1), stde = 200)
##' m3b <- lm(y ~ x1 + x2 + x3, data = dat)
##' summary(m3b)
##' mcDiagnose(m3b)
##'
genCorrelatedData2 <-
    function(N = 100, means = c(50,50,50), sds = c(10,10,10),
             rho = c(0.0, 0.0, 0.0), stde = 100,
             beta = c(0, 0.15, 0.1, -0.1), intercept = FALSE, 
             verbose = TRUE)
{
    d <- length(means)
    x.mat <- as.matrix(genX(N, means, sds, rho, intercept = TRUE))
    ## Get rid of Intercept if it is in there
    x.mat.noint <- x.mat[ , -grep("Intercept", colnames(x.mat))]
    
    beta1 <- beta[1:(d+1)]
    beta2 <- beta[-(1:(d+1))]

    ## pad beta2 with 0's on back end so it is right size to go into
    ## triangular matrix
    beta2 <- c(beta2, rep(0, times =(d*(d+1)/2) - length(beta2)))

    Bmat2 <- matrix(0, nrow = d, ncol = d)
    Bmat2[lower.tri(Bmat2, diag = TRUE)] <- beta2

    ##Would have been easier if I studied sparse matrix entry and usage.
    ## TODO 2013-05-16: This does a lot of calculations on zeroes.
    ## May enhance effiency to select columns before multiplying.
    intEffects <-  sapply(1:d, function(j) {
        if(!any( Bmat2[ ,j] != 0)) {
            return()
        }
        intEff <-  (x.mat.noint * x.mat.noint[ , j]) %*% Bmat2[ , j]
    })

    intEffects <- do.call("cbind", intEffects)

    y = x.mat %*% beta1 +  stde*rnorm (N, mean = 0, sd = 1)

    if (!is.null(intEffects)) y = y + rowSums(intEffects)

    dat <- if (!intercept){
               data.frame(y, x.mat.noint)
           } else {
               data.frame(y, x.mat)
           }
    
    if (verbose == TRUE){
        x.names <- colnames(x.mat.noint)  
        print("The equation that was calculated was")
        cat("y =",  beta1[1], "+", paste(beta1[2:(d+1)] , c(x.names), collapse = " + ", sep = "*"), "\n" ,
            "+ ")
        for(i in seq(d)){
            cat(paste(Bmat2[, i], x.names, x.names[i], collapse = " + ", sep = "*"), "\n",
                "+ ")
        }
        cat(paste("N(0,", stde, ") random error \n", sep = ""))
    }

    dat
}
NULL

##' Generate correlated data (predictors) for one unit
##'
##' This is used to generate data for one unit. It is recently
##' re-designed to serve as a building block in a multi-level data
##' simulation exercise.  The new arguments "unit" and "idx" can be
##' set as NULL to remove the multi-level unit and row naming
##' features.  This function uses the rockchalk::mvrnorm function, but
##' introduces a convenience layer by allowing users to supply
##' standard deviations and the correlation matrix rather than the
##' variance.
##'
##' Today I've decided to make the return object a data frame.  This
##' allows the possibility of including a character variable "unit"
##' within the result.  For multi-level models, that will help.  If
##' unit is not NULL, its value will be added as a column in the data
##' frame. If unit is not null, the rownames will be constructed by
##' pasting "unit" name and idx. If unit is not null, then idx will
##' be included as another column, unless the user explicitly sets
##' idx = FALSE. 
##'
##' @param N Number of cases desired
##' @param means A vector of means for p variables. It is optional to
##'     name them.  This implicitly sets the dimension of the
##'     predictor matrix as N x p. If no names are supplied, the
##'     automatic variable names will be "x1", "x2", and so forth. If
##'     means is named, such as c("myx1" = 7, "myx2" = 13, "myx3" =
##'     44), those names will be come column names in the output
##'     matrix.
##' @param sds Standard deviations for the variables.  If less than p
##'     values are supplied, they will be recycled.
##' @param rho Correlation coefficient for p variables. Several input
##'     formats are allowed (see \code{lazyCor}). This can be a single
##'     number (common correlation among all variables), a full matrix
##'     of correlations among all variables, or a vector that is
##'     interpreted as the strictly lower triangle (a vech).
##' @param Sigma P x P variance/covariance matrix.
##' @param intercept Default = TRUE, do you want a first column filled
##'     with 1?
##' @param col.names Names supplied here will override column names
##'     supplied with the means parameter. If no names are supplied
##'     with means, or here, we will name variables x1, x2, x3,
##'     ... xp, with Intercept at front of list if intercept =
##'     TRUE.
##' @param unit A character string for the name of the unit being
##'     simulated. Might be referred to as a "group" or "district" or
##'     "level 2" membership indicator.
##' @param idx If set TRUE, a column "idx" is added, numbering the
##'     rows from 1:N. If the argument unit is not NULL, then idx is
##'     set to TRUE, but that behavior can be overridded by
##'     setting idx = FALSE.
##' @export
##' @return A data frame with rownames to specify unit and
##'     individual values, including an attribute "unit" with the
##'     unit's name.
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @examples
##' X1 <- genX(10, means = c(7, 8), sds = 3, rho = .4)
##' X2 <- genX(10, means = c(7, 8), sds = 3, rho = .4, unit = "Kansas")
##' head(X2)
##' X3 <- genX(10, means = c(7, 8), sds = 3, rho = .4, idx = FALSE, unit = "Iowa")
##' head(X3)
##' X4 <- genX(10, means = c("A" = 7, "B" = 8), sds = c(3), rho = .4)
##' head(X4)
##' X5 <- genX(10, means = c(7, 3, 7, 5), sds = c(3, 6),
##'             rho = .5, col.names = c("Fred", "Sally", "Henry", "Barbi"))
##' head(X5)
##' Sigma <- lazyCov(Rho = c(.2, .3, .4, .5, .2, .1), Sd = c(2, 3, 1, 4))
##' X6 <- genX(10, means = c(5, 2, -19, 33), Sigma = Sigma, unit = "Winslow_AZ")
##' head(X6)
##'
genX <-
    function(N, means, sds, rho, Sigma = NULL, intercept = TRUE,
             col.names = NULL, unit = NULL, idx = FALSE)
{
    d <- length(means)
    if (!missing(rho) & !is.null(Sigma))
        stop ("Please provide rho or Sigma, not both")
    if (!is.null(Sigma) & !missing(sds))
        warning(paste("Note, if you provide Sigma we are ignoring",
                      " your input on the sds argument"))
    if ((!is.null(Sigma)) && (d != dim(Sigma)[1]))
        stop (paste("Sigma must have same number of rows",
                    "& columns as there are means"))
    if (is.null(Sigma)){
        R <- lazyCor(rho, d)
        if (length(sds) < d) sds <- rep(sds, length.out = d)
        Sigma <- lazyCov(Rho = R, Sd = sds)
    }
    if (missing(col.names) && is.null(names(means))) {
        col.names <- paste0("x", 1:d)
    } else if (missing(col.names) && !is.null(names(means))){
        col.names <- names(means)
    } else {
        if (length(col.names) != d)
            stop(paste("If you provide column names, please provide one",
                       "for each column in the means input"))
    }
    col.names <- make.names(col.names, unique = TRUE)

    ## If unit is meaningful, set idx TRUE. Defending against
    ## user values like unit = NULL and idx = NULL
    if (missing(idx) || (!identical(idx, FALSE) && !is.null(idx))){ 
        if (!missing(unit) & !is.null(unit))
            idx <- TRUE
    }

    x.mat <- rockchalk::mvrnorm(N, means, Sigma)
    if (is.null(unit)) {
        rownames <- 1:N
    } else {
        if (length(unit) > 1) stop("genX: just supply 1 unit name")
        rownames <- paste0(unit, "_", 1:N)
    }
    dimnames(x.mat) <- list(rownames, col.names)
    x.mat <- as.data.frame(x.mat)
    if (intercept) x.mat <- cbind(Intercept = 1L, x.mat)
    
    if (!is.null(unit)) x.mat[ , "unit"] <- rep(unit, N)
    if (isTRUE(idx)) x.mat[ , "idx"] <- seq(1L, N, by = 1L)
    attr(x.mat, "unit") <- unit
    x.mat
}
NULL
    



##' Generate correlated data for simulations (third edition)
##'
##' This is a revision of \code{genCorrelatedData2}.  The output is a
##' data frame that has columns for the predictors along with an error
##' term, the linear predictor, and the observed value of the outcome
##' variable.  The new features are in the user interface. It has a
##' better way to specify beta coefficients. It is also more flexible
##' in the specification of the names of the predictor columns.
##'
##' The enhanced methods for authors to specify a data-generating
##' process are as follows. Either way will work and the choice
##' between the methods is driven by the author's convenience.\cr
##' \itemize{
##' \item 1. Use the formula argument as a quoted string:
##' \code{"1 + 2.2 * x1 + 2.3 * x2 + 3.3 * x3 + 1.9 * x1:x2"}.
##' The "*" represents multiplication of coefficient times variable,
##' and the colon ":" has same meaning but it is used for products of variables.
##' \item 2. Use the beta argument with parameter names, \code{beta =
##' c("Intercept" = 1, x1 = 2.2, x2 = 2.3, x3 = 3.3, "x1:x2" = 1.9)}
##' where the names are the same as the names of the variables in the
##' formula. Names of the variables in the formula or the beta vector
##' should be used also in either the means parameter or the col.names
##' parameter.
##' }
##' 
##' The error distribution can be specified. Default is normal, with
##' draws provided by R's \code{rnorm}. All error models assume
##' \eqn{E[e] = 0} and the scale coefficient is the parameter
##' \code{stde}. Thus, the default setup's error will be drawn from
##' \code{rnorm(N, 0, stde)}. Any two parameter "location" and "scale"
##' distribution should work as well, as long as the first coefficient
##' is location (because we set that as 0 in all cases) and the second
##' argument is scale. For example, \code{distrib=rlogis}, will lead
##' to errors drawn from \code{rlogis(N, 0, stde)}. Caution: in rlogis,
##' the scale parameter is not the same as standard deviation.
##'
##' The only one parameter distribution currently supported is the T
##' distribution.  If user specifies \code{distrib=rt}, then the
##' \code{stde} is passed through to the parameter \code{df}. Note
##' that if increasing the stde parameter will cause the standard
##' deviation of \code{rt} to get smaller. \code{df=1} implies sd =
##' 794.6; \code{df=2} implies sd = 3.27; \code{df=3} implies 1.7773.
##'
##' Methods to specify error distributions in a more flexible way need
##' to be considered.
##' 
##' @param formula a text variable, e.g., \code{"y ~ 1 + 2*x1"}. Use
##'     ":" to create squared and interaction terms,
##'     \code{"y ~ 1 + 2*x1 + 1.1*x1:x1 + 0.2*x1:x2".} Multi-way
##'     interactions are allowed, eg
##'     \code{"y ~ 1 + 2*x1 + .4*x2 + .1*x3 + 1.1*x1:x1 + 0.2*x1:x2:x3".}
##'     Note author can specify any order of interation.
##' @param N sample size
##' @param means averages of predictors, can include names c(x1 = 10,
##'     x2 = 20) that will be used in the data.frame result.
##' @param sds standard deviations, 1 (common value for all variables)
##'     or as many elements as in \code{means}.
##' @param rho correlations, can be 1 or a vech for a correlation
##'     matrix
##' @param stde The scale of the error term. If \code{distrib=rnorm},
##'     stde is the standard deviation of the error term. If the user
##'     changes the distribution, this is a scale parameter that may
##'     not be equal to the standard deviation. For example,
##'     \code{distrib=rlogist} has a scale parameter such that a value
##'     of stde implies the error's standard deviation will be
##'     \eqn{stde * pi / sqrt(3)}.
##' @param beta slope coefficients, use either this or \code{formula},
##'     not both. It is easier (less error prone) to use named
##'     coefficients, but (for backwards compatability with
##'     \code{genCorrelatedData2}) names are not required. If named,
##'     use "Intercept" for the intercept coefficient, and use
##'     variable names that match the \code{xmeans} vector.  Un-named
##'     coefficients follow same rules as in
##'     \code{\link{genCorrelatedData2}}. The first (1 + p) values are
##'     for the intercept and p main effects.  With 3 predictors and
##'     no squares or interactions, specify four betas corresponding
##'     to \code{c(Intercept, x1, x2, x3)}. The squared and
##'     interaction terms may follow.  The largest possible model
##'     would correspond to \code{c(Intercept, x1, x2, x3, x1:x1,
##'     x1:x2, x1:x3, x2:x2, x2:x3, x3:x3)}.  Squares and interactions
##'     fill in a "lower triangle". The unnamed beta vector can be
##'     terminated with the last non-zero coefficient, the function
##'     will insert 0's for the coefficients at the end of the vector.
##' @param intercept TRUE or FALSE. Should the output data set include
##'     a column of 1's. If beta is an unnamed vector, should the
##'     first element be treated as an intercept?
##' @param col.names Can override names in means vector
##' @param verbose TRUE for diagnostics
##' @param ... extra arguments, ignored for now. We use that to ignore
##'     unrecognized parameters.
##' @param distrib An R random data generating function.  Default is
##'     \code{rnorm}. Also \code{rlogis} or any other two-parameter
##'     location/scale distribution will work. Special configuration
##'     allows \code{rt}. See details.
##' @return a data frame
##' @name genCorrelatedData3
##' @export genCorrelatedData3
##' @importFrom stats rnorm
##' @importFrom stats rt
##' @importFrom stats rlogis
##' @author Paul Johnson <pauljohn@@ku.edu> and Gabor Grothendieck <ggrothendieck@gmail.com>
##' @examples
##' set.seed(123123)
##' ## note: x4 is an unused variable in formula
##' X1a <-
##'     genCorrelatedData3("y ~ 1.1 + 2.1 * x1 + 3 * x2 + 3.5 * x3 + 1.1 * x1:x3",
##'                        N = 1000, means = c(x1 = 1, x2 = -1, x3 = 3, x4 = 1),
##'                        sds = 1, rho = 0.4, stde = 5)
##' lm1a <- lm(y ~ x1 + x2 + x3 + x1:x3, data = X1a)
##' ## note that normal errors have std.error. close to 5
##' summary(lm1a)
##' attr(X1a, "beta") 
##' attr(X1a, "formula")
##' ## Demonstrate name beta vector method to provide named arguments
##' set.seed(123123)
##' X2 <- genCorrelatedData3(N = 1000, means = c(x1 = 1, x2 = -1, x3 = 3, x4 = 1),
##'           sds = 1, rho = 0.4, 
##'           beta = c("Intercept" = 1.1, x1 = 2.1, x2 = 3,
##'                     x3 = 3.5, "x1:x3" = 1.1),
##'                     intercept = TRUE, stde = 5)
##' attr(X2, c("beta"))
##' attr(X2, c("formula"))
##' head(X2)
##' lm2 <- lm(y ~ x1 + x2 + x3 + x1:x3, data = X2)
##' summary(lm2)
##'
##' ## Equivalent with unnamed beta vector. Must carefully count empty
##' ## spots, fill in 0's when coefficient is not present. This
##' ## method was in genCorrelated2. Order of coefficents is
##' ## c(intercept, x1, ..., xp, x1:x1, x1:x2, x1:xp, x2:x2, x2:x3, ..., )
##' ## filling in a lower triangle.
##' set.seed(123123)
##' X3 <- genCorrelatedData3(N = 1000, means = c(x1 = 1, x2 = -1, x3 = 3, x4 = 1),
##'           sds = 1, rho = 0.4, 
##'           beta = c(1.1, 2.1, 3, 3.5, 0, 0, 0, 1.1),
##'                     intercept = TRUE, stde = 5)
##' attr(X3, c("beta"))
##' attr(X3, c("formula"))
##' head(X3)
##' lm3 <- lm(y ~ x1 + x2 + x3 + x1:x3, data = X3)
##' summary(lm3)
##'
##' ## Same with more interesting variable names in the means vector
##' X3 <- genCorrelatedData3(N = 1000,
##'           means = c(friend = 1, enemy = -1, ally = 3, neutral = 1),
##'           sds = 1, rho = 0.4, 
##'           beta = c(1.1, 2.1, 3, 3.5, 0, 0, 0, 1.1),
##'                     intercept = TRUE, stde = 5)
##' head(X3)
##' attr(X3, c("beta"))
##' 
##' 
##' X3 <- genCorrelatedData3(N = 1000, means = c(x1 = 50, x2 = 50, x3 = 50),
##'           sds = 10, rho = 0.4,
##'           beta = c("Intercept" = .1, x1 = .01, x2 = .2, x3 = .5,
##'                    "x1:x3" = .1))
##' lm3 <- lm(y ~ x1 + x2 + x3 + x1:x3, data = X3)
##' 
##' 
##' ## Names via col.names argument: must match formula
##' X2 <- genCorrelatedData3("y ~ 1.1 + 2.1 * educ + 3 * hlth + 3 * ses + 1.1 * educ:ses",
##'          N = 100, means = c(50, 50, 50, 20),
##'          sds = 10, rho = 0.4, col.names = c("educ", "hlth", "ses", "wght"))
##' str(X2) 
##'
##' X3 <- genCorrelatedData3("y ~ 1.1 + 2.1 * educ + 3 * hlth + 3 * ses + 1.1 * educ:ses",
##'          N = 100, means = c(50, 50, 50, 20),
##'          sds = 10, rho = 0.4, col.names = c("educ", "hlth", "ses", "wght"),
##'          intercept = TRUE)
##' str(X3)
##' 
##' ## note the logistic errors have residual std.error approximately 5 * pi/sqrt(3)
##' X1b <-
##'     genCorrelatedData3("y ~ 1.1 + 2.1 * x1 + 3 * x2 + 3.5 * x3 + 1.1 * x1:x3",
##'                        N = 1000, means = c(x1 = 1, x2 = -1, x3 = 3),
##'                        sds = 1, rho = 0.4, stde = 5, distrib = rlogis)
##' lm1b <- lm(y ~ x1 + x2 + x3 + x1:x3, data = X1b)
##' summary(lm1b)
##'
##' ## t distribution is very sensitive for fractional df between 1 and 2 (recall
##' ## stde parameter is passed through to df in rt.
##' X1c <-
##'     genCorrelatedData3("y ~ 1.1 + 2.1 * x1 + 3 * x2 + 3.5 * x3 + 1.1 * x1:x3",
##'                        N = 1000, means = c(x1 = 1, x2 = -1, x3 = 3),
##'                        sds = 1, rho = 0.4, stde = 1.2, distrib = rt)
##' lm1c <- lm(y ~ x1 + x2 + x3 + x1:x3, data = X1c)
##' summary(lm1c)
##' 
genCorrelatedData3 <- function (formula, N = 100,
                                means = c("x1" = 50, "x2" =  50, "x3" = 50),
                                sds = 10, rho = 0,
                                stde = 1,
                                beta = c(0, 0.15, 0.1, -0.1),
                                intercept = FALSE, col.names,
                                verbose = FALSE, ..., distrib = rnorm)
{

    ## From Gabor Grothendieck, r-help August 24, 2018:
    isChar <- function(e, ch) identical(e, as.symbol(ch))
    ## From Gabor Grothendieck, r-help August 24, 2018:
    ## Only works if formula is written out, as in x1 + x2 + x1:x2,
    ## not if it is abbreviated 
    Parse <- function(e) {
        if (length(e) == 1) {
            if (is.numeric(e)) return(e)
            else setNames(1, as.character(e))
        } else {
            if (isChar(e[[1]], "*")) {
                x1 <- Recall(e[[2]])
                x2 <- Recall(e[[3]])
                setNames(unname(x1 * x2), paste0(names(x1), names(x2)))
            } else if (isChar(e[[1]], "+")) c(Recall(e[[2]]), Recall(e[[3]]))
            else if (isChar(e[[1]], "-")) {
                if (length(e) == 2) -1 * Recall(e[[2]])
                else c(Recall(e[[2]]), -Recall(e[[3]]))
            } else if (isChar(e[[1]], ":")) setNames(1, paste(e[-1], collapse = ":"))
        }
    }

    ## If col.names does not include all elements of varnames, stop
    ## varnames comes from names(beta)
    ## Allow "Intercept" exception
    checkVarnames <- function(col.names, varnames){
        uniquenames <- unique(unlist(strsplit(varnames, "[:+*]")))
        col.names <- unique(c("Intercept", col.names))
        if (any(!uniquenames %in% col.names)){
            MESSG <- paste("formula uses variable not present in matrix:",
                           paste(uniquenames[!uniquenames %in% col.names], collapse = ","))
            stop(MESSG)
        } ## else do nothing
        TRUE
    }

    ## creates text (character variable) like a formula, not an R formula
    betanamestoformula <- function(beta){
        namefix <- gsub(":", "*", names(beta))
        namefix <- gsub("^\\s*$", "1", namefix)
        paste(beta, namefix, collapse = " + ", sep = "*")
    }

    
    d <- length(means)
    if (missing(col.names) && is.null(names(means))) {
        col.names <- paste0("x", 1:d)
    }
    else if (missing(col.names) && !is.null(names(means))) {
        col.names <- names(means)
    }
    else {
        if (length(col.names) != d)
            stop(paste("If you provide column names, please provide one",
                "for each column in the means input"))
    }

    ldots <- list(...)
    
    x.mat <- as.matrix(genX(N, means, sds, rho, intercept = intercept,
                            col.names = col.names))
    ## update col.names to match generated data
    ## may be one more name than col.names, b/c intercept
    x.mat.col.names <- colnames(x.mat)
    if(!missing(formula)){
        if(!missing(beta)) stop("Don't provide both beta and formula arguments")
        if(!inherits(formula, "formula")) formula <- as.formula(formula)
        beta <- Parse(formula[[3]])
        checkVarnames(x.mat.col.names, names(beta)) ## will stop if fail
    } else {
        ## using user-provided beta
        if(!is.null(names(beta))){
            if(any(names(beta) == "")){
                MESSG <- "All elements in beta should have variable names if any have names"
                stop(MESSG)
            }
            checkVarnames(x.mat.col.names, names(beta)) ## will stop if fail
        } else {
            beta1 <- beta[1:(d + as.integer(intercept))]
            names(beta1) <- x.mat.col.names
            beta2 <- beta[-(1:(d + as.integer(intercept)))]
            beta2 <- c(beta2, rep(0, times = (d * (d + 1)/2) - length(beta2)))
            intnames.mat <- outer(col.names, col.names, FUN=function(x,y) paste(x, y, sep=":"))
            beta2.names <- intnames.mat[lower.tri(intnames.mat, diag=TRUE)]
            names(beta2) <- beta2.names
            beta <- c(beta1, beta2)
        }
    }

    x.mat <- as.data.frame(x.mat)
    x.mat$error <- if (deparse(substitute(distrib)) == "rt"){
                       rt(n = N, df = stde)
                   } else {
                       distrib(N, 0, stde)
                   }

    newformula <- betanamestoformula(beta)
    ## If "Intercept" appears, change it to 1
    newformula <- gsub("Intercept", "1", newformula)
    x.mat$eta <- with(x.mat, eval(parse(text = newformula)))
    x.mat$y  <-  x.mat$eta + x.mat$error
    attr(x.mat, "beta") <- beta
    attr(x.mat, "formula") <- newformula
    attr(x.mat, "stde") <- stde
    x.mat
}





