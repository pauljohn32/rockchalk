##' Extracts numeric variables and presents an summary in
##' a workable format.
##'
##' Finds the numeric variables, and ignores the others. (See
##' \code{summarizeFactors} for a function that handles non-numeric
##' variables). It will provide quantiles (specified \code{probs} as
##' well as other summary statistics, specified \code{stats}.  Results
##' are returned in a data frame. The main benefits from this compared
##' to R's default summary are 1) more summary information is returned
##' for each variable (dispersion), 2) the results are returned in a
##' form that is easy to use in further analysis, 3) the variables in
##' the output may be alphabetized.
##' 
##' @param dat a data frame or a matrix
##' @param alphaSort If TRUE, the columns are re-organized in
##'     alphabetical order. If FALSE, they are presented in the
##'     original order.
##' @param probs Controls calculation of quantiles (see the R
##'     \code{quantile} function's \code{probs} argument). If FALSE or
##'     NULL, no quantile estimates are provided. Default is
##'     \code{c("min" = 0, "med" = 0.5, "max" = 1.0)}, which will
##'     appear in output as \code{c("min", "med", "max")}. Other
##'     values between 0 and 1 are allowed. For example, \code{c(0.3, 0.7)}
##'     will appear in output as \code{pctile_30\%} and
##'     \code{pctile_70\%}.
##' @param stats A vector including any of these: \code{c("min",
##'     "med", "max", "mean", "sd", "var", "skewness", "kurtosis",
##'     "nobs", "nmiss")}. Default includes all except \code{var}.
##'     "nobs" means number of observations with non-missing, finite
##'     scores (not NA, NaN, -Inf, or Inf). "nmiss" is the number of
##'     cases with values of NA. If FALSE or NULL, provide none of
##'     these.
##' @param na.rm default TRUE. Should missing data be removed to
##'     calculate summaries?
##' @param unbiased If TRUE (default), skewness and kurtosis are
##'     calculated with biased corrected (N-1) divisor in the standard
##'     deviation.
##' @param digits Number of digits reported after decimal
##'     point. Default is 2
##' @export
##' @return a data.frame with one column per summary element (rows are
##'     the variables).
##' @seealso summarize and summarizeFactors
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##'
summarizeNumerics <-
    function(dat, alphaSort = FALSE, probs = c(0, 0.5, 1.0),
             stats = c("mean", "sd", "skewness",
                      "kurtosis", "nobs", "nmiss"), na.rm = TRUE,
             unbiased = TRUE, digits = 2)
{
    ##:ess-bp-start::browser@nil:##
if (isTRUE(stats)) stats <- c("mean", "sd", "skewness",
                                     "kurtosis", "nobs", "nmiss")
    if (is.atomic(dat)) {
        datname <- deparse(substitute(dat))
        dat <- data.frame(dat)
        if (NCOL(dat) == 1) {
            colnames(dat) <- datname
        } else {
            colnames(dat) <- paste0(datname, "_",  seq(1, NCOL(dat)))
        }
    } else if (!is.data.frame(dat)) dat <- as.data.frame(dat)

    var2 <- function(x, na.rm, unbiased) {
        if (unbiased) {
            var(x, na.rm = na.rm)
        } else {
            mean((x - mean(x, na.rm = na.rm))^2)
        }
    }

    sd2 <- function(x, na.rm, unbiased) {
        if (unbiased) {
            sd(x, na.rm = na.rm)
        } else {
            sqrt(mean((x - mean(x, na.rm = na.rm))^2))
        }
    }
       
    nums <- sapply(dat, is.numeric)
    if (sum(nums) == 0) return(NULL)
    datn <- dat[, nums, drop = FALSE]
    if (alphaSort)
        datn <- datn[, sort(colnames(datn)), drop = FALSE]

    qtiles <- NULL
    sumdat <- NULL

    if (!missing(probs) && !is.null(probs) && is.logical(probs) &&
        length(probs) == 1 && !is.na(probs) && probs){
        prob.std <- c("min" = 0.0, "med" = 0.5, "max" = 1.0)
        if(isTRUE(probs)){
            probs <- prob.std
        }
        ## if stats includes "min" "med" or "max", adjust probs
        prob.toadd <- names(prob.std)[names(prob.std) %in% stats]
        probs <- sort(unique(c(probs, prob.std[prob.toadd])))
    }

    if (is.numeric(probs)){
        qtiles <- t(apply(datn, 2, stats::quantile,
                          probs = probs,
                          na.rm = na.rm))
        ## TODO: use regex to match leading digit
        colnames(qtiles) <- paste0("pctile_", colnames(qtiles))
        old <- c("pctile_0%", "pctile_50%", "pctile_100%")
        new <- c("min", "med", "max")
        for(i in 1:3){
            colnames(qtiles) <- gsub(old[i], new[i], colnames(qtiles))
        }        
    }
    if (any(c("mean", "sd", "var", "skewness", "kurtosis", "nobs", "nmiss") %in% stats)) {
        res <- list()
        if("mean" %in% stats)                    
            res[["mean"]] <- apply(datn, 2, mean, na.rm = na.rm)
        if("sd" %in% stats)
            res[["sd"]] <- apply(datn, 2, sd2, na.rm = na.rm,
                                 unbiased = unbiased)
        if("var" %in% stats)
            res[["var"]] <- apply(datn, 2, var2, na.rm = na.rm,
                                  unbiased = unbiased)
        if("skewness" %in% stats)
            res[["skewness"]] <- apply(datn, 2, skewness, na.rm = na.rm,
                                            unbiased = unbiased)
        if("kurtosis" %in% stats)
            res[["kurtosis"]] <- apply(datn, 2, kurtosis, na.rm = na.rm,
                                       unbiased = unbiased)

        if("nobs" %in% stats)
            res[["nobs"]] <- apply(datn, 2, function(x) sum(is.finite(x)))
        if("nmiss" %in% stats)
            res[["nmissing"]] <- apply(datn, 2,
                                       function(x) sum(is.na(x)))
        sumdat <- as.data.frame(res)
    }
    if (!is.null(qtiles) && !is.null(sumdat)){
        reslt <- merge(qtiles, sumdat, by = "row.names", sort = FALSE)
        rownames(reslt) <- reslt$Row.names
        reslt[["Row.names"]] <- NULL
    } else if (is.null(qtiles)){
        reslt <- sumdat
    } else if (is.null(sumdat)){
        reslt <- qtiles
    }
   
    attr(reslt, "stats") <- stats
    attr(reslt, "digits") <- digits
    attr(reslt, "probs") <- probs
    reslt
}
NULL

##' Calculate excess kurtosis
##'
##' Kurtosis is a summary of a distribution's shape, using the Normal
##' distribution as a comparison. A distribution with high kurtosis is
##' said to be leptokurtic. It has wider, "fatter" tails and a
##' "sharper", more "peaked" center than a Normal distribution. In a
##' standard Normal distribution, the kurtosis is 3.  The term
##' "excess kurtosis" refers to the difference \eqn{kurtosis - 3}.
##' Many researchers use the term kurtosis to refer to
##' "excess kurtosis" and this function follows suit.  The user may
##' set excess = FALSE, in which case the uncentered kurtosis is
##' returned.
##'
##' If kurtosis is smaller than 3 (or excess kurtosis is negative),
##' the tails are "thinner" than the normal distribution (there is
##' lower chance of extreme deviations around the mean). If kurtosis
##' is greater than 3 (excess kurtosis positive), then the tails are
##' fatter (observations can be spread more widely than in the Normal
##' distribution).
##' 
##' The kurtosis may be calculated with the small-sample
##' bias-corrected estimate of the variance. Set unbiased = FALSE if
##' this is not desired.  It appears somewhat controversial whether
##' this is necessary. According to the
##' US NIST,
##' \url{http://www.itl.nist.gov/div898/handbook/eda/section3/eda35b.htm},
##' kurtosis is defined as
##'                   
##' \deqn{kurtosis =  ( mean((x - mean(x))^4) )/ var(x)^2}
##'             
##' where \eqn{var(x)} is calculated with the denominator \eqn{N},
##' rather than \eqn{N-1}.
##'
##' A distribution is said to be leptokurtic if it is tightly bunched
##' in the center (spiked) and there are long tails. The long tails
##' reflect the probability of extreme values.
##' @param x A numeric variable (vector)
##' @param na.rm default TRUE. If na.rm = FALSE and there are missing
##'     values, the mean and variance are undefined and this function
##'     returns NA.
##' @param excess default TRUE. If true, function returns excess
##'     kurtosis (kurtosis -3). If false, the return is simply
##'     kurtosis as defined above.
##' @param unbiased default TRUE. Should the denominator of the
##'     variance estimate be divided by N-1, rather than N?
##' @export
##' @return A scalar value or NA
##' @author Paul Johnson <pauljohn@@ku.edu>
kurtosis <- function(x, na.rm = TRUE, excess = TRUE, unbiased = TRUE){
    if (!isTRUE(na.rm) & sum(is.na(x) > 0)) return(NA)
    x <- x[!is.na(x)]
    xm <- mean(x)
    xd <- x - xm
    var <- mean(xd^2)
   
    if (unbiased){
        kur <- mean(xd^4)/(var(x)^2)
    } else {
         kur <- mean(xd^4)/var^2
    }
    if (isTRUE(excess)) kur <- kur - 3
    kur
}


##' Calculate skewness
##'
##' Skewness is a summary of the symmetry of a distribution's
##' probability density function. In a Normal distribution, the
##' skewness is 0, indicating symmetry about the expected value.
##'
##' If na.rm = FALSE and there are missing values, the mean and
##' variance are undefined and this function returns NA.
##'
##' The skewness may be calculated with the small-sample bias-corrected
##' estimate of the standard deviation.  It appears somewhat controversial
##' whether this is necessary, hence the argument unbiased is provided.
##' Set unbiased = FALSE if it is desired to have the one recommended
##' by NIST, for example. According to the US NIST,
##' \url{http://www.itl.nist.gov/div898/handbook/eda/section3/eda35b.htm},
##' skewness is defined as the mean of cubed deviations divided by the
##' cube of the standard deviation.
##'       
##'                mean((x - mean(x))^3)
##' skewness =    ___________________
##'                  sd(x)^3
##'
##' where sd(x) is calculated with the denominator N, rather than
##' N-1. This is the Fisher-Pearson coefficient of skewness, they claim.
##' The unbiased variant uses the standard deviation divisor (N-1) to
##' bias-correct the standard deviation. 
##' 
##' @param x A numeric variable (vector)
##' @param na.rm default TRUE. Should missing data be removed?
##' @param unbiased default TRUE. Should the denominator of the
##'     variance estimate be divided by N-1?
##' @export
##' @return A scalar value or NA
##' @author Paul Johnson <pauljohn@@ku.edu>
skewness <- function(x, na.rm = TRUE, unbiased = TRUE){
    if (!isTRUE(na.rm) & sum(is.na(x) > 0)) return(NA)
    x <- x[!is.na(x)]
    xm <- mean(x)
    if (unbiased){
        skew <-  mean((x - xm)^3)/var(x)^(3/2)
    } else {
        skew <- mean((x - xm)^3)/(mean((x - xm)^2))^(3/2)
    }
    skew
}



##' Extracts non-numeric variables, calculates summary information,
##' including entropy as a diversity indicator.
##'
##' This function finds the non- numeric variables and ignores the
##' others. (See \code{summarizeNumerics} for a function that
##' handles numeric variables.)  It then treats all non-numeric
##' variables as if they were factors, and summarizes each. The main
##' benefits from this compared to R's default summary are 1) more
##' summary information is returned for each variable (entropy
##' estimates ofdispersion), 2) the columns in the output are
##' alphabetized. To prevent alphabetization, use alphaSort = FALSE.
##'
##' Entropy is one possible measure of diversity. If all outcomes are
##' equally likely, the entropy is maximized, while if all outcomes
##' fall into one possible category, entropy is at its lowest
##' values. The lowest possible value for entropy is 0, while the
##' maximum value is dependent on the number of categories. Entropy is
##' also called Shannon's information index in some fields of study
##' (Balch, 2000 ; Shannon, 1949 ).
##'
##' Concerning the use of entropy as a diversity index, the user might
##' consult Balch(). For each possible outcome category, let p
##' represent the observed proportion of cases. The diversity
##' contribution of each category is -p * log2(p). Note that if p is
##' either 0 or 1, the diversity contribution is 0.  The sum of those
##' diversity contributions across possible outcomes is the entropy
##' estimate. The entropy value is a lower bound of 0, but there is no
##' upper bound that is independent of the number of possible
##' categories. If m is the number of categories, the maximum possible
##' value of entropy is -log2(1/m).
##'
##' Because the maximum value of entropy depends on the number of
##' possible categories, some scholars wish to re-scale so as to bring
##' the values into a common numeric scale. The normed entropy is
##' calculated as the observed entropy divided by the maximum possible
##' entropy.  Normed entropy takes on values between 0 and 1, so in a
##' sense, its values are more easily comparable. However, the
##' comparison is something of an illusion, since variables with the
##' same number of categories will always be comparable by their
##' entropy, whether it is normed or not.
##'
##' Warning: Variables of class POSIXt will be ignored. This will be
##' fixed in the future. The function works perfectly well with
##' numeric, factor, or character variables.  Other more elaborate
##' structures are likely to be trouble.
##' @param dat A data frame
##' @param maxLevels The maximum number of levels that will be
##'     reported.
##' @param alphaSort If TRUE (default), the columns are re-organized
##'     in alphabetical order. If FALSE, they are presented in the
##'     original order.
##' @param stats Default is \code{c("nobs", "nmiss", "entropy",
##'     "normedEntropy")}.
##' @param digits Default 2.
##' @export
##' @return A list of factor summaries
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @seealso \code{\link{summarizeNumerics}}
##'
##' @references
##'
##' Balch, T. (2000). Hierarchic Social Entropy: An Information
##' Theoretic Measure of Robot Group Diversity. Auton. Robots, 8(3),
##' 209-238.
##'
##' Shannon, Claude. E. (1949). The Mathematical Theory of
##' Communication. Urbana: University of Illinois Press.
##' @examples
##' set.seed(21234)
##' x <- runif(1000)
##' xn <- ifelse(x < 0.2, 0, ifelse(x < 0.6, 1, 2))
##' xf <- factor(xn, levels=c(0,1,2), labels("A","B","C"))
##' dat <- data.frame(xf, xn, x)
##' summarizeFactors(dat)
##' ##see help for summarize for more examples
summarizeFactors <-
    function (dat = NULL, maxLevels = 5, alphaSort = TRUE,
              stats = c("entropy", "normedEntropy", "nobs", "nmiss"),
              digits = 2)
{
    if (is.atomic(dat)){
        datname <- deparse(substitute(dat))
        dat <- data.frame(dat)
        if (NCOL(dat) == 1) {
            colnames(dat) <- datname
        } else {
            colnames(dat) <- paste0(datname, "_",  seq(1, NCOL(dat)))
        }
    } else if (!is.data.frame(dat)) dat <- as.data.frame(dat)
    factors <- sapply(dat, function(x) {!is.numeric(x) & !inherits(x, "POSIXt")})
    if (sum(factors) == 0) return(NULL)
    datf <- dat[, factors, drop = FALSE]
    if (alphaSort)
        datf <- datf[, sort(colnames(datf)), drop = FALSE]
    z <- lapply(datf, summary.factor)
    attr(z, "class") <- c("summarizedFactors")
    attr(z, "maxLevels") <- maxLevels
    attr(z, "stats") <- stats
    attr(z, "digits") <- digits
    z
}
NULL
    


##' Prints out the contents of an object created by summarizeFactors
##' in the style of base::summary
##'
##' An object with class "summarizedFactors" is the input. Such an
##' object should be created with the function
##' rockchalk::summarizeFactors. Each element in that list is then
##' organized for printing in a tabular summary.  This should look
##' almost like R's own summary function, except for the additional
##' information that these factor summaries include.
##'
##' @export
##' @param x A summarizedFactors object produced by summarizeFactors
##' @param ... optional arguments. Only value currently used is digits,
##'   which defaults to 2.
##' @return A table of formatted output
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @seealso \code{\link{summarize}}, \code{\link{summarizeFactors}}, \code{\link{formatSummarizedNumerics}}
##' @examples
##' dat <- data.frame(xcat1 = gl(10, 3), xcat2 = gl(5, 6))
##' summarizeFactors(dat, maxLevels = 8)
##' formatSummarizedFactors(summarizeFactors(dat))
formatSummarizedFactors <- function(x, ...){
    ncw <- function(x) {
        z <- nchar(x, type = "w")
        if (any(na <- is.na(z))) {
            z[na] <- nchar(encodeString(z[na]), "b")
        }
        z
    }
    dots <- list(...)
    if (!is.null(dots$digits)) {
        digits = dots$digits
        dots$digits <- NULL
    } else if (!is.null(attr(x, "digits"))){
        digits <- attr(x, "digits")
    } else {
        digits <- 2
    }
    
    if (!is.null(dots$maxLevels)) {
        maxLevels = dots$maxLevels
        dots$maxLevels <- NULL
    } else if (!is.null(attr(x, "maxLevels"))){
        maxLevels <- attr(x, "maxLevels")
    } else {
        maxLevels <- 5
    }

    
    if (!is.null(dots$stats)) {
        stats <- dots$stats
        dots$stats <- NULL
    } else if (!is.null(attr(x, "stats"))){
        stats <- attr(x, "stats")
    } else {
        stats <- c("entropy", "normedEntropy", "nobs", "nmiss")
    }
    ## Omit stats requests that do not apply to factors
    stats.for.factors <- c("entropy", "normedEntropy", "nobs", "nmiss")
    stats <- stats[stats %in% stats.for.factors]
    
    nv <- length(x)
    ## cycle through variables and reduce them to maxLevels if needed
    ## prune out unwanted elements in stats as well
    for (i in 1L:nv){
        tt <- x[[i]][["table"]]
        if (length(tt) > maxLevels) {
            ## Only sort.list if to many levels
            o <- sort.list(tt, decreasing = TRUE)
            toExclude <- maxLevels:length(tt)
            tt <- c(tt[o[-toExclude]], `(All Others)` = sum(tt[o[toExclude]]))
            x[[i]][["table"]] <- tt
        }
        if(length(stats) == 0) {
            x[[i]][["stats"]] <- NULL
        } else {
            ## keep desired stats
            stats.est <- x[[i]][["stats"]]
            if (!is.null(stats.est)){
                stats.est <- stats.est[names(stats.est) %in% stats]
                x[[i]][["stats"]] <- stats.est
            }
        }
    }
    
    nm <- names(x)
    lw <- numeric(nv)
    ## nr maximum number of rows needed for any table
    nr <- max(unlist(lapply(x, function(y) NROW(y[["table"]]))))
    nsumlines <- max(unlist(lapply(x, function(y){
        if(is.null(y[["stats"]])) 0 else length(y[["stats"]])})))
    ##nr <- nr + nsumlines
    reslt <- list()
    for (i in 1L:nv) {
        tt <- x[[i]][["table"]]
        stats.est <- x[[i]][["stats"]]
        lbs1 <- format(names(tt))
        vals <- as.integer(tt)
        ttnew <- paste(lbs1, ": ", vals, sep = "")
        length(ttnew) <- nr
        if(is.null(stats.est)){
            lw[i] <- ncw(lbs1[1L])
            reslt[[i]] <- ttnew
            next()
        } else {
            wide1 <- ncw(lbs1[1L])
            lbs2 <- format(names(stats.est))
            vals <- format(stats.est, digits = digits)
            stats.est.new <- paste(lbs2, ": ", vals, sep = "")
            ttnew2 <- c(ttnew, stats.est.new)
            wide2 <- ncw(lbs2[1L])
            reslt[[i]] <- ttnew2
            lw[i] <- max(wide1, wide2) + 5
        }
    }
    reslt <- unlist(reslt, use.names = TRUE)
    dim(reslt) <- c(nr + nsumlines, nv)
    if (any(is.na(lw)))
        warning("probably wrong encoding in names(.) of column ",
                paste(which(is.na(lw)), collapse = ", "))
    blanks <- paste(character(max(lw, na.rm = TRUE) + 2L), collapse = " ")
    pad <- floor(lw - ncw(nm)/2)
    nm <- paste(substring(blanks, 1, pad), nm, sep = "")
    dimnames(reslt) <- list(rep.int("", nr + nsumlines), nm)
    attr(reslt, "class") <- c("table")
    reslt
}
NULL


##' Sorts numeric from discrete variables and returns separate
##' summaries for those types of variables.
##'
##' The work is done by the functions \code{summarizeNumerics} and
##' \code{summarizeFactors}.  Please see the help pages for those
##' functions for complete details.
##'
##' The major purpose here is to generate summary data structure
##' that is more useful in subsequent data analysis.  The numeric
##' portion of the summaries are a data frame that can be used in
##' plots or other diagnostics.
##' 
##' The term "factors" was used, but "discrete variables" would have
##' been more accurate.  The factor summaries will collect all
##' logical, factor, ordered, and character variables.
##'
##' Other variable types, such as Dates, will be ignored, with a
##' warning.
##' 
##' @param dat A data frame
##' @param alphaSort If TRUE, the columns are re-organized in
##'     alphabetical order. If FALSE, they are presented in the
##'     original order.
##' @param stats A vector of desired summary statistics. Set
##'     \code{stats = NULL} to omit all stat summaries. Legal elements
##'     are \code{c("min", "med", "max", "mean", "sd", "var",
##'     "skewness", "kurtosis", "entropy", "normedEntropy", "nobs",
##'     "nmiss")}. The statistics \code{c("entropy", "normedEntropy")}
##'     are available only for factor variables, while mean, variance,
##'     and so forth will be calculated only for numeric variables.
##'     \code{"nobs"} is the number of observations with non-missing,
##'     finite scores (not NA, NaN, -Inf, or Inf). \code{"nmiss"} is
##'     the number of cases with values of NA. The default setting for
##'     \code{probs} will cause \code{c("min", "med", "max")} to be
##'     included, they need not be requested explicitly. To disable
##'     them, revise \code{probs}.
##' @param probs For numeric variables, is used with the
##'     \code{quantile} function. The default is
##'     \code{probs = c(0, .50, 1.0)}, which are labeled in output as
##'     \code{c("min", "med", and "max")}. Set \code{probs = NULL}
##'     to prevent these in the output.
##' @param digits Decimal values to display, defaults as 2.
##' @param ... Optional arguments that are passed to
##'     \code{summarizeNumerics} and \code{summarizeFactors}. For
##'     numeric variables, one can specify \code{na.rm} and
##'     \code{unbiased}.  For discrete variables, the key argument is
##'     \code{maxLevels}, which determines the number of levels that
##'     will be reported in tables for discrete variables.
##' @return Return is a list with two objects 1) output from
##'     summarizeNumerics: a data frame with variable names on rows
##'     and summary stats on columns, 2) output from summarizeFactors:
##'     a list with summary information about each discrete
##'     variable. The display on-screen is governed by a method
##'     \code{print.summarize}.
##' @export
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @example inst/examples/summarize-ex.R
summarize <-
    function(dat, alphaSort = FALSE,
             stats = c("mean", "sd", "skewness", "kurtosis", "entropy",
                       "normedEntropy", "nobs", "nmiss"), probs = c(0, 0.50, 1.0),
             digits = 3, ...)
{
    dots <- list(...)
    if (is.atomic(dat)){
	   datname <- deparse(substitute(dat))
	   dat <- data.frame(dat)
           if (NCOL(dat) == 1) {
               colnames(dat) <- datname
           } else {
               colnames(dat) <- paste0(datname, "_",  seq(1, NCOL(dat)))
           }
    } else if (!is.data.frame(dat)) dat <- as.data.frame(dat)

    dotnames <- names(dots)
    ## next should give "alphaSort" "stats" "na.rm" "unbiased"
    nnames <- names(formals(summarizeNumerics))[-1L]
    ## args for numeric vars: names that need keeping if in dots
    ## initial argument list
    numargs <- list(dat = quote(dat), alphaSort = alphaSort, probs = probs,
                     stats = stats, digits = digits)
    argList <- modifyList(numargs, dots[names(dots) %in% nnames])
    datn <- do.call("summarizeNumerics", as.list(argList))

    fnames <- names(formals(summarizeFactors))[-1L]
    
    fargs <- list(dat = quote(dat), alphaSort = alphaSort, 
                     stats = stats, digits = digits)
    ## only possible addition is maxLevvels:
    fnames <- "maxLevels"
    argList <- modifyList(fargs, dots[names(dots) %in% fnames])
    datf <- do.call("summarizeFactors", as.list(argList))
    
    value <- list(numerics = datn, factors = datf)
    attr(value, "class") <- c("summarize", class(value))
    if (!is.null(datn)) {
        attr(value, "numeric.formatted") <- formatSummarizedNumerics(datn, digits = digits)
    }
    if (!is.null(datf)) {
        attr(value, "factor.formatted") <- formatSummarizedFactors(datf, digits = digits)
    }
    value
}
NULL


##' print method for output from summarize
##'
##' Be aware that the unrounded numeric matrix
##' is available as an attribute of the returned object.
##' This method displays a rounded, character-formatted
##' display of the numeric varibles.
##' @param x Object produced by summarize
##' @param digits Decimal values to display, defaults as 2.
##' @param ... optional arguments for print function.
##' @return x, unchanged
##' Prints objects created by summarize
##' @method print summarize
##' @export
print.summarize <- function(x, digits, ...){
    digits <- if(!missing(digits) && !is.null(digits)) digits else NULL
    if(!is.null(x$numerics)){
        cat("Numeric variables\n")
        num.frmt <- formatSummarizedNumerics(x$numerics, digits = digits, ...)
        print(num.frmt, quote = FALSE, print.gap = 3)
    } else {
        num.frmt <- NULL
    }
    if(!is.null(x$factors)){
        cat("\nNonnumeric variables\n")
        fac.frmt <- formatSummarizedFactors(x$factors, digits = digits, ...)
        print(fac.frmt)
    } else {
        fac.frmt <- NULL
    }
    invisible(list(numerics = num.frmt, factors = fac.frmt))
}
    
##' Reformat numeric summarize output as one column per variable,
##' similar to R summary
##'
##' The summarizeNumeric function returns a data frame with the
##' variable names on the rows and summary statistics (mean, median,
##' std. deviation) in the columns.This transposes and abbreviates
##' the information to look more like R summary.
##' 
##' @param x numeric summaries from summarize function
##' @param ... Other arguments, such as digits
##' @return An R \code{table} object
##' @author Paul Johnson
##' @export
##' @examples
##' set.seed(21234)
##' X <- matrix(rnorm(10000), ncol = 10, dimnames = list(NULL, paste0("xvar", 1:10)))
##' Xsum <- summarize(X)
##' Xsum$numerics
##' formatSummarizedNumerics(Xsum$numerics)
##' formatSummarizedNumerics(Xsum$numerics, digits = 5)
##' Xsum.fmt <- formatSummarizedNumerics(Xsum$numerics)
##' str(Xsum.fmt)
formatSummarizedNumerics <- function(x, ...){
    dots <- list(...)
    
    if (!is.null(dots$digits)) {
        digits = dots$digits
        dots$digits <- NULL
    } else if (!is.null(attr(x, "digits"))){
        digits <- attr(x, "digits")
    } else {
        digits <- 2
    }
        
    datnfmt <- x
    for (i in colnames(datnfmt)) datnfmt[ , i] <- round(datnfmt[ , i], digits)
    datnfmt <- format(t(datnfmt), trim = FALSE, scientific = 10)
    datnfmt <- gsub("(\\.0+)$", " ", datnfmt)
    class(datnfmt) <- c("table")
    lcolnames <-  nchar(dimnames(datnfmt)[[2]])
    colmax <- apply(datnfmt, 2, function(i) max(nchar(i)))
    padcount <- ifelse(colmax > lcolnames + 1, round(0.5*(colmax - lcolnames)), 0)
    pads <- vapply(padcount, function(i) {
            paste0(rep(" ", max(0, i, na.rm = TRUE)), collapse = "")
    }, character(1))
    dimnames(datnfmt)[[2]] <- paste0(pads, dimnames(datnfmt)[[2]])
    datnfmt
}

    
##' Tabulates observed values and calculates entropy
##'
##' This adapts code from R base summary.factor. It adds
##' the calculation of entropy as a measure of diversity.
##'
##' @param y a factor (non-numeric variable)
##' @return A list, including the summary table and vector of summary
##'     stats if requested.
##' @author Paul E. Johnson <pauljohn@@ku.edu>
summary.factor <-
    function(y)
{
    ## 5 nested functions to be used later
    divr <- function(p = 0) {
        ifelse(p > 0 & p < 1, -p * log2(p), 0)
    }
    entropy <- function(p) {
        sum(divr(p))
    }
    maximumEntropy <- function(N) -log2(1/N)
    normedEntropy <- function(x) {
		xent <- entropy(x)
	    if(xent == 0) return(0)
		xent/maximumEntropy(length(x))
	}
    nmiss <- sum(is.na(y), na.rm = TRUE)
    y <- factor(y)
    ll <- levels(y)
    tbl <- table(y)
    tt <- c(tbl)
    names(tt) <- dimnames(tbl)[[1L]]
    props <- prop.table(tbl)
    ## Calculate all stats, even if don't ask for them
    stat.est <- c(nobs = sum(tbl),
                  nmiss = nmiss,
                  entropy = entropy(props),
                  normedEntropy = normedEntropy(props))
    ##tt <- list(table = tt, stats = stat.est[names(stat.est) %in% stats])
    tt <- list(table = tt, stats = stat.est)
    tt
}
NULL




