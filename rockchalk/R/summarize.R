##' Extracts numeric variables and presents an alphabetized summary in
##' a workable format.
##'
##' This function finds the numeric variables and ignores the
##' others. (See \code{summarizeFactors} for a function that
##' handles non-numeric variables.). It calculates the quantiles for
##' each variable, as well as the mean, standard deviation, and
##' variance, and then packs those results into a matrix. The main
##' benefits from this compared to R's default summary are 1) more
##' summary information is returned for each variable (dispersion), 2)
##' the results are returned in a matrix that is easy to use in
##' further analysis, 3) the columns in the output are
##' alphabetized. To prevent alphabetization, use
##' alphaSort = FALSE.
##' @param dat a data frame or a matrix
##' @param alphaSort If TRUE (default), the columns are re-organized
##' in alphabetical order. If FALSE, they are presented in the
##' original order.
##' @param sumstat If TRUE (default), include mean, standard deviation, and count of NAs.
##' @param digits integer, used for number formatting output.
##' @export
##' @return a matrix with one column per variable and the rows
##' representing the quantiles as well as the mean, standard
##' deviation, and variance.
##' @seealso summarize and summarizeFactors
##' @author Paul E. Johnson <pauljohn@@ku.edu>
summarizeNumerics <- function(dat, alphaSort = TRUE, sumstat = TRUE,
    digits = max(3, getOption("digits") - 3)) {
    if (is.atomic(dat)) {
        datname <- deparse(substitute(dat))
        dat <- data.frame(dat)
        if (NCOL(dat) == 1) {
            colnames(dat) <- datname
        } else {
            colnames(dat) <- paste0(datname, "_",  seq(1, NCOL(dat)))
        }
    } else if (!is.data.frame(dat)) dat <- as.data.frame(dat)
    
    nums <- sapply(dat, is.numeric)
    if (sum(nums) == 0) return(NULL)
    datn <- dat[, nums, drop = FALSE]
    if (alphaSort)
        datn <- datn[, sort(colnames(datn)), drop = FALSE]
    sumdat <- apply(datn, 2, stats::quantile, na.rm = TRUE)
    sumdat <- round(sumdat, digits)
    if (sumstat) {
        sumdat <- rbind(sumdat, mean = apply(datn, 2, mean, na.rm = TRUE))
        sumdat <- rbind(sumdat, sd = apply(datn, 2, sd, na.rm = TRUE))
        sumdat <- rbind(sumdat, var = apply(datn, 2, var, na.rm = TRUE))
        sumdat <- round(sumdat, digits)
        sumdat <- rbind(sumdat, `NA's` = apply(datn, 2, function(x) sum(is.na(x))))
        sumdat <- rbind(sumdat, N = apply(datn, 2, function(x) length(x)))
    }
   sumdat
}
NULL

##' Tabulates observed values and calculates entropy
##'
##' This adapts code from R base summary.factor. It adds
##' the calculation of entropy as a measure of diversity.
##'
##' @param y a factor (non-numeric variable)
##' @param maxLevels The maximum number of levels that will
##' be presented in the tabulation.
##' @param sumstat If TRUE (default), entropy (diversity) estimate and
##' the number of NAs will be returned.
##' @return a vector of named elements including the summary
##' table as well as entropy and normed entropy.
##' @author Paul E. Johnson <pauljohn@@ku.edu>
summary.factor <-
    function(y, maxLevels = 5, sumstat = TRUE)
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
    nas <- is.na(y)
    y <- factor(y)
    ll <- levels(y)
    tbl <- table(y)
    tt <- c(tbl)
    names(tt) <- dimnames(tbl)[[1L]]
    o <- sort.list(tt, decreasing = TRUE)
    if (length(ll) > maxLevels) {
        toExclude <- maxLevels:length(ll)
        tt <- c(tt[o[-toExclude]], `(All Others)` = sum(tt[o[toExclude]]),
            `NA's` = sum(nas))
    } else {
        tt <- c(tt[o], `NA's` = sum(nas))
    }
    if (!sumstat) return(tt)
    props <- prop.table(tbl)
    tt <- c(tt, entropy = entropy(props), normedEntropy = normedEntropy(props), N= length(y))
}
NULL

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
##' @param maxLevels The maximum number of levels that will be reported.
##' @param alphaSort If TRUE (default), the columns are re-organized
##' in alphabetical order. If FALSE, they are presented in the
##' original order.
##' @param sumstat If TRUE (default), report indicators of dispersion
##' and the number of missing cases (NAs).
##' @param digits  integer, used for number formatting output.
##' @export
##' @return A list of factor summaries
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @seealso \code{\link{summarizeFactors}} and \code{\link{summarizeNumerics}}
##'
##' @references
##'
##' Balch, T. (2000). Hierarchic Social Entropy: An Information
##' Theoretic Measure of Robot Group Diversity. Auton. Robots, 8(3),
##' 209-238.
##'
##' Shannon, Claude. E. (1949). The Mathematical Theory of Communication. Urbana: University of Illinois Press.
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
              sumstat= TRUE, digits = max(3, getOption("digits") - 3))
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
    z <- lapply(datf, summary.factor,
                maxLevels = maxLevels, sumstat = sumstat)
    attr(z, "class") <- c("factorSummaries")
    z
}
NULL

##' Prints out the contents of an object created by summarizeFactors
##' in the style of base::summary
##'
##' An object with class "factorSummaries" is the input. Such an
##' object should be created with the function
##' rockchalk::summarizeFactors. Each element in that list is then
##' organized for printing in a tabular summary.  This should look
##' almost like R's own summary function, except for the additional
##' information that these factor summaries include.
##'
##' @method print factorSummaries
##' @export
##' @param x A factorSummaries object produced by summarizeFactors
##' @param ... optional arguments. Only value currently used is digits.
##' @return A table of formatted output
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @seealso \code{\link[base]{summary}} and
##' \code{\link{summarize}},
##' \code{\link{summarizeFactors}}
print.factorSummaries <- function(x, ...){
    ncw <- function(x) {
        z <- nchar(x, type = "w")
        if (any(na <- is.na(z))) {
            z[na] <- nchar(encodeString(z[na]), "b")
        }
        z
    }
    dots <- list(...)
    if (!is.null(dots$digits)) digits = dots$digits
    else digits = max(3, getOption("digits") - 3)
    nv <- length(x)
    nm <- names(x)
    lw <- numeric(nv)
    nr <- max(unlist(lapply(x, NROW)))
    for (i in 1L:nv) {
        sms <- x[[i]]
        lbs <- format(names(sms))
        sms <- paste(lbs, ":", format(sms, digits = digits),
                     "  ", sep = "")
        lw[i] <- ncw(lbs[1L])
        length(sms) <- nr
        x[[i]] <- sms
    }
    x <- unlist(x, use.names = TRUE)
    dim(x) <- c(nr, nv)
    if (any(is.na(lw)))
        warning("probably wrong encoding in names(.) of column ",
                paste(which(is.na(lw)), collapse = ", "))
    blanks <- paste(character(max(lw, na.rm = TRUE) + 2L), collapse = " ")
    pad <- floor(lw - ncw(nm)/2)
    nm <- paste(substring(blanks, 1, pad), nm, sep = "")
    dimnames(x) <- list(rep.int("", nr), nm)
    attr(x, "class") <- c("table")
    print(x)
    invisible(x)
}
NULL


##' Sorts numeric from factor variables and returns separate
##' summaries for those types of variables.
##'
##' The work is done by the functions \code{summarizeNumerics} and
##' \code{summarizeFactors}.  Please see the help pages for those
##' functions for complete details.
##'
##' @param dat A data frame
##' @param ... Optional arguments that are passed to summarizeNumerics and summarizeFactors.  These may be used:
##'  maxLevels The maximum number of levels that will be reported.
##'  alphaSort If TRUE (default), the columns are re-organized in alphabetical order. If FALSE, they are presented in the original order.
##'  digits  integer, used for number formatting output.
##' @return A list with 2 objects, numerics and factors. numerics is a matrix
##' of summary information, while factors is a list of factor summaries.
##' @export
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @example inst/examples/summarize-ex.R
summarize <-
    function(dat, ...)
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
    ## next should give c('digits', 'alphaSort')
    nnames <- names(formals(rockchalk::summarizeNumerics))[-1L]
    ## names that need keeping if in dots:
    keepnames <- dotnames %in% nnames
    if (sum(keepnames) > 0) {
        argList <- modifyList(list(dat = quote(dat)), dots[keepnames])
        datn <- do.call("summarizeNumerics", argList)
    } else {
        datn <- do.call("summarizeNumerics", args = list(dat = quote(dat)))
    }

    ## all ... can go to summarizeFactors
    datf <- rockchalk::summarizeFactors(dat, ...)

    value <- list(numerics = datn, factors = datf)
    value
}
NULL
