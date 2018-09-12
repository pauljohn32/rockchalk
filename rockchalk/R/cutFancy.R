
##' Create an ordinal variable by grouping numeric data input.
##'
##' This is a convenience function for usage of R's \code{cut}
##' function. Users can specify cutpoints or category labels or
##' desired proportions of groups in various ways. In that way, it has
##' a more flexible interface than \code{cut}. It also tries to notice
##' and correct some common user errors, such as omitting the outer
##' boundaries from the probs argument. The returned values are
##' labeled by their midpoints, rather than cut's usual boundaries.
##'
##' The dividing points, thought of as "thresholds" or "cutpoints",
##' can be specified in several ways.  \code{cutFancy} will
##' automatically create equally-sized sets of observations for a
##' given number of categories if neither \code{probs} nor
##' \code{cutpoints} is specified. The bare minimum input needed is
##' \code{categories=5}, for example, to ask for 5 equally sized
##' groups. More user control can be had by specifying either
##' \code{cutpoints} or \code{probs}. If \code{cutpoints} is not
##' specified at all, or if \code{cutpoints="quantile"}, then
##' \code{probs} can be used to specify the proportions of the data
##' points that are to fall within each range. On the other hand, one
##' can specify \code{cutpoints = "quantile"} and then \code{probs} will
##' be used to specify the proportions of the data points that are to
##' fall within each range.
##' 
##' If \code{categories} is not specified, the category names will be
##' created. Names for ordinal categories will be the numerical
##' midpoints for the outcomes.  Perhaps this will deviate from your
##' expectation, which might be ordinal categories name "0", "1", "2",
##' and so forth.  The numerically labeled values we provide can be
##' used in various ways during the analysis process. Read "?factor"
##' to learn ways to convert the ordinal output to other
##' formats. Examples include various ways of converting the ordinal
##' output to numeric.
##' 
##' The \code{categories} parameter works together with
##' \code{cutpoints}.  \code{cutpoints} allows a character string
##' "quantile". If \code{cutpoints} is not specified, or if the user
##' specifies a character string \code{cutpoints="quantile"}, then the
##' \code{probs} would be used to determine the cutpoints.  However,
##' if \code{probs} is not specified, then the \code{categories}
##' argument can be used. If \code{cutpoints="quantile"}, then
##' 
##' \itemize{
##' \item if \code{categories} is one integer, then it is interpreted
##' as the number of "equally sized" categories to be created, or
##'
##' \item \code{categories} can be a vector of names. The length
##' of the vector is used to determine the number of categories, and
##' the values are put to use as factor labels.
##' }
##' 
##' @param y The input data from which the categorized variable will
##'     be created.
##' @param cutpoints Optional paramter, a vector of thresholds at
##'     which to cut the data. If it is not supplied, the default
##'     value \code{cutpoints="quantile"} will take effect. Users can
##'     supplement with \code{probs} and/or \code{categories} as shown
##'     in examples.
##' @param probs This is an optional parameter, relevant only when the
##'     R function \code{\link{quantile}} function is used to
##'     calculate cutpoints. The length should be number of desired
##'     categories PLUS ONE, as in \code{c(0, .3, .6, 1)}. That will
##'     create categories that represent 1) less than .3, between .3
##'     and .6, and above .6.  A common user error is to specify only
##'     the internal divider values, such as \code{probs = c(.3,
##'     .6)}. To anticipate and correct that error, this function will
##'     insert the lower limit of 0 and the upper limit of 1 if they
##'     are not already present in \code{probs}.
##' @param categories Can be a number to designate the number of
##'     sub-groups created, or it can be a vector of names used. If
##'     \code{cutpoints} and \code{probs} are not specified, the
##'     parameter \code{categories} should be an integer to specify
##'     how many data groups to create.It is required if
##'     cutpoints="quantile" and probs is not specified. Can also be a
##'     vector of names to be used for the categories that are
##'     created. If category names are not provided, the names for the
##'     ordinal variable will be the midpoint of the numeric range
##'     from which they are constructed.
##' @return an ordinal vector with attributes "cutpoints" and "props"
##'     (proportions)
##' @importFrom stats embed
##' @name cutFancy
##' @export cutFancy
##' @examples
##' set.seed(234234)
##' y <- rnorm(1000, m = 35, sd = 14)
##' yord <- cutFancy(y, cutpoints = c(30, 40, 50))
##' table(yord)
##' attr(yord, "props")
##' attr(yord, "cutpoints")
##' yord <- cutFancy(y, categories = 4L)
##' table(yord, exclude = NULL)
##' attr(yord, "props")
##' attr(yord, "cutpoints")
##' yord <- cutFancy(y, probs = c(0, .1, .3, .7, .9, 1.0),
##'                   categories = c("A", "B", "C", "D", "E"))
##' table(yord, exclude = NULL)
##' attr(yord, "props")
##' attr(yord, "cutpoints")
##' yord <- cutFancy(y, probs = c(0, .1, .3, .7, .9, 1.0))
##' table(yord, exclude = NULL)
##' attr(yord, "props")
##' attr(yord, "cutpoints")
##' yasinteger <- as.integer(yord)
##' table(yasinteger, yord)
##' yasnumeric <- as.numeric(levels(yord))[yord]
##' table(yasnumeric, yord)
##' barplot(attr(yord, "props"))
##' hist(yasnumeric)
##' X1a <-
##'    genCorrelatedData3("y ~ 1.1 + 2.1 * x1 + 3 * x2 + 3.5 * x3 + 1.1 * x1:x3",
##'                        N = 10000, means = c(x1 = 1, x2 = -1, x3 = 3),
##'                        sds = 1, rho = 0.4)
##' ## Create cutpoints from quantiles
##' probs <- c(.3, .6)
##' X1a$yord <- cutFancy(X1a$y, probs = probs)
##' attributes(X1a$yord)
##' table(X1a$yord, exclude = NULL)
cutFancy <- function(y, cutpoints = "quantile", probs, categories){
    if(!missing(cutpoints) && !is.null(cutpoints)){
        if(is.numeric(cutpoints)){
            ## Use dividers provided by user
            yrange <- range(y, na.rm=TRUE)
            ## need outer boundaries to get centers:
            breaks <- c(yrange[1], cutpoints, yrange[2]) 
            centers <- rowMeans(embed(breaks, 2))
            if(missing(categories) || is.null(categories)){
                ## create category names: centernames
                centernames <- formatC(centers, 2)
            } else {
                centernames <- categories
            }
            ## Use input category labels
            yord <- cut(y, breaks = breaks, labels = centernames,
                        include.lowest = TRUE, ordered_result = TRUE)
            props <- prop.table(table(yord))
            attr(yord, "cutpoints") <- cutpoints
            attr(yord, "props") <- props
            return(invisible(yord))
        } else if(cutpoints != "quantile") {
            MESSG <- "only `quantile` is allowed if cutpoints is not numeric thresolds"
            stop(MESSG)
        }
    }

    ## pad probs with 0, and 1 one edges to help with common user error
    if(!missing(probs) && !is.null(probs)){
        if(abs(zapsmall(probs[1] - 0)) > 0) {probs <- c(0, probs)}
        if(abs(zapsmall(1 - probs[length(probs)])) > 0) {probs <- c(probs, 1)}
    }
    
    ## categories = 1 number, not vector of names
    if(!missing(categories) && !is.null(categories) && length(categories) == 1) {
        categories <- as.integer(categories)
        if(is.null(categories)){
            MESSG <- "categories cannot be coerced to an integer"
            stop(MESSG)
        }
        ## we will make up names by midpoints of data
        if(missing(probs) || is.null(probs)){
            ## categories is an integer:
            ## cutpoints is even spaced sections with n of groups = "categories"
            probs <- seq(0, 1, length.out = categories + 1)
        } else {
            MESSG <- "single-value categories parameter is ignored if probs is specified"
            warning(MESSG)
            breaks <- quantile(y, probs =  probs)
        }
        breaks <- quantile(y, probs =  probs)
        centers <- rowMeans(embed(breaks, 2))
        centernames <- formatC(centers, 2)
    } else if(!missing(categories) && !is.null(categories) && length(categories) > 1){
        ## categories has more values
        if(!missing(probs) && !is.null(probs)){
            if (length(categories) +1 != length(probs)){
                MESSG <- "Number of categories should be 1 less than probs vector"
                stop(MESSG)
            }
        }
        if(missing(probs) || is.null(probs)){
            ## Use categories to figure how many levels needed
            ## cutpoints is even spaced sections with n of groups = "categories"
            probs <- seq(0, 1, length.out = length(categories) + 1)
        }
        breaks <- quantile(y, probs = probs)
        centernames <- categories
    } else if(missing(categories) || is.null(categories)) {
        if(is.null(probs) || missing(probs)){
              MESSG <- "Must specify either categories or probs"
              stop(MESSG)
        } else {
            breaks <- quantile(y, probs =  probs)
            centernames <- formatC(rowMeans(embed(breaks, 2)))
        }
    }
    yord <- cut(y, breaks = breaks, labels = centernames,
                include.lowest = TRUE, ordered_result = TRUE)
    ## exclude outer edges
    cutpoints <- unname(breaks[-c(1, length(breaks))])
    props <- prop.table(table(yord))
    attr(yord, "cutpoints") <- cutpoints
    attr(yord, "props") <- props
    invisible(yord)
}
