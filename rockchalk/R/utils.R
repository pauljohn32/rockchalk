#' Create sequences for plotting
#'
#' \code{plotSeq} is a convenience for the creation of sequence
#' that can be used for plotting example values and calculating
#' predicted values. By default, the length of the plotting
#' sequence will be equal to the length of the original sequence.
#' In that case, the only effect is to create an evenly-spaced
#' set of values. If \code{length.out} is specified, the user
#' determines the number of elements in plotSeq.
#'
#' The primary intended usage is for the creation of
#' plotting sequences of numeric variables.  It takes
#' a variable's range and the fills in evenly spaced steps.
#' If x is a factor variable, the levels will be returned.
#' Uses of this functionality are planned in the future.
#' @usage plotSeq(x, length.out = length(x))
#' @param x an R vector variable
#' @param length.out the number of elements in the desired plotting sequence.
#' @export plotSeq
#' @seealso \code{pretty}
#' @examples
#' #Create a quadratic regression
#'
#' stde <- 14
#' x <- rnorm(100, m=50, s=10)
#' y <- 0.2 - 02*x + 0.2*x^2 + stde*rnorm(100)
#' mod1 <- lm (y ~ poly(x, 2))
#'
#' plot(x, y, main="The Quadratic Regression")
#' seqx <- plotSeq(x, length.out=10)
#' seqy <- predict(mod1, newdata=data.frame(x=seqx))
#' lines(seqx, seqy, col="red")
#'
#' # Notice the bad result when a plotting sequence is
#' # not used.
#' plot(x, y, main="Bad Plot Result")
#' seqy <- predict(mod1)
#' lines(x, seqy, col="green")

plotSeq <- function (x, length.out = length(x))
{
  if (is.numeric(x)){
    xr <- range(x, na.rm = TRUE)
    pseq <- seq(xr[1], xr[2], length.out = length.out)
    return(pseq)
  }else{
    if (is.factor(x)){
      pseq <- levels(x)
      return(pseq)
    }else{
      stop("plotSeq can only create plotting sequences for numeric or factor variables")
    }
  }
}
NULL


##' Select most frequently occurring values from numeric or categorical variables.
##'
##' The "n" most frequently occurring values are returned, sorted by
##' frequency of occurrence (in descending order). The names attribute
##' includes information about the percentage of cases that have the
##' indicated values.
##'
##' This is used by plotSlopes, plotCurves, and other "newdata" making
##' functions.
##' @param x A numeric or character variable
##' @param n The maximum number of values that may be returned.
##' @return A named vector.
##' @author Paul E. Johnson <pauljohn@@ku.edu>
cutByTable <- function(x, n = 5) {
    table1 <- table(x)
    n <- min(n, length(names(table1)))
    table1 <- sort(table1, decreasing = T)
    table1cut <- table1[1:n]
    tabNames <- names(table1cut)
    res <- if(!is.factor(x)){
        as.numeric(tabNames)
    }else{
        as.factor( tabNames)
    }
    freq <- 100*round(table1cut/sum(table1),1)
    names(res) <- paste(tabNames," (",freq,"%)", sep="")
    res
}
NULL


## TODO:2012-04-23 now this only supplies the median, median+/-25.
## It ignores n, should fix.

##' Calculates the "center" quantiles, always including the median, when n is odd.
##'
##' If the numeric variable has fewer than 6 unique observed values,
##' this will send the data to cutByTable.
##' The default return will find dividing points at three quantiles:
##'            c(0.25, 0.50, 0.75)
##' If n=4, the dividing points will be c(0.20, 0.40, 0.60, 0.80)
##' If n=5, c(0.10, 0.30, 0.50, 0.70, 0.90)
##' Larger n that are odd will include 0.5 and evenly spaced points
##' out to 0 and 1. Larger n that is even will return evenly spaced
##' points calculated by R's \code{pretty} function.
##' @param x A numeric vector.
##' @param n The number of quantile points. See details.
##' @return A vector
##' @author Paul E. Johnson <pauljohn@@ku.edu>
cutByQuantile <- function(x, n = 3){
    uniqueVals <- unique(x)
    if (length(uniqueVals) < 6) {
        qs <- cutByTable(x, n)
        invisible(qs)
    } else {
        cutVector <- if(n == 1){
            c(0.50)
        }else if(n == 2){
            c(0.25, 0.75)
        }else if(n == 3){
            c(0.25, 0.50, 0.75)
        }else if(n == 4){
            c(0.20, 0.40, 0.60, 0.80)
        }else if(n == 5){
            c(0.10, 0.30, 0.50, 0.70, 0.90)
        }else if(n > 5) {
            if(n %% 2 == 0) {
                g <- 0.5 / n %/% 2
                c(seq(0, 0.5-g, by = g), 0.5, seq(0.5+g, 1.0, by = g))
            } else {
                pretty(c(0, 1), n = n-1)
            }
        }
        qs <- quantile(x, probs = cutVector, na.rm = TRUE)
        invisible(qs)
    }
}


##' Returns center values of x, the mean, mean-std.dev, mean+std.dev
##'
##' If the numeric variable has fewer than 6 unique observed values,
##' this will send the data to cutByTable.
##' @return A named vector
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @param x A numeric variable
##' @param n Should be 1, 3 or 5. If 2 < n < 5, values that divide
##' the data at c(m-sd, m, m+sd) are returned. If n > 4, the
##' returned values are c(m-2sd, m-sd, m, m+sd, m+2sd).
cutBySD <- function(x, n = 3){
    uniqueVals <- unique(x)
    if (length(uniqueVals) < 6) {
        qs <- cutByTable(x, n)
        invisible(qs)
    } else {
        mx <- round(mean(x, na.rm = TRUE),2)
        sdx <- round(sd(x, na.rm = TRUE),2)
        if (n == 1){
            qs <- c( mx )
            suffix <- c("(m)")
        } else if (n <= 4) {
            qs <- c(mx - sdx, mx, mx + sdx)
            suffix <- c("(m-sd)", "(m)", "(m+sd)")
        } else {
            qs <- c(mx - 2*sdx, mx - sdx, mx, mx + sdx, mx + 2*sdx)
            suffix <- c("(m-2sd)","(m-sd)","(m)","(m+sd)","(m+2sd)")
        }
        names(qs) <-  paste(suffix)
        invisible(qs)
    }
}





##' Select focal values from an observed variable.
##'
##' This is a generic function with 2 methods, getFocal.default
##' handles numeric variables, while getFocal.factor handles
##' factors. No other methods have been planned for preparation.
##'
##' This is used in functions like \code{plotSlopes} or
##' \code{plotCurves}.
##'
##' @param x Required. A numeric variable
##' @param xvals Optional. Optional. \code{xvals} can be a vector, an
##' algorithm name, or a function. If \code{xvals} is not provided, a
##' default method will be selected ("quantile" is default for numeric
##' variables, "table" is default for factors).
##' @param n Optional. The number of focal values to be returned
##' @return A vector.
##' @rdname getFocal
##' @export
##' @author Paul E. Johnson <pauljohn@@ku.edu>

getFocal <-
    function(x, xvals = NULL, n = 3)
{
    UseMethod("getFocal")
}

NULL


##' @return A named vector of values.
##' @export
##' @rdname getFocal
##' @author <pauljohn@@ku.edu>
##' @method getFocal default
##' @S3method getFocal default
##' @examples
##' x <- rnorm(100)
##' getFocal(x)
##' getFocal(x, xvals = "quantile")
##' getFocal(x, xvals = "quantile", n = 5)
##' getFocal(x, xvals = "std.dev")
##' getFocal(x, xvals = "std.dev", n = 5)
##' getFocal(x, xvals = c(-1000, 0.2, 0,5))
##'
getFocal.default <- function(x, xvals = NULL, n = 3)
{
    xRange <- magRange(range(x, na.rm = TRUE), 1.1)

    if (is.null(xvals))
        return(xfocal <- rockchalk:::cutByQuantile(x, n))

    if (is.numeric(xvals)) {
        if ((xvals > max(xRange)) || (xvals < min(xRange))){
            warning("values requested out of observed range in getFocal")
        }
        xvals <- sort(xvals)
        return(xvals)
    }

    if (is.character(xvals)) {
        xvals <- match.arg(tolower(xvals),
                           c("quantile", "std.dev.","table"))
        xfocal <- switch(xvals,
                         table = rockchalk:::cutByTable(x, n),
                         quantile = rockchalk:::cutByQuantile(x, n),
                         "std.dev." = rockchalk:::cutBySD(x, n),
                         stop("unknown 'xvals' algorithm in getFocal"))
        return(xfocal)
    }
    stop("getFocal received unexpected input for xvals")
}

NULL


##' @return A named vector of values.
##' @rdname getFocal
##' @export
##' @author <pauljohn@@ku.edu>
##' @method getFocal factor
##' @S3method getFocal factor
##' @examples
##' x <- factor(c("A","B","A","B","C"))
##' getFocal(x)
getFocal.factor <- function(x, xvals = NULL, n = 3)
{
    if (is.null(xvals)) {
        xvals <- rockchalk:::cutByTable(x, n)
        return(xvals)
    }
   if (is.vector(xvals)) {
       if (!all(xvals %in% levels(x))) stop("xvals includes non-observed levels of x")
       return(xvals)
    }
    if (is.function(xvals)) {
        xvals <- xvals(x, n)
        return(xvals)
    }

    if (is.character(x)) {
        xvals <- match.arg(tolower(xvals),
                           c("table"))
        xvals <- switch(xvals,
                        table = rockchalk:::cutByTable(x, n),
                        stop("Sorry, only known algorithm for factors is 'table'"))
        return(xvals)
    }
    stop("getFocal received unexpected input xvals")
}


