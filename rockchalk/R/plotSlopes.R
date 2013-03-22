##' Assists creation of predicted value lines for values of a moderator variable.
##'
##' This is a "simple slope" plotter for linear regression.  The term
##' "simple slopes" was coined by psychologists (Aiken and West, 1991;
##' Cohen, et al 2002) to refer to analysis of interaction effects for
##' particular values of a moderating variable, be it continuous or
##' categorical. To use this function, the user should estimate a
##' regression (with as many variables as desired, including
##' interactions) and the resulting regression object is then supplied
##' to this function, along with user requests for plots of certain
##' variables.
##'
##' The variable \code{plotx} will be the horizontal plotting
##' variable; it must be numeric.  The variable \code{modx} is the
##' moderator variable. It may be either a numeric or a factor
##' variable.  A line will be drawn to represent the
##' predicted value for selected values of the moderator.
##'
##' The parameter \code{modxVals} is optional.  It is used to
##' fine-tune the values of the moderator that are used to create the
##' simple slope plot.  Numeric and factor moderators are treated
##' differently. If the moderator is a numeric variable, then some
##' particular values must be chosen for plottings. If the user does
##' not specify the parameter \code{modxVals}, then lines will be
##' drawn for the quantile values of the moderator.  If the moderator
##' is a factor, then lines are drawn for each different value of the
##' factor variable, unless the user specifies a subset of levels with
##' the \code{modxVals} parameter.
##'
##' For numeric moderators, the user may specify a vector of values
##' for the numeric moderator variable, such as c(1,2,3). The user may
##' also specify an algorithm, either "quantile" (which would be
##' selected by default) or "std.dev." The alternative method at this
##' time is "std.dev.", which causes 5 lines to be drawn. These lines
##' are the "standard deviations about the mean of \code{modx}" lines,
##' at which modx is set at mean - k* standard deviation, and k takes
##' on values -2, -1, 0, 1, 2.
##'
##' Here is a wrinkle. There can be many variables in a regression
##' model, and we are plotting only for the \code{plotx} and
##' \code{modx} variables. How should we calculate predicted values
##' when the values of the other variables are required?  For the
##' other variables, the ones that are not explicitly inlcluded in the
##' plot, we use the mean and mode, for numeric or factor variables
##' (respectively). Those values can be reviewed in the newdata object
##' that is created as a part of the output from this function
##'
##' @param model Required. Fitted regression object. Must have a predict method
##' @param plotx Required. String with name of IV to be plotted on x axis
##' @param modx  Required. String for moderator variable name. May be either numeric or factor.
##' @param modxVals Optional. If modx is numeric, either a character
##' string, "quantile", "std.dev.", or "table", or a vector of values
##' for which plotted lines are sought. If modx is a factor, the
##' default approach will create one line for each level, but the user
##' can supply a vector of levels if a subset is desired..
##' @param plotPoints Optional. TRUE or FALSE: Should the plot include the scatterplot points along with the lines.
##' @param plotLegend Optional. TRUE or FALSE: Include a default legend. Set to FALSE if use wants to run a different legend command after the plot has been drawn.
##' @param col Optional. A color vector.  By default, the R's builtin colors will be used,  which are "black", "red", and so forth.  Instead, a vector of color names can be supplied, as in c("pink","black", "gray70").  A color-vector generating function like rainbow(10) or gray.colors(5) can also be used. A vector of color names can be supplied with this function. Color names will be recycled if the plot requires more different colors than the user provides.
##' @param llwd An optional vector of line widths used while plotting the lines that represent the values of the factor. This applies only to the lines in the plot. The ... argument will also allow one to pass options that are parsed by plot, such as lwd. That deterimine the thickness of points in the plot.
##' @param ... further arguments that are passed to plot
##' @export
##' @import car
##' @return The plot is drawn on the screen, and the return object includes the "newdata" object that was used to create the plot, along with the "modxVals" vector, the values of the moderator for which lines were drawn. It also includes the call that generated the plot.
##' @seealso plotCurves and testSlopes
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @references
##' Aiken, L. S. and West, S.G. (1991). Multiple Regression: Testing and Interpreting Interactions. Newbury Park, Calif: Sage Publications.
##'
##' Cohen, J., Cohen, P., West, S. G., and Aiken, L. S. (2002). Applied Multiple Regression/Correlation Analysis for the Behavioral Sciences (Third.). Routledge Academic.
##' @example inst/examples/plotSlopes-ex.R

plotSlopes <-
  function (model = NULL, plotx = NULL, modx = NULL, modxVals = NULL,
            plotPoints = TRUE, plotLegend = TRUE, col, llwd, ...)
{
    if (is.null(model))
        stop("plotSlopes requires a fitted regression model.")
    if (is.null(plotx))
        stop("plotSlopes requires the name of the variable to be drawn on the x axis")
    if (is.null(modx))
        stop("plotSlopes requires the name of moderator variable for which several slopes are to be drawn")

    cl <- match.call()
    mm <- model.matrix(model)
    depVar <- model$model[, 1]
    modxVar <- model$model[, modx]
    plotxVar <- model$model[, plotx]
    if (!is.numeric(plotxVar))
        stop(paste("plotSlopes: The variable", plotx, "should be a numeric variable"))
    ylab <- colnames(model$model)[1]
    plotyRange <- magRange(depVar, mult=c(1,1.2))
    plotxRange <- range(mm[, plotx], na.rm = TRUE)
    plotxSeq <- plotSeq(plotxRange, length.out = 40)

    if (is.factor(modxVar)) { ## modxVar is a factor
        if (is.null(modxVals)) {
            modxVals <- levels(modxVar)
        } else {
            if (!all(modxVals %in% levels(modxVar))) stop("modxVals includes non-observed levels of modxVar")
        }
    } else {                  ## modxVar is not a factor
        modxRange <- range(modxVar, na.rm=TRUE)
        if (is.null(modxVals)) {
            modxVals <- rockchalk:::cutByQuantile(modxVar)
        } else {
            if (is.numeric(modxVals)) {
                ;# print("TODO: Insert some checks that modxVals are reasonable")
            } else {
                if (is.character(modxVals)) {
                    modxVals <- match.arg(tolower(modxVals),
                                          c("quantile", "std.dev.","table"))
                    print(modxVals)
                    modxVals <- switch(modxVals,
                                       table = rockchalk:::cutByTable(modxVar),
                                       quantile = rockchalk:::cutByQuantile(modxVar),
                                       "std.dev." = rockchalk:::cutBySD(modxVar),
                                       stop("unknown 'modxVals' algorithm"))
                }
            }
        }
    }
    lmx <- length(modxVals)
    if (missing(col)) col <- 1:lmx
    if (length(col) < lmx) col <- rep(col, length.out = lmx)
    if (missing(llwd)) llwd <- 2
    if (length(llwd) < lmx) llwd <- rep(llwd, length.out = lmx)
    predictors <- colnames(model$model)[-1]
    predictors <- setdiff(predictors, c(modx, plotx))
    newdf <- data.frame(expand.grid(plotxRange, modxVals))
    colnames(newdf) <- c(plotx, modx)
    if (length(predictors) > 0) {
        newdf <- cbind(newdf, centralValues(as.data.frame(model$model[, predictors])))
        colnames(newdf) <- c(plotx, modx, predictors)
    }
    newdf$pred <- predict(model, newdata = newdf)
    dotargs <- list(...)

    parms <- list(mm[, plotx], depVar, xlab = plotx, ylab = ylab,
                  type = "n")
    parms <- modifyList(parms, dotargs)
    do.call("plot", parms)
    if (plotPoints){
        parms <- list(x = mm[, plotx], y = depVar, xlab = plotx, ylab = ylab,
                      cex = 0.5, lwd = 0.2)
        if (is.factor(modxVar)) {
            parms[["col"]] <- col
            parms <- modifyList(parms, dotargs)
            do.call("points", parms)
        } else {
            parms <- modifyList(parms, dotargs)
            do.call("points", parms)
        }
    }
    for (i in 1:lmx) {
        pdat <- newdf[newdf[, modx] %in% modxVals[i], ]
        parms <- list(x = pdat[, plotx], y = pdat$pred, lty = i)
        parms <- modifyList(parms, dotargs)
        parms <- modifyList(parms, list(col = col[i], lwd = llwd[i]))
        do.call("lines", parms)
    }
    if (is.null(names(modxVals))) {
        legnd <- paste(modxVals, sep = "")
    }
    else {
        legnd <- paste(names(modxVals), sep = "")
    }
    if(plotLegend) legend("topleft", legend = legnd, lty = 1:lmx, col = col, lwd = llwd,
                          bg = "white", title= paste("moderator:", modx))

    z <- list(call = cl, newdata = newdf, modxVals = modxVals)
    class(z) <- "rockchalk"

    invisible(z)
}
