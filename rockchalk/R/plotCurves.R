##' Assists creation of predicted value curves for regression models.
##'
##'
##' This is similar to \code{plotSlopes}, but it accepts regressions
##' in which there are transformed variables, such as "log(x1)".
##' Think of this a new version of R's \code{termplot}, but it allows
##' for interactions.  It creates a plot of the predicted dependent
##' variable against one of the numeric predictors, \code{plotx}. It
##' draws a predicted value line for several values of \code{modx}, a
##' moderator variable. The moderator may be a numeric or categorical
##' moderator variable.
##'
##' The user may designate which particular values of the moderator
##' are used for calculating the predicted value lines.   That is,
##' \code{modxVals = c( 12,22,37)} would draw lines for values 12, 22,
##' and 37 of the moderator.
##'
##' If the user does not specify the parameter \code{modxVals},
##' built-in algorithms will select the "focal points". Three algorithms
##' have been prepared so far, \code{quantile}, \code{std.dev.}, and
##' \code{table}. If the number of unique observed values is smaller
##' than 6, the \code{table} method is used.  The \code{n} most frequently
##' observed values of modx are selected. Otherwise, the quantile
##' method is used. Predictive lines are plotted for the following
##' percentiles {0.25,0.50,0.75}. The algorithm \code{std.dev.} plots
##' three lines, one for the mean of modx, and one for the mean minus
##' one standard deviation, and the other for the mean plus one
##' standard deviation.
##'
##'
##' @param model Required. Fitted regression object. Must have a
##' predict method
##' @param plotx Required. String with name of IV to be plotted on x axis
##' @param modx Required. String for moderator variable name. May be
##' either numeric or factor.
##' @param n Optional.  Number of focal values of \code{modx}, used by
##' algorithms specified by modxVals; will be ignored if modxVals
##' supplies a vector of focal values.  ##' @param modxVals
##' Optional. If modx is numeric, either a vector of values, or a
##' character string to select an algorithm ("quantile","std.dev." or
##' "table"), or a user-supplied function to select focal values (a
##' new method similar to \code{getFocal}). If modx is a factor, may
##' be a vector of valid levels of \code{modx}, a function, or an
##' algorithm name. Currently, the only available algorithm is "table"
##' (see \code{getFocal.factor}.
##' @param interval Optional. Intervals provided by the
##' \code{predict.lm} may be supplied, either "conf" (95% confidence
##' interval for the estimated conditional mean) or "pred" (95%
##' interval for observed values of y given the rest of the model).
##' @param plotPoints Optional. TRUE or FALSE: Should the plot include
##' the scatterplot points along with the lines.
##' @param col Optional.  A color vector for predicted value lines (and
##' intervals if requested). If not specified, the R's builtin palette()
##' will be used. User may supply a vector of valid color names,
##' either explicitly c("pink","black", "gray70") or implicitly,
##' rainbow(10) or gray.colors(5). Color names will be recycled if there
##' are more focal values of \code{modx} than colors provided.
##' @param envir environment to search for variables.
##' @param llwd Optional. Line widths for predicted values. Can be
##' single value or a vector, which will be recycled as necessary.
##' @param ... further arguments that are passed to plot.
##' @export
##' @import car
##' @return A plot is created as a side effect, a list is returned including
##' 1) the call, 2) a newdata object that includes information on the curves that were
##' plotted, 3) a vector modxVals, the values for which curves were drawn.
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @example  inst/examples/plotCurves-ex.R
plotCurves <-
    function (model, plotx, modx, n, modxVals = NULL, interval,
              plotPoints = TRUE, col, llwd, envir = environment(formula(model)), ...)
{
    if (missing(model))
        stop("plotCurves requires a fitted regression model.")
    if (missing(plotx))
        stop("plotCurves requires the name of the variable to be drawn on the x axis")
    if (missing(modx))
        stop("plotCurves requires the name of moderator variable for which several slopes are to be drawn")

    cl <- match.call()
    mf <- model.frame(model)
    emf <- model.data(model)

    plotxVar <- emf[ , plotx]
    if (!is.numeric(plotxVar))
        stop(paste("plotCurves: The variable", plotx, "should be a numeric variable"))

    modxVar <- emf[ , modx]
    depVar <- model.response(mf)

    ylab <- names(mf)[1]  ## returns transformed DV

    plotyRange <- magRange(depVar, mult = c(1,1.2))
    plotxRange <- range(plotxVar, na.rm = TRUE)
    plotxVals <- plotSeq(plotxRange, length.out = 40)


    ##copied from new plotSlopes.
    ##TODO: Abstract this LATER
    if (is.factor(modxVar)) { ## modxVar is a factor
        n <- ifelse(missing(n), nlevels(modxVar), n)
        modxVals <- getFocal(modxVar, xvals = modxVals, n)
    } else {
        n <- ifelse(missing(n), 3, n)
        modxVals <- getFocal(modxVar, xvals = modxVals, n)
    }


    ## Again, adapt from new plotSlopes
    focalVals <- list(modxVals, plotxVals)
    names(focalVals) <- c(modx, plotx)
    newdf <- newdata(model, fl = focalVals, emf = emf)


    ## predictors <- attr(emf, "varNamesRHS")
    ## predictors <- setdiff(predictors, c(modx, plotx))
    ## newdf <- data.frame(expand.grid(plotxSeq, modxVals))
    ## colnames(newdf) <- c(plotx, modx)
    ## if (length(predictors) > 0) {
    ##     newdf <- cbind(newdf, centralValues(emf[, predictors, drop = FALSE]))
    ##     colnames(newdf) <- c(plotx, modx, predictors)
    ## }


    if (!missing(interval)) {
        np <- predict(model, newdata = newdf, interval = interval)
        newdf <- cbind(newdf, np)
    } else {
        newdf$fit <- predict(model, newdata = newdf)
    }

    ## Now begin the plotting work.
    lmx <- length(modxVals)
    if (missing(col)) col <- 1:lmx
    if (length(col) < lmx) col <- rep(col, length.out = lmx)
    if (missing(llwd)) llwd <- 2
    if (length(llwd) < lmx) llwd <- rep(llwd, length.out = lmx)


    dotargs <- list(...)

    parms <- list(plotxVar, depVar, xlab = plotx, ylab = ylab,
                  ylim = plotyRange, type = "n")
    parms <- modifyList(parms, dotargs)
    do.call("plot", parms)


    if (plotPoints){
        parms <- list(plotxVar, depVar, xlab = plotx, ylab = ylab,
                      ylim = plotyRange, col = col, cex = 0.5, lwd = 0.2)
        if (is.factor(modxVar))  parms[["col"]] <- col
        parms <- modifyList(parms, dotargs)
        do.call("points", parms)
    }

    if (!missing(interval)){
        for (i in 1:lmx) {
            nCol <- col2rgb(col[i])
            pdat <- newdf[newdf[, modx] %in% modxVals[i], ]
            parms <- list(x = c(pdat[, plotx], pdat[NROW(pdat):1 , plotx]), y = c(pdat$lwr, pdat$upr[NROW(pdat):1]), lty = i)
            parms <- modifyList(parms, dotargs)
            parms <- modifyList(parms, list(border = rgb(red = t(nCol), alpha = 50, maxColorValue = 255), col = rgb(red = t(nCol), alpha = 15, maxColorValue = 255), lwd = 0.3* llwd[i]))
            do.call("polygon", parms)
        }
    }


    for (i in 1:lmx) {
        pdat <- newdf[newdf[, modx] %in% modxVals[i], ]
        parms <- list(x = pdat[, plotx], y = pdat$fit, lty = i)
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
    legend("topleft", legend = legnd, lty = 1:lmx, col = 1:lmx,
           bg = "white", title= paste("moderator:", modx))

    z <- list(call=cl, newdata=newdf, modxVals = modxVals)
    class(z) <- "rockchalk"

    invisible(z)
}
