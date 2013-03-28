##' Plot predicted values for focal values of a moderator variable.
##'
##' This is a "simple slope" plotter for linear regression objects
##' that are created by \code{lm}.  The term "simple slopes" was
##' coined by psychologists (Aiken and West, 1991; Cohen, et al 2002)
##' to refer to analysis of interaction effects for particular values
##' of a moderating variable. The moderating variable may be
##' continuous or categorical, lines will be plotted for focal values
##' of that variable.
##'
##' This function works well with lm models in which the predictor
##' formula includes interactions, but it does not work well with
##' nonlinear predictors (log(x) and poly(x)).  For that, please use
##' \code{plotCurves}. plotSlopes is needed only when one wants to
##' create an output object that can be used as in put for
##' \code{testSlopes}.
##'
##' The argument \code{plotx} is the name of the horizontal plotting
##' variable; it must be numeric.  The argument \code{modx} is the
##' moderator variable. It may be either a numeric or a factor
##' variable.
##'
##' There are many ways to specify focal values using the arguments
##' \code{modxVals} and \code{n}. This changed in rockchalk-1.7.0.  If
##' modxVals is a vector of values, then the argument \code{n} is
##' ignored.  However, if modxVals is one of the name of one of the
##' algorithms, "table", "quantile", or "std.dev.", then the argument
##' \code{n} sets number of focal values to be selected.  For numeric
##' \code{modx}, n defaults to 3, but for factors \code{modx} will be
##' the number of observed values of \code{modx}. If modxVals is
##' omitted, the defaults will be used ("table" for factors,
##' "quantile" for numeric variables).
##'
##' For the predictors besides \code{modx} and \code{plotx} (the ones
##' that are not explicitly included in the plot), predicted values
##' are calculated with variables se to the mean and mode, for numeric
##' or factor variables (respectively). Those values can be reviewed
##' in the newdata object that is created as a part of the output from
##' this function
##'
##' @param model Required. Fitted regression object. Must have a
##' predict method
##' @param plotx Required. String with name of predictor to be plotted
##' on horizontal axis
##' @param modx Required. String for moderator variable name. May be
##' either numeric or factor.
##' @param n Optional. Number of focal values of
##' \code{modx}, used by algorithms specified by modxVals; will be
##' ignored if modxVals supplies a vector of focal values.
##' @param modxVals Optional. Focal values of \code{modx} for which
##' lines are desired. May be a vector of values or the name of an
##' algorithm, "quantile", "std.dev.", or "table".
##' @param interval Optional. Intervals provided by the
##' \code{predict.lm} may be supplied, either "conf" (95% confidence
##' interval for the estimated conditional mean) or "pred" (95%
##' interval for observed values of y given the rest of the model).
##' @param plotPoints Optional. TRUE or FALSE: Should the plot include
##' the scatterplot points along with the lines.
##' @param plotLegend Optional. TRUE or FALSE: Include a default
##' legend. Set to FALSE if user wants to customize a legend after the
##' plot has been drawn.
##' @param col Optional. A color vector for predicted value lines (and
##' intervals if requested). If not specified, the R's builtin palette()
##' will be used. User may supply a vector of valid color names,
##' either explicitly c("pink","black", "gray70") or implicitly,
##' rainbow(10) or gray.colors(5). Color names will be recycled if there
##' are more focal values of \code{modx} than colors provided.
##' @param llwd Optional. Line widths for predicted values. Can be
##' single value or a vector, which will be recycled as necessary.
##' @param ... further arguments that are passed to plot. Any
##' arguments that customize plot output, such as lwd, cex, and so
##' forth, may be supplied.
##' @export
##' @import car
##' @return The plot is drawn on the screen, and the return object
##' includes the "newdata" object that was used to create the plot,
##' along with the "modxVals" vector, the values of the moderator for
##' which lines were drawn. It also includes the call that generated
##' the plot.
##' @seealso plotCurves and testSlopes
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @references
##' Aiken, L. S. and West, S.G. (1991). Multiple Regression: Testing and Interpreting Interactions. Newbury Park, Calif: Sage Publications.
##'
##' Cohen, J., Cohen, P., West, S. G., and Aiken, L. S. (2002). Applied Multiple Regression/Correlation Analysis for the Behavioral Sciences (Third.). Routledge Academic.
##' @example inst/examples/plotSlopes-ex.R

plotSlopes <-
  function (model, plotx, modx, n = 3, modxVals = NULL ,
            interval = c("none", "confidence", "prediction"),
            plotPoints = TRUE, plotLegend = TRUE, col, llwd, ...)
{
    if (missing(model))
        stop("plotSlopes requires a fitted regression model.")
    if (missing(plotx))
        stop("plotSlopes requires the name of the variable to be drawn on the x axis")

    cl <- match.call()
    mm <- model.matrix(model)
    depVar <- model$model[, 1]
    plotxVar <- model$model[, plotx]
    if (!is.numeric(plotxVar))
        stop(paste("plotSlopes: The variable", plotx, "should be a numeric variable"))
    ylab <- colnames(model$model)[1]
    plotyRange <- magRange(depVar, mult = c(1, 1.1))
    plotxRange <- range(mm[, plotx], na.rm = TRUE)

    plotxVals <- plotSeq(plotxRange, length.out = 40)

    ## Create focalVals object, needed by newdata
    if (missing(modx) || is.null(modx)){
        if (missing(interval)) {
            focalVals <- list(plotxRange)
        } else {
            focalVals <- list(plotxVals)
        }
        names(focalVals) <- c(plotx)
    } else {
        modxVar <- model$model[, modx]
        if (is.factor(modxVar)) { ## modxVar is a factor
            n <- ifelse(missing(n), nlevels(modxVar), n)
            modxVals <- getFocal(modxVar, xvals = modxVals, n)
        } else {
            n <- ifelse(missing(n), 3, n)
            modxVals <- getFocal(modxVar, xvals = modxVals, n)
        }

        ## if no interval plot requested, we only need 2 points from plotx
        ## to plot lines
        if (missing(interval)) {
            focalVals <- list(modxVals, plotxRange)
        } else {
            focalVals <- list(modxVals, plotxVals)
        }

        names(focalVals) <- c(modx, plotx)
    }

    newdf <- newdata(model, fl = focalVals)

    if (!missing(interval)) {
        np <- predict(model, newdata = newdf, interval = interval)
        newdf <- cbind(newdf, np)
    } else {
        newdf$fit <- predict(model, newdata = newdf)
    }

    dotargs <- list(...)

    ## Now begin the plotting work.
    if (missing(modx) || is.null(modx)) {
        lmx <- 1
    } else {
        lmx <- length(modxVals)
    }

    if (missing(col)) col <- 1:lmx
    if (length(col) < lmx) col <- rep(col, length.out = lmx)
    if (missing(llwd)) llwd <- 2
    if (length(llwd) < lmx) llwd <- rep(llwd, length.out = lmx)

    parms <- list(mm[, plotx], depVar, xlab = plotx, ylab = ylab,
                  ylim = plotyRange, type = "n")
    parms <- modifyList(parms, dotargs)

    do.call("plot", parms)

    if (!missing(interval)) {
        for (i in 1:lmx) {
            nCol <- col2rgb(col[i])
            if(missing(modx) || is.null(modx)) {
                pdat <- newdf
            } else {
                pdat <- newdf[newdf[ , modx] %in% modxVals[i], ]
            }
            parms <- list(x = c(pdat[, plotx], pdat[NROW(pdat):1 , plotx]), y = c(pdat$lwr, pdat$upr[NROW(pdat):1]), lty = i)
            parms <- modifyList(parms, dotargs)
            parms <- modifyList(parms, list(border = rgb(red = t(nCol), alpha = 50, maxColorValue = 255), col = rgb(red = t(nCol), alpha = 15, maxColorValue = 255), lwd = 0.3* llwd[i]))
            do.call("polygon", parms)
        }
    }

    for (i in 1:lmx) {
        if(missing(modx) || is.null(modx)) {
            pdat <- newdf
        } else {
            pdat <- newdf[newdf[ , modx] %in% modxVals[i], ]
        }
        parms <- list(x = pdat[, plotx], y = pdat$fit, lty = i)
        parms <- modifyList(parms, dotargs)
        parms <- modifyList(parms, list(col = col[i], lwd = llwd[i]))
        do.call("lines", parms)
    }

    if (plotPoints){
        parms <- list(x = mm[, plotx], y = depVar, xlab = plotx, ylab = ylab,
                      cex = 0.5, lwd = 0.2)
        if (exists("modxVar") && is.factor(modxVar)) {
            parms[["col"]] <- col[as.numeric(modxVar)]
        }
        parms <- modifyList(parms, dotargs)
        do.call("points", parms)
    }

    if (plotLegend){
        if (missing(modx) || is.null(modx)){
            titl <- "Regression Analysis"
            legnd <- c("Predicted Values")
            if (!missing(interval)) legnd[2] <- paste(interval, "interval")
        } else if (is.null(names(modxVals))) {
            titl <- paste("Moderator:", modx)
            legnd <- paste(modxVals, sep = "")
        } else {
            titl <- paste("Moderator:", modx)
            legnd <- paste(names(modxVals), sep = "")
        }
        legend("topleft", legend = legnd, lty = 1:lmx, col = col,
               lwd = llwd, bg = "white", title = titl)
    }
    z <- list(call = cl, newdata = newdf, modxVals = modxVals, col = col)
    class(z) <- "rockchalk"

    invisible(z)
}
