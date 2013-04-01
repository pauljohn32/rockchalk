##' Assists creation of predicted value curves for regression models.
##'
##'
##' Creates a predicted value plot that includes a separate predicted
##' value line for each value of a focal variable. The x axis variable
##' is specified by the \code{plotx} argument. As of rockchalk 1.7.x,
##' the moderator argument, modx, is optional. Think of this a new
##' version of R's \code{termplot}, but it allows for
##' interactions. And it handles some nonlinear transformations more
##' gracefully than termplot.
##'
##' This is similar to \code{plotSlopes}, but it accepts regressions
##' in which there are transformed variables, such as "log(x1)".
##' It creates a plot of the predicted dependent
##' variable against one of the numeric predictors, \code{plotx}. It
##' draws a predicted value line for each value of \code{modx}, a
##' moderator variable. The moderator may be a numeric or categorical
##' moderator variable.
##'
##' The user may designate which particular values of the moderator
##' are used for calculating the predicted value lines.  That is,
##' \code{modxVals = c( 12,22,37)} would draw lines for values 12, 22,
##' and 37 of the moderator. User may instead supply a character
##' string to choose one of the built in algorithms. The default
##' algorithm is "quantile", which will select \code{n} values that
##' are evenly spaced along the \code{modx} axis. The algorithm
##' "std.dev" will select the mean of \code{modx} (m) and then it will
##' select values that step away from the mean in standard deviation
##' sd units. For example, if \code{n = 3}, the focal
##' values will \code{m, m - sd, am + sd}.
##'
##' @param model Required. Fitted regression object. Must have a
##' predict method
##' @param plotx Required. String with name of predictor for the x axis
##' @param modx Optional. String for moderator variable name. May be
##' either numeric or factor.
##' @param n Optional.  Number of focal values of \code{modx}, used by
##' algorithms specified by modxVals; will be ignored if modxVals
##' supplies a vector of focal values.
##' @param modxVals Optional. A vector of focal values for which
##' predicted values are to be plotted. May also be a character string
##' to select an algorithm ("quantile","std.dev." or "table"), or a
##' user-supplied function to select focal values (a new method
##' similar to \code{getFocal}). If modx is a factor, currently, the
##' only available algorithm is "table" (see \code{getFocal.factor}.
##' @param interval Optional. Intervals provided by the
##' \code{predict.lm} may be supplied, either "conf" (95% confidence
##' interval for the estimated conditional mean) or "pred" (95%
##' interval for observed values of y given the rest of the model).
##' @param plotPoints Optional. TRUE or FALSE: Should the plot include
##' the scatterplot points along with the lines.
##' @param plotLegend Optional. TRUE or FALSE: Should the default legend be included?
##' @param col Optional.  A color vector to differentiate the moderator
##' values in the plot. If not specified, the R's builtin palette()
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
    function (model, plotx, modx, n, modxVals = NULL,
              interval = c("none", "confidence", "prediction"),
              plotPoints = TRUE, plotLegend = TRUE,
              col, llwd, envir = environment(formula(model)), ...)
{
    if (missing(model))
        stop("plotCurves requires a fitted regression model.")
    if (missing(plotx))
        stop("plotCurves requires the name of the variable to be drawn on the x axis")

    cl <- match.call()
    interval <- match.arg(interval)
    mf <- model.frame(model)
    emf <- model.data(model)

    plotxVar <- emf[ , plotx]
    if (!is.numeric(plotxVar))
        stop(paste("plotCurves: The variable", plotx, "should be a numeric variable"))


    depVar <- model.response(mf)

    ylab <- names(mf)[1]  ## returns transformed DV

    plotyRange <- magRange(depVar, mult = c(1,1.2))
    plotxRange <- range(plotxVar, na.rm = TRUE)
    plotxVals <- plotSeq(plotxRange, length.out = 40)


    if (missing(modx) || is.null(modx)){
        focalVals <- list(plotxVals)
        names(focalVals) <- c(plotx)
    } else {
        modxVar <- emf[ , modx]
        if (is.factor(modxVar)) { ## modxVar is a factor
            n <- ifelse(missing(n), nlevels(modxVar), n)
            modxVals <- getFocal(modxVar, xvals = modxVals, n)
        } else {
            n <- ifelse(missing(n), 3, n)
            modxVals <- getFocal(modxVar, xvals = modxVals, n)
        }

        focalVals <- list(modxVals, plotxVals)
        names(focalVals) <- c(modx, plotx)
    }

    newdf <- newdata(model, fl = focalVals, emf = emf)

    if (interval != "none") {
        np <- predict(model, newdata = newdf, interval = interval)
        newdf <- cbind(newdf, np)
    } else {
        newdf$fit <- predict(model, newdata = newdf)
    }

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


    dotargs <- list(...)

    parms <- list(plotxVar, depVar, xlab = plotx, ylab = ylab,
                  ylim = plotyRange, type = "n")
    parms <- modifyList(parms, dotargs)
    do.call("plot", parms)


    if (plotPoints){
        parms <- list(x = plotxVar, y = depVar, xlab = plotx, ylab = ylab,
                      ylim = plotyRange, cex = 0.5, lwd = 0.2)
        if (exists("modxVar") && is.factor(modxVar)) {
            parms[["col"]] <- col[as.numeric(modxVar)]
        }
        parms <- modifyList(parms, dotargs)
        do.call("points", parms)
    }


    ##iCol: rgb color matrix. Why does rgb insist the columns be
    iCol <- col2rgb(col)
    ### bCol: border color
    bCol <-  mapply(rgb, red = iCol[1,], green = iCol[2,], blue = iCol[3,], alpha = 50, maxColorValue = 255)
    ### sCol: shade color
    sCol <-  mapply(rgb, red = iCol[1,], green = iCol[2,], blue = iCol[3,], alpha = 15, maxColorValue = 255)

    if (interval != "none") {
        for (i in 1:lmx) {
            if(missing(modx) || is.null(modx)) {
                pdat <- newdf
            } else {
                pdat <- newdf[newdf[ , modx] %in% modxVals[i], ]
            }
            parms <- list(x = c(pdat[, plotx], pdat[NROW(pdat):1 , plotx]), y = c(pdat$lwr, pdat$upr[NROW(pdat):1]), lty = i)
            parms <- modifyList(parms, dotargs)
            parms <- modifyList(parms, list(border = bCol[i], col = sCol[i], lwd = 0.3* llwd[i]))
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

    if (plotLegend){
        lty <- 1:lmx
        if (missing(modx) || is.null(modx)){
            titl <- "Regression analysis"
            legnd <- c("Predicted values")
            if (interval != "none") {
                legnd[2] <- paste("95%", interval, "interval")
                col <- c(col, 0)
                lty <- c(lty, 0)
                llwd <- c(llwd, 0)
            }
        } else if (is.null(names(modxVals))) {
            titl <- paste("Moderator:", modx)
            legnd <- paste(modxVals, sep = "")
        } else {
            titl <- paste("Moderator:", modx)
            legnd <- paste(names(modxVals), sep = "")
        }
        legend("topleft", legend = legnd, lty = lty, col = col,
               lwd = llwd, bg = "white", title = titl)
    }
    z <- list(call = cl, newdata = newdf, modxVals = modxVals, col = col)

    class(z) <- "rockchalk"

    invisible(z)
}
