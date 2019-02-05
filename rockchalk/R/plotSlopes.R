##' Generic function for plotting regressions and interaction effects
##'
##' This is a generic function for plotting regression
##' objects. So far, there is an implementation for \code{lm()} objects.
##' This allows interaction effects, but not nonlinearities like log(x1).
##' For that, please see \code{plotCurves}.
##'
##' @param model Required. A fitted Regression
##' @param plotx Required. Name of one predictor from the fitted
##' model to be plotted on horizontal axis
##' @param ... Additional arguments passed to methods. Often includes
##' arguments that are passed to plot. Any arguments that customize
##' plot output, such as lwd, cex, and so forth, may be
##' supplied. These arguments intended for the predict method will be
##' used: c("type", "se.fit", "interval", "level", "dispersion",
##' "terms", "na.action")
##' @export plotSlopes
##' @importFrom graphics abline arrows legend lines mtext plot points polygon text
##' @importFrom methods as
##' @importFrom stats AIC addmargins as.formula coef cor
##'             df.residual drop1 family fitted formula lm
##'             model.frame model.matrix model.response na.omit na.pass
##'             napredict nobs pchisq pf predict
##'             predict.lm pt qnorm qt quantile resid
##'             rnorm sd setNames terms var vcov
##' @importFrom utils browseURL methods modifyList 
##' @rdname plotSlopes
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @seealso \code{\link[rockchalk]{testSlopes}} \code{\link[rockchalk]{plotCurves}}
##' @return Creates a plot and an output object that summarizes it.
plotSlopes <- function(model, plotx, ...) UseMethod("plotSlopes")

##' Plot predicted values for focal values of a moderator variable.
##'
##' This is a "simple slope" plotter for linear regression objects
##' that are created by \code{lm()}.  The function \code{plotCurves()}
##' can handle nonlinear predictive equations and generalized linear
##' models. The term "simple slopes" was coined by psychologists
##' (Aiken and West, 1991; Cohen, et al 2002) for analysis of
##' interaction effects for particular values of a moderating
##' variable. The moderating variable may be continuous or
##' categorical, lines will be plotted for focal values of that
##' variable.
##'
##' This function works well with lm models in which the predictor
##' formula includes interactions, but it does not work well with
##' nonlinear predictors (log(x) and poly(x)).  For that, please use
##' \code{plotCurves}. plotSlopes is needed only when one wants to
##' create an output object that can be used as input for
##' \code{testSlopes()}.
##'
##' The argument \code{plotx} is the name of the horizontal plotting
##' variable; it must be numeric.  The argument \code{modx} is the
##' moderator variable. It may be either a numeric or a factor
##' variable. As of version 1.7, the modx argument may be omitted. A
##' single predicted value line will be drawn. That version also
##' introduced the arguments interval and n.
##'
##' There are many ways to specify focal values using the arguments
##' \code{modxVals} and \code{n}. This changed in rockchalk-1.7.0.  If
##' \code{modxVals} is omitted, a default algorithm will be used,
##' selecting \code{n} values for plotting. \code{modxVals} may be a
##' vector of values (for a numeric moderator) or levels (for a
##' factor).  If modxVals is a vector of values, then the argument
##' \code{n} is ignored.  However, if modxVals is one of the name of
##' one of the algorithms, "table", "quantile", or "std.dev.", then
##' the argument \code{n} sets number of focal values to be selected.
##' For numeric \code{modx}, n defaults to 3, but for factors
##' \code{modx} will be the number of observed values of
##' \code{modx}. If modxVals is omitted, the defaults will be used
##' ("table" for factors, "quantile" for numeric variables).
##'
##' For the predictors besides \code{modx} and \code{plotx} (the ones
##' that are not explicitly included in the plot), predicted values
##' are calculated with variables set to the mean and mode, for numeric
##' or factor variables (respectively). Those values can be reviewed
##' in the newdata object that is created as a part of the output from
##' this function
##'
##' @param plotxRange Optional. If not specified, the observed
##' range of plotx will be used to determine the axis range.
##' @param modx Optional. String for moderator variable name. May be
##' either numeric or factor. If omitted, a single predicted value line
##' will be drawn.
##' @param n Optional. Number of focal values of
##' \code{modx}, used by algorithms specified by modxVals; will be
##' ignored if modxVals supplies a vector of focal values.
##' @param modxVals Optional. Focal values of \code{modx} for which
##' lines are desired. May be a vector of values or the name of an
##' algorithm, "quantile", "std.dev.", or "table".
##' @param interval Optional. Intervals provided by the
##' \code{predict.lm} may be supplied, either "confidence" (confidence
##' interval for the estimated conditional mean) or "prediction"
##' (interval for observed values of y given the rest of the model).
##' The level can be specified as an argument (which goes into ...
##' and then to the predict method)
##' @param plotPoints Optional. TRUE or FALSE: Should the plot include
##' the scatterplot points along with the lines.
##' @param plotLegend Optional. TRUE or FALSE: Include a default
##' @param legendPct Default = TRUE. Variable labels print with sample percentages.
##' @param legendArgs Set as NULL if no legend is desired. Otherwise, this
##' can be a list of named arguments that will override the settings
##' I have for the legend.
##' @param col Optional. A color vector for predicted value lines (and
##' intervals if requested). If not specified, the R's builtin palette()
##' will be used. User may supply a vector of valid color names,
##' either explicitly c("pink","black", "gray70") or implicitly,
##' rainbow(10) or gray.colors(5). Color names will be recycled if there
##' are more focal values of \code{modx} than colors provided.
##' @param llwd Optional, default = 2. Line widths for predicted values. Can be
##' single value or a vector, which will be recycled as necessary.
##' @param opacity Optional, default = 100. A number between 1 and 255.
##' 1 means "transparent" or invisible, 255 means very dark.
##' Determines the darkness of confidence interval regions
##' @export
##' @method plotSlopes lm
##' @rdname plotSlopes
##' @import carData
##' @return The return object includes the "newdata" object that was
##' used to create the plot, along with the "modxVals" vector, the
##' values of the moderator for which lines were drawn, and the color
##' vector. It also includes the call that generated the plot.
##' @references
##' Aiken, L. S. and West, S.G. (1991). Multiple Regression:
##' Testing and Interpreting Interactions. Newbury Park, Calif: Sage Publications.
##'
##' Cohen, J., Cohen, P., West, S. G., and Aiken, L. S. (2002).
##' Applied Multiple Regression/Correlation Analysis for the Behavioral
##' Sciences (Third.). Routledge Academic.
##' @example inst/examples/plotSlopes-ex.R
plotSlopes.lm <-
    function (model, plotx, modx = NULL, n = 3, modxVals = NULL ,
              plotxRange = NULL, interval = c("none", "confidence", "prediction"),
              plotPoints = TRUE, legendPct = TRUE, legendArgs,
              llwd = 2, opacity = 100, ...,
              col = c("black", "blue", "darkgreen", "red", "orange", "purple", "green3"),
              type = c("response", "link"))
{
    if (missing(model))
        stop("plotSlopes requires a fitted regression model.")
    if (missing(plotx)) stop("plotSlopes: plotx argument is required")
    else if(!is.character(plotx)) plotx <-as.character(substitute(plotx))

    ## Explanation of variable names
    ## "Vals" means focal values
    ## "Var"  means observed values
    ## "plotx" and "modx" will be reduced to character for variable names
    
    cl <- match.call()
    dotargs <- list(...)
    dotnames <- names(dotargs)
    level <- if (!is.null(dotargs[["level"]])) dotargs[["level"]] else 0.95
    
    mm <- model.data(model)
    plotxVar <- mm[, plotx]

    ## ylab is name of dependent variable, 
    ylab <- as.character(terms(model)[[2]])

    interval <- match.arg(interval)
    type <- match.arg(type)
    
    if (!missing(modx)) {
        zzz <- as.character(substitute(modx))
        if(!is.character(modx)) modx <- as.character(substitute(modx))
    }

    ## gets "raw" untransformed data
    ## depVar <- model.data[ , ylab]
    ## try to get the logged or transformed value???
    ## should be same as model.response(model.frame())??
    depVar <- with(mm, eval(terms(model)[[2]]))
    
    if(is.null(plotxRange)){
        if (is.numeric(plotxVar)){
            plotxRange <- range(plotxVar, na.rm = TRUE)
        }
    }

    ## We are not allowing user to select categories (yet)
    ## TODO Check what character variable does here!
    if (is.factor(plotxVar)){
        plotxVals <- levels(droplevels(plotxVar))
    } else {
        plotxVals <- plotSeq(plotxRange, length.out = 40)
    }

    if((missing(modxVals) || is.null(modxVals)) && !(missing(modx) || is.null(modx))){
        modxVar <- mm[, modx]
        if (is.factor(modxVar)) { ## modxVar is a factor
            n <- ifelse(missing(n), nlevels(modxVar), n)
            modxVals <- getFocal(modxVar, xvals = modxVals, n, pct = legendPct)
        } else {
            modxVals <- getFocal(modxVar, xvals = modxVals, n, pct = legendPct)
        }
    } 
    
    predVals <- list(plotxVals)
    names(predVals) <- plotx
    if(!is.null(modx)) {
        if(is.null(modxVals)) modxVals <- "table"
        TEXT <- paste0("predVals[[\"", modx, "\"]] <- modxVals")
        eval(parse(text = TEXT))
    }
    
    parms.pred <- list(model, predVals = predVals, type = type, interval = interval)
    validForPredict <- c("se.fit", "dispersion", "terms", "na.action",
                         "level", "pred.var", "weights")
    dotsForPredict <- dotnames[dotnames %in% validForPredict]
    if (length(dotsForPredict) > 0) {
        parms.pred <- modifyList(parms.pred, dotargs[dotsForPredict])
        dotargs[[dotsForPredict]] <- NULL
    }
    
    newdf <-  do.call("predictOMatic", parms.pred)

    ## ignores confidence interval height here. Why?
    plotyRange <- if(is.numeric(depVar)){
        magRange(depVar, mult = c(1, 1.2))
    } else {
        stop(paste("plotSlopes: use plotCurves if this is a glm (logit, probit, count)"))
    }

    if(is.numeric(plotxVals)){
        parms <- list(newdf = newdf, olddf = mm, plotx = plotx, modx = modx,
                       modxVals = modxVals, depName = depVar, 
                       interval = interval, level = level, plotPoints = plotPoints,
                       col = col, opacity = opacity, xlim = plotxRange,
                       ylim = plotyRange, ylab = ylab, llwd = llwd)
        if(!missing(legendArgs) && !is.null(legendArgs)) parms[["legendArgs"]] <- legendArgs
        parms <- modifyList(parms, dotargs)
        plotArgs <- do.call("plotFancy", parms)
    } else {
        if (!plotPoints)
        plotyRange <- magRange(range(c(newdf[["fit"]], if(!is.null(newdf[["lwr"]])) min(newdf[["lwr"]])),
                                   if(!is.null(newdf[["upr"]])) max(newdf[["upr"]])), 1.25)
               
        parms <- list(newdf, olddf = mm,  plotx = plotx, modx = modx, 
                      modxVals = modxVals, depName = depVar, xlim = plotxRange,
                      xlab = plotx, ylab = ylab,
                      ylim = plotyRange, main = "", col = col)
        if(!missing(legendArgs) && !is.null(legendArgs)) parms[["legendArgs"]] <- legendArgs
        parms <- modifyList(parms, dotargs)
        plotArgs <- do.call("plotFancyCategories", parms)
    }
        
    z <- list(call = cl, newdata = newdf, modxVals = modxVals, col = plotArgs$col, lty = plotArgs$lty)
    class(z) <- c("plotSlopes", "rockchalk")

    invisible(z)
}
NULL

##' Regression plots with predicted value lines, confidence intervals, color coded interactions
##'
##' This is the back-end for the functions plotSlopes and plotCurves. Don't use it directly.
##'
##' @param newdf The new data frame with predictors and fit, lwr, upr
##'     variables
##' @param olddf A data frame with variables(modxVar, plotxVar,
##'     depVar)
##' @param plotx Character string for name of variable on horizontal
##'     axis
##' @param modx Character string for name of moderator variable.
##' @param modxVals Values of moderator for which lines are desired
##' @param interval TRUE or FALSE: want confidence intervals?
##' @param plotPoints TRUE or FALSE: want to see observed values in
##'     plot?
##' @param legendArgs Set as "none" for no legend. Otherwise, a list
##'     of arguments for the legend function
##' @param col requested color scheme for lines and points. One per
##'     value of modxVals.
##' @param llwd requested line width, will re-cycle.
##' @param opacity Value in 0, 255 for darkness of interval shading
##' @param ... Other arguments passed to plot function.
##' @return col, lty, and lwd information
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##'
plotFancy <-
    function(newdf, olddf, plotx, modx, modxVals, interval, plotPoints,
             legendArgs, col = NULL, llwd = 2, opacity, ...)
{
    dotargs <- list(...)
    
    ## Damn. Need ylab from dotargs explicitly
    ylab <- dotargs[["depName"]]
    if (!is.null(dotargs[["ylab"]])) ylab <- dotargs[["ylab"]]
    ## Also need level
    if (!is.null(dotargs[["level"]])) {
        level <- dotargs[["level"]]
        dotargs[["level"]] <- NULL
    } else {
        level <- 0.95
    }

    ## sort rows
    newdf <- if(is.null(modx)) newdf[order(newdf[[plotx]]), ]
                   else newdf[order(newdf[[plotx]], newdf[[modx]]), ]
    
    modxVar <- if(is.null(modx)) rep(1, NROW(olddf)) else olddf[ , modx]
    plotxVar <- olddf[ , plotx]
    depVar <- olddf[ , 1] ## column 1 is observed dv fix me here with name

    ## Now begin the plotting work.
    if (missing(modx) || is.null(modx)) {
        modxVals <- 1
        lmx <- 1
    } else {
        lmx <- length(modxVals)
    }

  
    ## if modx is a factor's name, we want to use all the levels
    ## to set the color scheme, even if some are not used in this
    ## particular plot.
    if (is.factor(modxVar)) {
        modxLevels <- levels(modxVar)
    } else {
        modxLevels <- modxVals
        if (is.null(names(modxVals))) names(modxVals) <- modxVals
    }
 
    if (length(modxLevels) != length(modxVals)) {stop("goof")}

    
    ## Deal w colors
    if (is.null(col)) {
        if (is.factor(modxVar)) {
            col <- seq_along(modxLevels)
            names(col) <- modxLevels
        }  else {
            col <- 1:lmx
            names(col) <- names(modxVals)
        }
    } else {
        if (is.null(names(col))){
            col <- rep(col, length(lmx))
            if (is.factor(modxVar)) names(col) <- modxLevels
            else names(col) <- names(modxVals)
        } else if (length(col) < lmx) {
            stop("plotFancy: named color vector does not match data.")
        } else if (length(col) > length(modxLevels)) {
            col <- rep(col, length.out = length(modxLevels))
            if (is.null(names(col))) {
                browser()
                names(col) <- names(modxLevels[1:length(col)])
            }
            
        }
    }

    ## Deal w line widths
    if (length(llwd) < length(col)) {
        llwd <- rep(llwd, length.out = length(col))
    }
    names(llwd) <- names(col)

    lty <- seq_along(col)
    if (!is.null(dotargs[["lty"]])) {
        lty <-  rep(dotargs[["lty"]], length.out = lmx)
        dotargs[["lty"]] <- NULL ## erase
    }
    ## lty <- seq_along(col)
    ## if (is.factor(modxVar)) {
    ##     lty <- seq_along(modxLevels)
    ## } else {
    ##     lty <- seq_along(modxVals)
    ## }
    names(lty) <- names(col)

    parms <- list(x = plotxVar, y = depVar, xlab = plotx, ylab = ylab,
                  type = "n")
    dots.plot <- dotargs[names(dotargs)[names(dotargs) %in% c(names(par()), formalArgs(plot.default))]]
    parms <- modifyList(parms, dots.plot)

    do.call("plot", parms)

    ## iCol: rgb color matrix. Why does rgb insist the columns be
    iCol <- col2rgb(col)
    ### bCol: border color

    bCol <- mapply(rgb, red = iCol[1,], green = iCol[2,],
                   blue = iCol[3,], alpha = opacity, maxColorValue = 255)
    ### sCol: shade color
    sCol <-  mapply(rgb, red = iCol[1,], green = iCol[2,],
                    blue = iCol[3,], alpha = opacity/3, maxColorValue = 255)

    if (interval != "none") {
        for (j in modxVals) {
            k <- match(j, modxVals)   ##integer index
            if (is.factor(modxVar)) i <- j  ## level names
            else i <- k  ## i integer

            if (missing(modx) || is.null(modx)) {
                pdat <- newdf
            } else {
                pdat <- newdf[newdf[ , modx] %in% j, ]
            }
            parms <- list(x = c(pdat[, plotx], pdat[NROW(pdat):1 , plotx]),
                          y = c(pdat$lwr, pdat$upr[NROW(pdat):1]), lty = lty[i])
            dots.plot <- dotargs[names(dotargs)[names(dotargs) %in% c(names(par()), formalArgs(polygon))]]
            parms <- if(!is.null(dots.plot)) modifyList(parms, dots.plot)
            parms <- modifyList(parms, list(border = bCol[i],
                                            col = sCol[i], lwd = 0.3* llwd[k]))
            
            do.call("polygon", parms)
        }
    }

    for (j in modxVals) {
        if (is.factor(modxVar)) i <- j  ## level names
        else i <- match(j, modxVals)   ##integer index
        if (missing(modx) || is.null(modx)) {
            pdat <- newdf
        } else {
            pdat <- newdf[newdf[ , modx] %in% j, ]
        }
        parms <- list(x = pdat[, plotx], y = pdat$fit, lty = lty[i])
        dots.plot <- dotargs[names(dotargs)[names(dotargs) %in% c(names(par()), formalArgs(lines))]]
        parms <- if(!is.null(dots.plot)) modifyList(parms, dots.plot)
        parms <- modifyList(parms, list(col = col[i], lwd = llwd[i]))
        do.call("lines", parms)
    }


    if (plotPoints) {
        parms <- list(xlab = plotx, ylab = ylab,
                      cex = 0.6, lwd = 0.75)
        if (is.factor(modxVar)) {
            parms[["col"]] <- col[as.vector(modxVar[modxVar %in% modxVals])]
            parms[["x"]] <- plotxVar[modxVar %in% modxVals]
            parms[["y"]] <- depVar[modxVar %in% modxVals]
        } else {
            parms[["col"]] <- 1
            parms[["x"]] <- plotxVar
            parms[["y"]] <- depVar
        }
        dots.plot <- dotargs[names(dotargs)[names(dotargs) %in% c(names(par()), formalArgs(points))]]
        parms <- if(!is.null(dots.plot)) modifyList(parms, dots.plot)
        do.call("points", parms)
    }

    if (!missing(legendArgs) && (legendArgs == "none")) {
        ## "do nothing"
    } else if (!is.null(modx)){
        ## guess legend parms from modx information
       if (is.factor(modxVar)){ ## level names
            col <- col[as.vector(modxVals)]
            lty <- lty[as.vector(modxVals)]
            llwd <- llwd[as.vector(modxVals)]
        } else {
            col <- col[names(modxVals)]
            lty <- lty[names(modxVals)]
            llwd <- llwd[names(modxVals)]
        }
        titl <- paste("Moderator:", modx)
        if (is.null(names(modxVals))) {
            legnd <- paste(modxVals, sep = "")
        } else {
            legnd <- paste(names(modxVals), sep = "")
        }
        
        legend.parms <- list(x = "topleft", legend = legnd, lty = lty, col = col,
                             lwd = llwd, bg = "white", title = titl)
        if(!missing(legendArgs) && !is.null(legendArgs)) legend.parms <- modifyList(legend.parms, legendArgs)
        do.call(legend, legend.parms)
    } else {
        legend.parms <- list()
        legend.parms[["x"]] <- "topleft"
        legend.parms[["title"]] <-  "Regression analysis"
        legnd <- c("Predicted values")
        col <- col[1]
        lty <- lty[1]
        llwd <- llwd[1]
        if (interval != "none") {
            legnd[2] <- paste(level, interval, "interval")
            col <- c(col, 0)
            lty <- c(lty, 0)
            llwd <- c(llwd, 0)
        }
        legend.parms[["legend"]] <- legnd
        legend.parms[["col"]] <- col
        legend.parms[["lty"]] <- lty
        legend.parms[["lwd"]] <- llwd
        if(!missing(legendArgs) && !is.null(legendArgs)) legend.parms <- modifyList(legend.parms, legendArgs)
        ## Give the author what they asked for 
        do.call(legend, legend.parms)
    }

    invisible(list(col = col, lty = lty, lwd = llwd))
}

NULL



##' Draw standard error bar for discrete variables
##'
##' Used with plotSlopes if plotx is discrete
##' 
##' @param x The x center point
##' @param y The fitted "predicted" value
##' @param lwr The lower confidence interval bound
##' @param upr The upper confidence interval bound
##' @param arrow.width Arrowhead length must be specified in inches. See ?arrows
##' @param width Thickness of shaded column
##' @param col Color for a bar
##' @param opacity Value in c(0, 254). 120 is default, that's partial see through.
##' @return NONE
##' @export
##' @author Paul Johnson
##' 
se.bars <- function(x, y, lwr, upr, width=1, col = col.se, opacity = 120, lwd = 1) {
    h <- 0.35 * width #half width
    q <- 0.15 * width #quarter width
    d <- 0.05 * width #shaded area
    iCol <- col2rgb(col)
    bCol <- mapply(rgb, red = iCol[1,], green = iCol[2,],
                   blue = iCol[3,], alpha = .8 * opacity, maxColorValue = 255)
    ### sCol: shade color
    sCol <-  mapply(rgb, red = iCol[1,], green = iCol[2,],
                    blue = iCol[3,], alpha = opacity/3, maxColorValue = 255)
    lCol <- mapply(rgb, red = iCol[1,], green = iCol[2,],
                   blue = iCol[3,], alpha = min(255, 2*opacity), maxColorValue = 255)
    if (!is.null(lwr)){
        polygon(c(x-d, x+d, x+d,  x-d), c(rep(lwr, 2), rep(upr, 2)), col = sCol, border = bCol)
        ## PROBLEM: lwd widths are not linked to lwd parameter.
        ##  arrows(x0 = x, y0 = lwr, y1 = upr, code=3, angle=90, length=arrow.width, col = lCol, cex=0.5, lwd = 2)
        lines(c(x, x), c(lwr, upr), col = lCol, cex=0.5, lwd = lwd, lend = 1, ljoin = 1)
        lines(c(x, x) + c(-q, q), c(lwr, lwr), col = lCol, lwd = lwd, lend = 1, ljoin = 1, lmitre = 2)
        lines(c(x, x) + c(-q, q), c(upr, upr), col = lCol, lwd = lwd, lend = 1, ljoin = 1, lmitre = 2)
    }
    lines(c(x, x) + c(-h, h), c(y, y), col = lCol, lwd = lwd + 1)
    NULL
}


NULL


##' Draw display for discrete predictor in plotSlopes
##'
##' There's plotFancy for numeric predictor. This is for discrete
##' 
##' @param newdf The new data object, possibly from predictOMatic
##' @param olddf The model data matrix
##' @param plotx Name of horizontal axis variable
##' @param modx Name of moderator
##' @param modxVals values for modx
##' @param xlab X axis label
##' @param xlim x axis limits. Don't bother setting this, the internal
##'     numbering is too complicated.
##' @param ylab y axis label
##' @param ylim y axis limits
##' @param col color pallet for values of moderator variable
##' @param opacity Value in 0, 255 for darkness of interval shading
##' @param main main title
##' @param space same as space in barplot, vector c(0, 1) is
##'     c(space_between, space_before_first)
##' @param width width of shaded bar area
##' @param llwd requested line width, will re-cycle.
##' @param drawGrid TRUE: draw reference lines for estimates of first
##'     group
##' @param gridlwd line widths for reference lines, default 0.3
##' @param gridlty line type for reference lines, default 5
##' @param ... Arguments sent to par
##' @param legendArgs Arguments to the legend function. Set as "none"
##'     if no legend is needed. Otherwise, provide a list
##' @export
##' @return None
##' @author Paul Johnson <pauljohn@@ku.edu>
##' 
plotFancyCategories <- function(newdf, olddf, plotx, modx=NULL,
                                modxVals, xlab, xlim, ylab, ylim,
                                col = c("black", "blue", "darkgreen", "red", "orange", "purple", "green3"), opacity = 120, main,
                                space=c(0,1), width=1, llwd = 1, offset=0, lty = 1, 
                                drawGrid = TRUE, gridlwd = 0.3, gridlty = 5, ...,
                                legendArgs)
{
    call.orig <- match.call()
    plotxval <- newdf[[plotx]]
    plotxVals <- levels(plotxval)

    if(is.null(modx)){
        modxVals <-  1
    }

    ##Caution, modxVals comes out in wrong order compared to levels.
    ## Why? well put back in order according to levels(newdf)
    if(!is.null(modx)){
        modxVal.names <- names(modxVals)
        names(modxVal.names) <- modxVals
        modxVal.names <- modxVal.names[levels(newdf[[modx]])]
        modxVals <- names(modxVal.names)
        names(modxVals) <- modxVal.names
        
        modx.levels <- modxVals
    }
    ## sort rows
    newdf <- if(is.null(modx)) newdf[order(newdf[[plotx]]), ]
             else newdf[order(newdf[[plotx]], newdf[[modx]]), ]
 
    if(!is.null(modx) && any(levels(newdf[[modx]]) != modxVals) )stop("levels fail")
    
    if(missing(ylim)){
        ylim <- magRange(range(c(newdf[["fit"]], if(!is.null(newdf[["lwr"]])) min(newdf[["lwr"]])),
                                if(!is.null(newdf[["upr"]])) max(newdf[["upr"]])), 1.25)
    }
    if(missing(xlim) || is.null(xlim)){
        xlim <- range(seq_along(plotxVals)) + c(-0.5, 0.5)
    }
    if(missing(xlab) || is.null(xlab)){
        xlab <- plotx
    }
    if(missing(ylab) || is.null(ylab)){
        ylab <- attr(mm, "terms")[[2]]
    }
    ##if(!is.null(modx) && missing(legend.title)){
    ##    legend.title <- modx
    ##}
        
    if(missing(main)){
        main <- "rockchalk discrete predictor plot"
    }

    ## lnc: levels needing colors; I got tired if if/then is.null(modx), see if this helps
    lnc <- if(is.null(modx)) plotxVals  else  modx.levels
    ## can we simplify by having a name selector color/line/ 
    xname <- if(is.null(modx)) plotx else modx

    lwd <- rep(llwd, length(lnc))
    names(lwd) <- lnc
    lty <-  rep(lty, length(lnc))
    names(lty) <- lnc
    
    if(!is.null(names(col))){
        if (any(!lnc %in% names(col))) stop("names in color vector incorrect")
    }
    
    if(missing(col) || is.null(col) || length(col) < length(lnc)) col <- mycolors

    if (length(col) >= length(lnc)){
        col <- col[1:length(lnc)]
        names(col) <- lnc
    } else {
        col <- rep(col, length.out = length(lnc))
        names(col) <- lnc                   
    }
  
    newdf$col <- col[as.character(newdf[[xname]])] 
        
    width <- if(length(width) > 0 && length(width) < length(lnc)) rep(width, length.out = length(lnc))
    names(width) <- lnc
    newdf$xloc <- as.numeric(newdf[[xname]])
    
    newdf$lwd <- lwd[as.numeric(newdf[[xname]])]
    
    ## Previous were done like this before
    if(is.null(modx)){
        newdf$width <- width[as.numeric(newdf[[plotx]])]
    } else {
        newdf$width <- width[as.numeric(newdf[[modx]])]
    }
   
    gridlwd <- if(length(gridlwd) < length(lnc)) rep(gridlwd, length.out = length(lnc))
    gridlty <- if(length(gridlty) < length(lnc)) rep(gridlty, length.out = length(lnc))        



    space <- space * mean(width)
    ## In a data subset, now many rows and columns are there?
    NR <- if(is.null(modx)) 1 else length(unique(newdf[[modx]]))
    NC <- length(unique(newdf[[plotx]]))
    
    if (length(space) == 2) {
        space <- rep.int(c(space[2L], rep.int(space[1L], NR - 1)), NC)
    }
    width <- rep_len(width, NR) ## width of a bar
    offset <- rep_len(as.vector(offset), length(width))
    delta <- width/2
    w.r <- cumsum(space + width)
    w.m <- w.r - delta
    w.l <- w.m - delta
    newdf$w.m <- w.m
    newdf$w.r <- w.r
    newdf$w.l <- w.l
    ## if (horiz) {
    ##     if (is.null(xlim)) 
    ##         xlim <- range(rAdj, height + offset, na.rm = TRUE)
    ##     if (is.null(ylim)) 
    ##         ylim <- c(min(w.l), max(w.r))
    ## }
    ## else {
    ##if (is.null(xlim)) 
        xlim <- c(min(w.l), max(w.r))
    ##if (is.null(ylim)) 
    ##    ylim <- range()
    ##   }
    ## w.m <- matrix(w.m, ncol = NC)


     plot(1, 0, type = "n", ## xlab = xlabs[i], ylab = ylabs[i], 
         xlim = xlim, ylim = ylim, main = main, xaxt = "n",
         xlab = xlab, ylab = ylab)
    
    if (is.null(modx)) {
        axis(1, at = newdf$w.m, labels = newdf[[plotx]])
    }else{
        ## TODO: Tricky, better test this
        xloc <- aggregate(newdf$w.m, by = list(plotx = newdf[[plotx]]),  mean)
        axis(1, at = xloc[ , "x"], labels = xloc[ , "plotx"])
    }
    
    for(mm in 1:(NROW(newdf))){
        ##if(!is.null(newdf[["lwr"]])){
            se.bars(newdf[mm , "w.m"], newdf[mm, "fit"], newdf[mm , "lwr"], newdf[mm , "upr"],
                    col = newdf[mm , "col"], width = newdf[mm , "width"])
        #}
    }

    if(is.null(modx)){
        rownames(newdf) <- newdf[[plotx]]
        plotxList <- list(newdf)
        names(plotxList) <- plotx
    } else {
        plotxList <- split(newdf, f = newdf[ , plotx], drop = TRUE)
        for(i in names(plotxList)) rownames(plotxList[[i]]) <- plotxList[[i]][[xname]]
    }
    
    if(length(plotxList) > 1) {
        for(i in 1:(length(plotxList) - 1)){
            set1 <- plotxList[[i]]
            set2 <- plotxList[[i+1]]
            segments(set1[["w.m"]], set1[["fit"]], set2[["w.m"]], set1[["fit"]], col = col, lwd = gridlwd, lty = gridlty ) 
         }
    }
    
    ## if (plotPoints) {
    ##     parms <- list(xlab = plotx, ylab = ylab, cex = 0.6, lwd = 0.75)
    ##     if (is.factor(modxVar)) {
    ##         parms[["col"]] <- col[as.vector(modxVar[modxVar %in% modxVals])]
    ##         parms[["x"]] <- plotxVar[modxVar %in% modxVals]
    ##         parms[["y"]] <- depVar[modxVar %in% modxVals]
    ##     } else {
    ##         parms[["col"]] <- 1
    ##         parms[["x"]] <- olddf[[plotxVar
    ##         parms[["y"]] <- olddf[[depVar]]
    ##     }
    ##     parms <- modifyList(parms, dotargs)
    ##     do.call("points", parms)
    ## }




    if(!missing(legendArgs) && legendArgs == "none") {
                                        # "do nothing"
    } else if (!is.null(modx)){
        ## guess legend parms from modx information
        legend.parms <- list(x = "bottomleft",
                             fill = col[as.character(lnc)],
                             col = col[as.character(lnc)],
                             lty = lty[as.character(lnc)],
                             lwd = lwd[as.character(lnc)],
                             border = NA, bty = "n")
      
        if(any(modxVals != lnc)){ browser();
            stop("plotFancyCategories: fatal error")
        }
        if (is.null(names(modxVals))) {
            legend.parms[["title"]] <- paste("Moderator:", modx)
            legend.parms[["legend"]] <- modxVals
        } else {
            legend.parms[["title"]] <-  paste("Moderator:", modx)
            legend.parms[["legend"]] <- names(modxVals)
        }
        
        if(!missing(legendArgs) && !is.null(legendArgs)) legend.parms <- modifyList(legend.parms, legendArgs)
        do.call(legend, legend.parms)
    } else {
        ## Give the author what they asked for 
         if(!missing(legendArgs) && !is.null(legendArgs)) do.call(legend, legendArgs)
    }

    
    invisible(list(newdf = newdf, col = col, lwd = lwd, call.orig=call.orig))
}

NULL
