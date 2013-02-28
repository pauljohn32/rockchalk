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
##' built-in algorithms will select the "cut points". Three algorithms
##' have been prepared so far, \code{quantile}, \code{std.dev.}, and
##' \code{table}. If the number of unique observed values is smaller
##' than 6, the \code{table} method is used.  The 5 most frequently
##' observed values of modx are selected. Otherwise, the quantile
##' method is used. Predictive lines are plotted for the following
##' percentiles {0.25,0.50,0.75}. The algorithm \code{std.dev.} plots
##' three lines, one for the mean of modx, and one for the mean minus
##' one standard deviation, and the other for the mean plus one
##' standard deviation.
##'
##'
##' @param model Required. Fitted regression object. Must have a predict method
##' @param plotx Required. String with name of IV to be plotted on x axis
##' @param modx Required. String for moderator variable name. May be either numeric or factor.
##' @param modxVals Optional. If modx is numeric, either a character string, "quantile", "std.dev.",
##' or "table", or a vector of values for which plotted lines are
##' sought. If modx is a factor, the default approach will create one
##' line for each level, but the user can supply a vector of levels if
##' a subset is desired.
##' @param plotPoints Optional. Should the plot include the scatterplot points along with the lines.
##' @param col Optional. A color vector.  By default, the R's builtin colors will be used,  which are "black", "red", and so forth.  Instead, a vector of color names can be supplied, as in c("pink","black", "gray70").  A color-vector generating function like rainbow(10) or gray.colors(5) can also be used. A vector of color names can be supplied with this function. Color names will be recycled if the plot requires more different colors than the user provides.
##' @param envir environment to search for variables.
##' @param ... further arguments that are passed to plot.
##' @export
##' @import car
##' @return A plot is created as a side effect, a list is returned including
##' 1) the call, 2) a newdata object that includes information on the curves that were
##' plotted, 3) a vector modxVals, the values for which curves were drawn.
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @example  inst/examples/plotCurves-ex.R

plotCurves <-
  function (model = NULL, plotx = NULL, modx = NULL, modxVals = NULL,
            plotPoints = TRUE, col, envir = environment(formula(model)), ...)
{
  if (is.null(model))
    stop("plotCurves requires a fitted regression model.")
  if (is.null(plotx))
    stop("plotCurves requires the name of the variable to be drawn on the x axis")
  if (is.null(modx))
    stop("plotCurves requires the name of moderator variable for which several slopes are to be drawn")

  ## carrier <- function(term, data, enc = NULL) {
  ##   if (length(term) > 1L)
  ##     carrier(term[[2L]])
  ##   else eval(term, envir = data, enclos = enc)
  ## }
  ## carrier.name <- function(term) {
  ##   if (length(term) > 1L)
  ##     carrier.name(term[[2L]])
  ##   else as.character(term)
  ## }

  cl <- match.call()
  mf <- model.frame(model)
  ##  tt <- terms(model)

  ##  cn <- parse(text = colnames(mf))
  ## varnames <- unlist(lapply(cn, carrier.name))
  ## emf <- get_all_vars(tt, data = expand.model.frame(model, varnames, na.expand=TRUE))
  emf <- model.data(model)

  ## plotxVar <- carrier(parse(text = plotx), emf, enc=envir)
  plotxVar <- emf[ , plotx]
  if (!is.numeric(plotxVar))
    stop(paste("plotCurves: The variable", plotx, "should be a numeric variable"))

  ##modxVar <- carrier(parse(text = modx), emf, enc=envir)
  modxVar <- emf[ , modx]
  depVar <- model.response(mf)

  ylab <- names(mf)[1]  ## returns transformed DV
  ##ylab <- varnames[1] ## returns untransformed carrier DV
  plotyRange <- magRange(depVar, mult=c(1,1.2))
  plotxRange <- range(plotxVar, na.rm=TRUE)
  plotxSeq <- plotSeq(plotxRange, length.out = 40)

  if (is.factor(modxVar)) { ## modxVar is a factor
    if (is.null(modxVals)) {
      modxVals <- levels(modxVar)
    } else {
      if (!all(modxVals %in% levels(modxVar))) stop("modxVals includes non-observed levels of modxVar")
    }
  } else {                  ## modxVar is not a factor
    ## modxRange <- range(modxVar, na.rm=TRUE)
    if (is.null(modxVals)) {
      modxVals <- cutByQuantile(modxVar)
    } else {
      if (is.numeric(modxVals)) {
      ##TODO: Insert some checks that modxVals are reasonable
      } else {
        if (is.character(modxVals)) {
          modxVals <- match.arg(tolower(modxVals),
                                c("quantile", "std.dev.", "table"))
          print(modxVals)
          modxVals <- switch(modxVals,
                             table = cutByTable(modxVar),
                             quantile = cutByQuantile(modxVar),
                             "std.dev." = cutBySD(modxVar),
                             stop("unknown 'modxVals' algorithm"))
        }
      }
    }
  }
  lmx <- length(modxVals)
  if (missing(col)) col <- 1:lmx
  if (length(col) < lmx) rep(col, length.out = lmx)

##  predictors <- colnames(emf)[-1]
  predictors <- attr(emf, "varNamesRHS")
  predictors <- setdiff(predictors, c(modx, plotx))
  newdf <- data.frame(expand.grid(plotxSeq, modxVals))
  colnames(newdf) <- c(plotx, modx)
  if (length(predictors) > 0) {
    newdf <- cbind(newdf, centralValues(emf[, predictors, drop = FALSE]))
    colnames(newdf) <- c(plotx, modx, predictors)
  }
  newdf$pred <- predict(model, newdata = newdf)
  dotargs <- list(...)
  if (!plotPoints){
    parms <- list(plotxVar, depVar, xlab = plotx, ylab = ylab, ylim = plotyRange,
         type = "n")
    parms <- modifyList(parms, dotargs)
    do.call("plot", parms)
  } else {
    if (is.factor(modxVar)) {
      parms <- list(plotxVar, depVar, xlab = plotx, ylab = ylab, ylim = plotyRange,
           col = col)
      parms <- modifyList(parms, dotargs)
      do.call("plot", parms)
    }
    else {
      parms <- list(plotxVar, depVar, xlab = plotx, ylab = ylab, ylim = plotyRange)
      parms <- modifyList(parms, dotargs)
      do.call("plot", parms)
    }
  }
  for (i in 1:lmx) {
    pdat <- newdf[newdf[, modx] %in% modxVals[i], ]
    lines(pdat[, plotx], pdat$pred, lty = i, col = col[i], lwd = 2)
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
  class(z) <- "rockchalk2d"

  invisible(z)
}
