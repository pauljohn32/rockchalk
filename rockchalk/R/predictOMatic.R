##Paul Johnson
## 2012-05-04

## This is a proposed new & improved back end for regression users who
## want to calculate predicted values for selected values of inputs in
## regression. The end user is most likely to use the main function
## "predictOMatic", but the support functions newdata and model.data
## may be helpful in some applications.  Similar in spirit to the
## packages Effects, Zelig, rms, and others that attempt to do the
## same. This is distinguished mainly because it is more flexible for
## end users and works with a broader array of model formulas. Does
## (or will) work with troublesome formulas like log(10+x) + sin(x) +
## poly(x,2).

## I believe "model.data" may be suitable for widespread use in R
## packages like termplot that attempt to re-construct input data
## sets from fitted models.



##' Creates the newdata frame required in predict methods.
##'
##' This function uses various tricks to create a "newdata" object
##' suitable for use in predict methods for R regression objects. It
##' scans the fitted model, discerns the names of the predictors, and
##' then generates a new data frame.  The argument \code{predVals}
##' (predictor values) is very important. It determines which
##' variables are selected for more careful inspection. This argument
##' was called "fl" in previous editions of rockchalk, and that name
##' still works as well. See details.
##'
##' This is used by several rockchalk functions, including plotSlopes,
##' plotCurves, and predictOMatic.  However, I have found many
##' occassions on which it is useful to create newdata objects
##' directly.
##'
##' If not supplied with a predVals  argument, newdata returns a data
##' frame with one row -- the central values (means and modes) of the
##' variables in the data frame that was used to fit the model.
##'
##' The \code{predVals} argument allows users to make fine-grained
##' requests. \code{predVals} can be a named list that supplies
##' specific values for particular predictors.  The omitted predictors
##' will be set at their central values. Any legal vector of values is
##' allowed. For example, \code{predVals = list(x1 = c(10, 20, 30), x2 =
##' c(40, 50), xcat = levels(xcat)))}. That will create a newdata
##' object that has all of the "mix and match" combinations for those
##' values, while the other predictors are set at their central
##' values.
##'
##' In rockchalk 1.7.3, a convenience feature was added that gives
##' automatic value selection via keywords. If the user declares a
##' variable with the "default" keyword, then the default divider
##' algorithm is used to select focal values.  The default divider
##' algorithm is an optional argument of this function. If the default
##' is not desired, the user can specify a divider algorithm by
##' character string, either "quantile", "std.dev.", "seq", or "table".
##' The user can mix and match algorithms along with requests
##' for specific focal values, as in
##' \code{predVals = list(x1 = "quantile", x2 = "std.dev.",
##' x3 = c(10, 20, 30), xcat1 <- levels(xcat1))}
##'
##' @param model Required. Fitted regression model
##' @param predVals Predictor Values that deserve investigation.
##' Previously, the argument was called "fl".  This can be a vector of
##' variable names, a vector that names variables and divider
##' algorithms, or a full list that supplies variables and possible
##' values. Please see details and examples.
##' @param fl focus list, now called x predVals. This is deprecated.
##' @param n Optional. Default = 3. How many focal values are desired?
##' This value is used when various divider algorithms are put to use
##' if the user has specified keywords "default", "quantile", "std.dev."
##' "seq", and "table".
##' @param divider Default is "quantile". Determines the method of
##' selection. Should be one of c("quantile", "std.dev", "seq", "table").
##' @param emf Optional. data frame used to fit model (not a model
##' frame, which may include transformed variables like
##' log(x1). Instead, use output from function \code{model.data}). It
##' is UNTRANSFORMED variables ("x" as opposed to poly(x,2).1 and
##' poly(x,2).2).
##' @return A data frame of x values that could be used as the
##' data = argument in the original regression model. The attribute
##' "varNamesRHS" is a vector of the predictor variable names.
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @export
##' @seealso \code{predictOMatic}
##' @example inst/examples/predictOMatic-ex.R
newdata <- function (model = NULL, predVals = NULL, fl = predVals, emf = NULL, n = 3, divider = "quantile"){
    if (is.null(emf)) emf <- model.data(model = model)
    divider <- match.arg(tolower(divider),
                         c("quantile", "std.dev.","table","seq"))

    varNamesRHS <- attr(emf, "varNamesRHS")
    emf <- emf[ , varNamesRHS, drop = FALSE]
    modelcv <- centralValues(emf)
    if (is.null(fl)) return(modelcv)

    ## if fl is a vector with no names, create fl, a list with fl's values as names
    ## if fl is a vector with names, create fl, a list
    if(!is.list(fl)){
        flnames <- names(fl)
        if (is.null(flnames)){
            flnames <- fl
            fl <- vector("list", length = length(flnames))
            names(fl) <- flnames
        } else {
            fl <- as.list(fl)
        }
    }

    if (sum(!names(fl) %in% varNamesRHS) > 0) stop(cat(c("Error. The focus list:  predVals requests variables that are not included in the original model. The names of the variables in the focus list be drawn from this list: ",  varNamesRHS, "\n")))
    ## TODO: Consider "padding" range of fl for numeric variables so that we
    ## get newdata objects including the min and max values.
    flnames <- names(fl)
    for(x in flnames) {
        if (is.character(fl[[x]]) && length(fl[[x]]) == 1)
            if (fl[[x]] == "default") {
                fl[[x]] <- focalVals(emf[ ,x], divider, n)
            } else if (fl[[x]] %in% c("quantile", "std.dev.", "table", "seq")){
                fl[[x]] <- focalVals( emf[ ,x],  divider = fl[[x]], n)
            }
    }

    mixAndMatch <- expand.grid(fl)
    ## TODO: Its OK to select columns this way, but no better way add names?
    unames <- colnames(modelcv)[!colnames(modelcv) %in% colnames(mixAndMatch)]
    newdf <- cbind(mixAndMatch, modelcv[  , unames])
    colnames(newdf) <- c(colnames(mixAndMatch), unames)
    newdf
}
NULL

##' Creates a "raw" (UNTRANSFORMED) data frame equivalent
##' to the input data that would be required to fit the given model.
##'
##' Unlike model.frame and model.matrix, this does not return transformed
##' variables. It deals with regression formulae that have
##' functions like poly(x, d) in them. It differentiates x from d in
##' those expressions. And it also manages log(x + 10).
##'
##' @param model A fitted regression model in which the data argument
##' is specified. This function will fail if the model was not fit
##' with the data option.
##' @return A data frame
##' @export
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @example inst/examples/model.data-ex.R
model.data <- function(model){
    ## from nls, returns -1 for missing variables
    lenVar <- function(var, data) tryCatch(length(eval(as.name(var),
                         data, env)), error = function(e) -1)
    fmla <- formula(model)
    varNames <- all.vars(fmla) ## all variable names
    fmla2 <- fmla
    fmla2[[2L]] <- 0
    varNamesRHS <- all.vars(fmla2)
    varNamesLHS <- setdiff(varNames, varNamesRHS)
    env <- environment(fmla)
    if (is.null(env))
        env <- parent.frame()

    dataOrig <-  eval(model$call$data, environment(formula(model)))
    dataOrigRN <- row.names(dataOrig)
    n <- sapply(varNames, lenVar, data = dataOrig)
    targetLength <- length(eval(as.name(varNamesLHS[1]), dataOrig, env))
    varNames <- varNames[n == targetLength]
    ldata <- lapply(varNames, function(x) {
        myv <- eval(as.name(x), dataOrig, env)
        row.names(myv) <- NULL
        myv
    }
                    )
    names(ldata) <- varNames
    data <- data.frame(ldata[varNames])
    if (!is.null(dataOrigRN)) row.names(data) <- dataOrigRN
    ## remove rows listed in model's na.action
    ## TODO: question: what else besides OMIT might be called for?
    if ( !is.null(model$na.action)){
        data <- data[ -as.vector(model$na.action), , drop = FALSE]
    }
    ## keep varNamesRHS that exist in datOrig
    attr(data, "varNamesRHS") <- setdiff(colnames(data), varNamesLHS)
    invisible(data)
}
NULL


##' Create a focal value vector.
##'
##' This selects some values of a variable and creates a new "focal vector"
##' from them. Can use one "divider" algorithm, to be selected by name.
##'
##' This is a "wrapper" (or convenience) function that re-directs work
##' to other functions. The functions that do the work to select the
##' focal values for types ("table", "quantile", "std.dev.", "seq") are
##' (cutByTable(), cutByQuantile(), cutBySD(), and plotSeq())
##'
##' The built-in R function \code{pretty()}
##' works as of rockchalk 1.7.2. Any function that accepts an argument
##' n will work, as long as it creates a vector of values.
##'
##' @param x The input variable may be numeric or a factor.
##' @param divider Either a quoted string name of an algorithm or a
##' function. Default = "quantile" for numeric variables, "table" for
##' factors. Other valid values: "seq" for an evenly spaced sequence
##' from minimum to maximum, "std.dev." for a sequence that has the
##' mean at the center and values on either side that are proportional
##' to the standard deviation.
##' @param n Desired number of focal values.
##' @export
##' @return A named vector of focal values selected from a variable. The
##' values of the names should be informative and useful for plotting or
##' other diagnostic work.
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @export
##' @seealso \code{predictOMatic} \code{newdata}
focalVals <-
    function(x, divider = "quantile", n = 3)
{
    if (is.function(divider)) {
        res <-  divider(x, n)
        return(res)
    }

    if (is.numeric(x)) {
        divider <- match.arg(tolower(divider),
                             c("quantile", "std.dev.","table", "seq"))
        res <- switch(divider,
                      table = rockchalk:::cutByTable(x, n),
                      quantile = rockchalk:::cutByQuantile(x, n),
                      "std.dev." = rockchalk:::cutBySD(x, n),
                      "seq" = rockchalk:::plotSeq(x, n),
                      stop("unknown 'divider' algorithm"))
    }  else {
        res <- rockchalk:::cutByTable(x, n)
    }
    res
}
NULL



##' predictOMatic creates predicted values for a fitted regression
##' model. This demonstrates marginal effects of the predictor
##' variables.
##'
##' The behavior of this function depends on the argument predVals
##' (predictor values).  If no predVals argument is supplied,
##' predictOMatic creates a list of new data frames, one for each
##' predictor variable. It uses the default divider algorithm (see the
##' divider argument) and it estimates predicted values for \code{n}
##' different values of the predictor. A model with formula \code{y ~
##' x1 + x2 + x3} will cause 3 separate output data frames, one for
##' each predictor. They will be named objects in the list.
##'
##' If predVals is suppiled, it must name only predictors that are
##' fitted in the model. \code{predictOMatic} will choose the mean or mode for
##' variables that are not explicitly listed, and selected values of
##' the named variables are "mixed and matched" to make a data set.
##
##' There are many formats in which it can be supplied.  Suppose a
##' regression formula is \code{y1 ~ sex + income + health +
##' height}. The simplest format for predVals will be a vector of
##' variable names, leaving the selection of detailed values to the
##' default algorithms. For example, \code{predVals = c("income",
##' "height")} will cause sex and health to be set at central values
##' and income and height will have target values selected according
##' to the divider algorithm (see the argument \code{divider}).
##'
##' If the user wants various divider algorithms to be used, change
##' the argument to predvals = c(income = "quantile", height =
##' "std.dev."). The dividers provided by the rockchalk package are
##' "quantile", "std.dev.", "seq" and "table".  Those are discussed
##' more completely in the help for \code{focalVals}.  The appropriate
##' algorithms will select focal values of the predictors and they
##' will supply \code{n} values for each in a "mix and match" data
##' frame. After rockchalk 1.7.2, the divider argument can also be the
##' name of a function, such as R's pretty.
##'
##' Finally, users who want very fine grained control over
##' predictOMatic can supply a named list of predictor
##' values. For example, predVals = list(height = c(5.5, 6.0, 6.5),
##' income = c(10, 20, 30, 40, 50), sex = levels(dat$sex)). One can
##' also use algorithm names here.  Some predVals = list(height =
##' c(5.5, 6.0, 6.5), income = "quantile") and so forth. Examples are
##' offered below.
##'
##' The variables named in the predVals argument should be the names
##' of the variables in the raw data frame, not the names that R
##' creates when it interprets a formula. We want "x", not the
##' transformation in the functions (not \code{log(x)}, or
##' \code{as.factor(x)} or \code{as.numeric(x)}). If a formula has a
##' predictor \code{poly(height, 3)}, then the predVals argument
##' should refer to height, not \code{poly(height, 3)}.  I've invested
##' quite a bit of effort to make sure this "just works" (many
##' alternative packages that calculate predicted values do not).
##'
##' It it important to make sure that diagnostic plots and summaries
##' of predictions are calculated with the exact same data that was
##' used to fit the model. This is surprisingly difficult because
##' formulas can include things like log(income + d) and so forth. The
##' function \code{model.data} is the magic bullet for that part of
##' the problem.
##'
##' Here is one strategy for doing this.
##'
##' Fit a regression model
##'
##' d <- 3
##' alpha <- 13
##' m1 <- lm(yout ~ xin + xout + poly(xother,2) + log(xercise + alpha), data = dat)
##'
##' Then grab the data that was used from the model
##'
##' m1dat <- model.data(m1)
##'
##' Now, when you are thinking about which values you might like to
##' specify in predVals, use m1dat to decide. Try
##'
##' summarize(m1dat)
##'
##' Then run something like
##'
##' predictOMatic( m1, predVals = list(xin = median(m1dat$xin), xout =
##' c(1,2,3), xother = quantile(m1dat$xother))
##'
##' Get the idea?
##'
##' @param model Required. A fitted regression model. A \code{predict}
##' method must exist for that model.
##' @param predVals Predictor values. Optional. Users can supply
##' detailed information to determine which predictors and which
##' values for them are selected. The allowed format has been
##' generalized a great deal in rockchalk 1.7.4, allowing now a vector
##' of variable names, a vector of named algorithms, or a list
##' containing detailed value selections. See details and examples.
##' @param divider An algorithm name from c("quantile", "std.dev",
##' "seq", "table") or a user-provided function.  This sets the method
##' for selecting values of the predictor. Documentation for the
##' rockchalk methods can be found in the functions
##' \code{cutByQuantile}, \code{cutBySD}, \code{plotSeq}, and
##' \code{cutByTable},.
##' @param n Default = 5. The number of values for which
##' predictions are sought.
##' @param fl focus list. DEPRECATED. Previous name of argument predVals.
##' @param ... Optional arguments to be passed to the predict function
##' @return A data frame or a list of data frames.
##' @export
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @example inst/examples/predictOMatic-ex.R
predictOMatic <- function(model = NULL, predVals = NULL, divider = "quantile", n = 5, fl = predVals, ...) {
    dots <- list(...)
    dotnames <- names(dots)

    ## 2013-04-18: What magic was I planning with dots here? Can't recall :(
    ##  ## next should give c('digits', 'alphaSort')
    ##  nnames <- names(formals(rockchalk::summarizeNumerics))[-1L]
    ##  ## names that need keeping if in dots:
    ##  keepnames <- dotnames %in% nnames

    emf <- model.data(model = model)
    varNamesRHS <- attr(emf, "varNamesRHS")

    ##2013-04-18. Considering changing user interface
    ## if(length(n) < length(fl)) n <- rep(n, length.out = length(fl))
    ## if(length(divider) < length(fl)) divider <- rep(divider, length.out = length(fl)

    if (is.null(fl)){
        flxxx <- list()
        nd <- lapply (varNamesRHS, function(x) {
            flxxx[[x]]  <- focalVals(emf[ ,x], divider, n)
            ndnew <- newdata(model, fl = flxxx[x], emf = emf)
            row.names(ndnew) <- names(flxxx[[x]])
            pargs <- list(model, newdata = ndnew, type = "response")
            pargs <-  modifyList(pargs, dots)
            fit <- do.call("predict", pargs)
            ## fit <- predict(model, newdata = ndnew, type = "response", ...)
            ndnew <- cbind(fit, ndnew)
            ndnew
        })
        attr(nd, "flnames") <- varNamesRHS
        names(nd) <- varNamesRHS
    } else {
        if (!is.list(fl) & is.vector(fl)) {
            if (is.null(names(fl))){ ##no names
                flnames <- fl
                fl <- lapply(flnames, function(x) "default")
                names(fl) <- flnames
            } else {  ## names and options
                fl <- as.list(fl)
            }
        }
        flnames <- names(fl)
        if (any(! flnames %in% varNamesRHS))
            stop(paste("Sorry, predictOMatic won't work. \nYou cannot put variables in predVals unless you fit them in the model first. \nFor this model, the only legal values would be, ", varNamesRHS))

        for(x in flnames) {
            if (is.character(fl[[x]]) && length(fl[[x]]) == 1)
                if (fl[[x]] == "default") {
                    fl[[x]] <- focalVals(emf[ ,x], divider, n)
                } else if (is.function(fl[[x]]) | fl[[x]] %in% c("quantile", "std.dev.","table", "seq")){
                    fl[[x]] <- focalVals(emf[ ,x],  divider = fl[[x]], n)
                }
        }

        nd <- newdata(model, fl, emf = emf)
        pargs <- list(model, newdata = nd, type = "response")
        pargs <-  modifyList(pargs, dots)
        fit <- do.call("predict", pargs)
        fit <- predict(model, newdata = nd, type = "response", ...)
        nd <- cbind(fit, nd)
        attr(nd, "flnames") <- flnames
    }
    nd
}
NULL


## ## Other approaches I've wrestled with for model.data

## ## model.data.1: how its done in termplot, using carrier functions
## ## I used this in "plotCurves" of rockchalk
## ## Problem: cant handle log(10+x), can handle log(x+10)
## ## termplot has same trouble, observe:
## ## x <- rpois(100,l=6)
## ## y <- rpois(100, l=6)
## ## m1 <- lm(log(y) ~ log(10+x))
## ## termplot(m1)

## model.data.1 <- function(model = NULL) {
##     carrier <- function(term, data, enc = NULL) {
##         if (length(term) > 1L)
##             carrier(term[[2L]])
##         else eval(term, envir = data, enclos = enc)
##     }
##     carrier.name <- function(term) {
##         if (length(term) > 1L)
##             carrier.name(term[[2L]])
##         else as.character(term)
##     }
##     mt <- terms(model)
##     mt <- delete.response(mt)
##     mf <- model.frame(model) ##[ ,-1] ## -1 gets rid of DV
##     cn <- parse(text = colnames(mf))
##     varnames <- unique(unlist(lapply(cn, carrier.name)))
##     print(varnames)
##     emf <- get_all_vars(mt, data = expand.model.frame(model, varnames, na.expand=TRUE))
##     emf <- emf[ , varnames]
##     attr(emf, "varnames") <- varnames
##     emf
## }


## ## Bill Dunlap suggests this approach, r-help 2012-04-22.
## ## mt is a terms object:
## ## unique(unlist(lapply(attr(mt, "variables")[-1], all.vars)))
## ## Solves log(10+x) problem
## ## EXCEPT, as Bill warns, it
## ## includes d in poly(x2,d). That includes "d" as a variable
## ## name.
## model.data.2 <- function(model = NULL) {
##     mt <- terms(model)
##     varnames <- unique(unlist(lapply(attr(mt, "variables")[-1], all.vars)))
##     print(varnames)
##     emf <- get_all_vars(mt, data = expand.model.frame(model, varnames, na.expand=TRUE))
##     emf <- emf[ , varnames1]
##     attr(emf, "varnames") <- varnames1
##     emf
## }


