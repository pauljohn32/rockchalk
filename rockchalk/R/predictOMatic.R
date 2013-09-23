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
##' was called \code{"fl"} in previous editions of rockchalks.
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
newdata <- function (model = NULL, predVals = NULL, emf = NULL, n = 3,
                     divider = "quantile"){
    if (is.null(emf)) emf <- model.data(model = model)
    divider <- match.arg(tolower(divider),
                         c("quantile", "std.dev.","table","seq"))

    varNamesRHS <- attr(emf, "varNamesRHS")
    emf <- emf[ , varNamesRHS, drop = FALSE]
    modelcv <- centralValues(emf)
    if (is.null(predVals)) return(modelcv)

    ## if predVals is a vector with no names, create predVals,
    ## a list with predVals's values as names
    ## if predVals is a vector with names, create predVals, a list
    if(!is.list(predVals)){
        flnames <- names(predVals)
        if (is.null(flnames)){
            flnames <- predVals
            predVals <- vector("list", length = length(flnames))
            names(predVals) <- flnames
        } else {
            predVals <- as.list(predVals)
        }
    }

    if (sum(!names(predVals) %in% varNamesRHS) > 0) stop(cat(c("Error. The focus list:  predVals requests variables that are not included in the original model. The names of the variables in the focus list be drawn from this list: ",  varNamesRHS, "\n")))
    ## TODO: Consider "padding" range of predVals for numeric variables so that we
    ## get newdata objects including the min and max values.
    flnames <- names(predVals)

    for(x in flnames) {
        if (is.null(predVals[[x]])){
            predVals[[x]] <- focalVals(emf[ ,x], divider, n)
        } else if (length(predVals[[x]]) == 1)
            if (is.character(predVals[[x]]) & (predVals[[x]] == "default")) {
                predVals[[x]] <- focalVals(emf[ ,x], divider, n)
            } else if (is.character(predVals[[x]]) & (predVals[[x]] %in% c("quantile", "std.dev.", "table", "seq"))){
            predVals[[x]] <- focalVals( emf[ ,x],  divider = predVals[[x]], n)
        }
    }

    mixAndMatch <- expand.grid(predVals)
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
##' @param na.action Defaults to na.pass, so model as it would appear in user workspace is re-created.
##' @return A data frame
##' @export
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @example inst/examples/model.data-ex.R
model.data <- function(model, na.action = na.pass){
    ## from nls, returns -1 for missing variables
    lenVar <- function(var, data) tryCatch(NROW(eval(as.name(var),
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

    dataOrig <-  eval(model$call$data, env)
    if (is.null(dataOrig)) dataOrig <- model.frame(model)

    dataOrigRN <- row.names(dataOrig)
    n <- sapply(varNames, lenVar, data = dataOrig)

    targetLength <- max(n) ## suppose the longest-length is the number
    ## of rows in input data, before missings removed.

    varNames <- varNames[n == targetLength]

    varNamesRHS <- varNamesRHS[varNamesRHS %in% varNames]
    varNamesLHS <- varNamesLHS[varNamesLHS %in% varNames]


#    ldata <- lapply(varNames, function(x) {
#        myv <- eval(as.name(x), dataOrig, env)
#        row.names(myv) <- NULL
#        myv
#    }
#                    )
#    names(ldata) <- varNames
#    data <- data.frame(ldata[varNames])
#    if (!is.null(dataOrigRN)) row.names(data) <- dataOrigRN

    mylhs <- eval(varNamesLHS)
    myrhs <- eval(paste(varNamesRHS, collapse = " + "))
    myfmla <- as.formula(paste(mylhs, " ~", myrhs), env = environment(formula(model)))
    data <- model.frame(myfmla, dataOrig, na.action = na.action)


    ## remove rows listed in model's na.action
    ## TODO: double check. Following not needed because model.frame does it
    ##if (!is.null(model$na.action)){
    ##     data <- data[ -as.vector(model$na.action), , drop = FALSE]
    ##}

    ## keep varNamesRHS
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
##' If predVals is supplied, it must name only predictors that are
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
##' @param ... Optional arguments to be passed to the predict
##' function. In particular, the arguments se.fit and interval are
##' extracted from ... and used to control the output.
##' @return A data frame or a list of data frames.
##' @export
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @example inst/examples/predictOMatic-ex.R
predictOMatic <-
    function(model = NULL, predVals = NULL, divider = "quantile", n = 5, ...)
{
    dots <- list(...)
    dotnames <- names(dots)

    interval <- "none"
    if (!is.null(dots[["interval"]])){
        interval <- dots[["interval"]]
        dots[["interval"]] <- NULL
    }

    se.fit <- FALSE
    if (!is.null(dots[["se.fit"]])){
        se.fit <- dots[["se.fit"]]
        dots[["se.fit"]] <- NULL
    }


    ## 2013-05-12 TODO, or FIXME 1) no smarter way than previous?  2)
    ## Why not trust modifyList below to do the work for us. 3) Why
    ## did I only attend to "interval" 4) will any of the other
    ## predict arguments cause trouble??

    ## 2013-04-18: What magic was I planning with dots here? Can't recall :(
    ##  ## next should give c('digits', 'alphaSort')
    ##  nnames <- names(formals(rockchalk::summarizeNumerics))[-1L]
    ##  ## names that need keeping if in dots:
    ##  keepnames <- dotnames %in% nnames

    emf <- model.data(model = model)
    varNamesRHS <- attr(emf, "varNamesRHS")

    ##2013-04-18. Considering changing user interface
    ## if(length(n) < length(predVals)) n <- rep(n, length.out = length(predVals))
    ## if(length(divider) < length(predVals)) divider <- rep(divider, length.out = length(predVals)

    if (is.null(predVals)){
        flxxx <- list()
        nd <- lapply (varNamesRHS, function(x) {
            flxxx[[x]]  <- focalVals(emf[ ,x], divider, n)
            ndnew <- newdata(model, predVals = flxxx[x], emf = emf)
            row.names(ndnew) <- names(flxxx[[x]])
            pargs <- list(model, newdata = ndnew, type = "response", interval = interval, se.fit = se.fit)
            pargs <-  modifyList(pargs, dots)
            fit <-  do.call("predictCI", pargs)
            if (interval != "none")  ndnew <- cbind(ndnew, fit$fit)
            else ndnew <- cbind(ndnew, fit = fit$fit[ ,"fit"])
            if (se.fit) ndnew <- cbind(ndnew, fit$se.fit)
            attr(ndnew, "residual.scale") <- fit$residual.scale
            ndnew
        })
        attr(nd, "pnames") <- varNamesRHS

        names(nd) <- varNamesRHS
    } else {
        if (!is.list(predVals) & is.vector(predVals)) {
            if (is.null(names(predVals))){ ##no names
                pnames <- predVals
                predVals <- lapply(pnames, function(x) "default")
                names(predVals) <- pnames
            } else {  ## names and options
                predVals <- as.list(predVals)
            }
        }
        pnames <- names(predVals)
        if (any(! pnames %in% varNamesRHS))
            stop(paste("Sorry, predictOMatic won't work. \nYou cannot put variables in predVals unless you fit them in the model. \nFor this model, the only legal values would be, ", paste(varNamesRHS, collapse = " "), "\n"))

        for(x in pnames) {
            if (is.character(predVals[[x]]) && length(predVals[[x]]) == 1)
                if (predVals[[x]] == "default") {
                    predVals[[x]] <- focalVals(emf[ ,x], divider, n)
                } else if (is.function(predVals[[x]]) | predVals[[x]] %in% c("quantile", "std.dev.","table", "seq")){
                    predVals[[x]] <- focalVals(emf[ ,x],  divider = predVals[[x]], n)
                }
        }

        nd <- newdata(model, predVals, emf = emf)
        pargs <- list(model, newdata = nd, type = "response", interval = interval)
        pargs <-  modifyList(pargs, dots)

        fit <-  do.call("predictCI", pargs)
        if (interval != "none")  nd <- cbind(nd, fit$fit)
        else nd <- cbind(nd, fit = fit$fit[ ,"fit"])
        if (se.fit) nd <- cbind(nd, fit$se.fit)
        attr(nd, "residual.scale") <- fit$residual.scale
        attr(nd, "pnames") <- pnames
    }
    nd
}
NULL



##' Calculate a predicted value matrix (fit, lwr, upr) for a
##' regression, either lm or glm, on either link or response scale.
##'
##' This adapts code from predict.glm and predict.lm. I eliminated
##' type = "terms" from consideration.
##'
##' R's predict.glm does not have an interval argument. There are
##' about 50 methods to calculate CIs for predicted values of GLMs,
##' that's a major worry. I don't want to simulate them, that seems
##' brutish.  This function takes the simplest route, calculating the
##' (fit, lwr, upr) in the linear predictor scale, and then if type=
##' "response", those 3 columns are put through linkinv().  This is
##' the same method that SAS manuals suggest they use, same as Ben
##' Bolker suggests in r-help (2010).  I'd rather use one of the
##' fancy tools like Edgeworth expansion, but that R code is not
##' available (but is promised).
##'
##' Use predict.lm with se.fit = TRUE to calculate fit and se.fit.
##' Then calculate lwr and upr as fit +/- tval * se.fit. If model is
##' lm, the model df.residual will be used to get tval. If glm, this
##' is a normal approximation, so we thugishly assert tval = 1.98.
##'
##' There's some confusing term translation. I wish R lm and glm would
##' be brought into line. For lm, residual.scale = sigma. For glm,
##' residual.scale = sqrt(dispersion)
##'
##' @param object Regression object, class must include glm or lm.
##' @param newdata Data frame including focal values for predictors
##' @param type One of c("response", "link"), defaults to former.
##' @param interval One of c("none", "confidence",
##' "prediction"). "prediction" is defined only for lm objects, not
##' for glm.
##' @param dispersion Will be estimated if not provided. The variance coefficient of the glm, same as scale squared. Dispersion is allowed as an argument in predict.glm.
##' @param scale  The square root of dispersion. In an lm, this is the RMSE, called sigma in summary.lm.
##' @param na.action What to do with missing values
##' @param level 0.95 or whatever confidence level one desires.
##' @param ... Other arguments to be passed to predict
##' @return c(fit, lwr, upr), and possibly more.
##'
predictCI <-
  function(object, newdata = NULL, type = c("response", "link"),
           interval = c("none", "confidence", "prediction"),
           dispersion = NULL, scale = NULL,
           na.action = na.pass, level = .95, ...)
{
    interval <- match.arg(interval)
    type <- match.arg(type)
    na.act <- object$na.action
    object$na.action <- NULL ## Why did predict.glm do this?

    ## if interval = none, figure if the predict method gives us a vector of numbers,
    ## or a matrix, or a list with fit as an argument. Oh, this is
    ## frustrating.
    if (interval == "none") {
        dots <- list(...)
        pargs <- list(object, newdata = newdata, type = "response", se.fit = TRUE, na.action = na.action)
        pargs <-  modifyList(pargs, dots)
        predtry <-  try(do.call("predict", pargs))
        if (inherits(predtry, "try-error")) {
            stop("rockchalk:::predCI: predict(object) gave an error. Maybe this regression model does not have a predict method? \n")
        }
        ## vectors or matrices are atomic
        if (is.atomic(predtry)) {
            if (is.vector(predtry)) {
                predtry <- as.matrix(predtry, ncol = 1)
                dimnames(predtry) = list(NULL, c("fit"))
            } else if (!is.matrix(predtry)) {
                stop("atomic, but not a vector or matrix. What are you?")
            }
            pred <- list(fit = predtry, se.fit = NULL)
            return(pred)
        }
        if (is.list(predtry)) {
            fit <- predtry[["fit"]]
            if (is.null(fit)) {
                stop(paste("rockchalk:predictCI: return object from predict",
                           "does not have a fit element.",
                           "We cannot understand that."))
            } else if (is.vector(fit)) {
                fit <- as.matrix(fit, ncol = 1)
                dimnames(fit) = list(NULL, c("fit"))
            } else { ##gamble, try to cast it as a matrix
                fit <- as.matrix(fit)
                ##TODO: rethink. if there is no fit[["fit"]] inside there, other stuff will break.
            }
            predtry[["fit"]] <- fit
            if (is.null(predtry[["se.fit"]])) predtry[["se.fit"]] <- NULL
            ## previous silly? if se.fit is undefined, we want to set it NULL.
            ## or do we care? Won't future accessors get NULL automatically?
            return(predtry)
        } else {
            stop(paste("rockchalk:predictCI: return object from predict is not",
                       "a vector or a matrix or a list.",
                       "fit element. We can't understand that."))
        }
    }
    ## First, try the object's predict method. Need to know if
    ## a model's predict method answers in an understandable way
    ## to predict(object, type = "response", interval = "confidence").
    ##
    ## Frustratingly, R glm objects don't give error or warning
    ## from that, instead they just ignore it.

    ## Lets try, see what we get. If a model has no predict
    ## method, this should alert us.
    intervaltry <- interval
    if (interval == "none") intervaltry <- "confidence"
    ## TODO previous "none" should be impossible, please verify that
    ## can never happen.
    predtry <- try(predict(object, newdata = newdata, se.fit = TRUE,
                           type = type, interval = intervaltry,
                           na.action = na.action))

    if (inherits(predtry, "try-error")) {
        cat(paste("rockchalk:::predCI: There was an error from predict(object).",
                  "We will see if we can improvize.\n"))
    } else {
        ## Perplexing, Must weed out return from  predict.glm or
        ## other non-standard predict methods.
        if (is.list(predtry)) {
            if (is.vector(predtry$fit)) {
                cat(paste("rockchalk:::predCI: model's predict method does not return an interval.",
                          "We will improvize with a Wald type approximation",
                          "to the confidence interval \n"))
            } else if (is.null(dimnames(predtry[["fit"]]))) {
                warning(paste("rockchalk:::predCI. The object's predict method",
                              "did not return a $fit component that has dimnames,",
                              "so I'll ignore that output and improvize.\n"))
            } else {
                if (all(dimnames(predtry[["fit"]])[[2]] == c("fit", "lwr", "upr")))
                    return(predtry)
                ## if this test is passed, the work is done, the model's
                ## predict method returned the expected structure
            }
        } else if (is.vector(predtry)) {
            cat(paste("rockchalk:::predCI: model's predict method does not return an interval.",
                      "We will try to improvize with a Wald type approximation to the",
                      "confidence interval \n"))
        }
    }
    ## If model was from lm, previous should have returned already.

    ## Previous will not return for glm, hopefully. glm has no interval
    ## argument, but using one doesn't cause error. It just returns
    ## what we don't want.

    ## Now we are wrestling with glm and other models who claim their
    ## class is glm or lm. It is possible a model with class "lm" could
    ## end up here because user has not implemented a method we expect
    ## but still says model is in class "lm".

    ## I'm adapting code from R predict.lm and predict.glm
    if (inherits(object, c("glm", "lm"))) {
        ## summary.survreg has no ... argument.
        if(inherits(object, "survreg")) dispersion <- 1.

        if (inherits(object, "glm")) {
            if (is.null(dispersion) || dispersion == 0) {
                dispersion <- summary(object, dispersion=dispersion)$dispersion
            }

            if (interval == "prediction") {
                stop("rockchalk::predictCI, prediction intervals not defined
            for glm in general. Try confidence intervals instead.")
            }
            ## scale = residual.scale in predict.glm
            if (is.null(scale) || scale == 0) {
                scale <- as.vector(sqrt(dispersion))
            }
        } else if (inherits(object, "lm")) {
            if (is.null(scale) || scale == 0) {
                scale <- summary(object)$sigma
            }
        }

        ## set se.fit = TRUE always, even if user doesn't want it.
        ## make them have it! Trying to make return structure more understandable.

        pred <- predict.lm(object, newdata = newdata, se.fit = TRUE, scale = scale,
                           type = "response", interval = "none", na.action = na.action)

        ## pred insides, at least:
        ## pred$fit      on linear predictor (==link) scale
        ## pred$se.fit   on linear predictor (==link) scale

        ## glm: use normal approximation. Calculate fit +/- on link scale first
        ## lm: use Wald-type CI with correct t value
        if (inherits(object, "glm")) {
            fit <- cbind(fit = pred$fit,
                         lwr = pred$fit - 1.96 * pred$se.fit,
                         upr = pred$fit + 1.96 * pred$se.fit)
        } else if (inherits(object, "lm")) {
            tval <- qt((1 - level)/2, lower.tail = FALSE, object$df.residual)
            fit <- cbind(fit = pred$fit,
                         lwr = pred$fit - tval * pred$se.fit,
                         upr = pred$fit + tval * pred$se.fit)
        }
        ## following does nothing to lm output, but does invert glm
        if (type == "response") fit <- family(object)$linkinv(fit)

        if(missing(newdata) && !is.null(na.act)) {
            fit <- napredict(na.act, fit)
            se.fit <- napredict(na.act, pred$se.fit)
        }
        pred <- list(fit = fit, se.fit = pred$se.fit, residual.scale = scale)
        return(pred)
    }

    ## Could add stanzas for model types here. This is commented out
    ## now because I don't know if the predict method for betareg works
    ## the way I expect.
    ## if (inherits(object, "betareg")){
    ##     pred <- predict(object, newdata = newdata , type = type)
    ##     se   <- sqrt(predict(object, newdata = newdata, type = "variance"))
    ##     fit <- cbind(fit = pred,
    ##                  lwr = pred - 1.96 * se,
    ##                  upr = pred + 1.96 * se)
    ##     pred <- list(fit = fit, se.fit = se)
    ##     return(pred)
    ## }


    msg <-
        paste("rockchalk:::predictCI. The regression object was not a lm, or glm,",
              "so I can't tell how to calculate a confidence interval.",
              "May I politely suggest you rewrite your predict function to accept",
              "the argument \"interval\" in a manner similar to predict.lm, or",
              "email the rockchalk maintainer with information about this unfamiliar",
              "regression you are trying to run.\n")
    ## TODO: Consider the stone age methods.
    ## object.b <- coef(object)
    ## object.linkPred <- newdata %*% object.b
    stop(msg)
}



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


## This is what I was using until rockchalk-1.7.6.0

## model.data <- function(model){
##     ## from nls, returns -1 for missing variables
##     lenVar <- function(var, data) tryCatch(length(eval(as.name(var),
##                          data, env)), error = function(e) -1)
##     fmla <- formula(model)
##     varNames <- all.vars(fmla) ## all variable names
##     fmla2 <- fmla
##     fmla2[[2L]] <- 0
##     varNamesRHS <- all.vars(fmla2)
##     varNamesLHS <- setdiff(varNames, varNamesRHS)
##     env <- environment(fmla)
##     if (is.null(env))
##         env <- parent.frame()
##     if (is.null(model$call$data)) stop("rockchalk:::model.data. \nPlease refit your regression model using the \ndata argument. Otherwise, I don't know how to re-construct the data structure.\n")
##     dataOrig <-  eval(model$call$data, environment(formula(model)))
##     dataOrigRN <- row.names(dataOrig)
##     n <- sapply(varNames, lenVar, data = dataOrig)
##     targetLength <- length(eval(as.name(varNamesRHS[1]), dataOrig, env))
##     varNames <- varNames[n == targetLength]
##     ldata <- lapply(varNames, function(x) {
##         myv <- eval(as.name(x), dataOrig, env)
##         row.names(myv) <- NULL
##         myv
##     }
##                     )

##         names(ldata) <- varNames
##     data <- data.frame(ldata[varNames])
##     if (!is.null(dataOrigRN)) row.names(data) <- dataOrigRN
##     ## remove rows listed in model's na.action
##     ## TODO: question: what else besides OMIT might be called for?
##     if ( !is.null(model$na.action)){
##         data <- data[ -as.vector(model$na.action), , drop = FALSE]
##     }
##     ## keep varNamesRHS that exist in datOrig
##     attr(data, "varNamesRHS") <- setdiff(colnames(data), varNamesLHS)
##     invisible(data)
## }
## NULL
