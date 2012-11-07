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



##' Creates the newdata frame required in predict.
##'
##' If not supplied with a focus list, newdata returns a data frame
##' with one row-- the central values (means and modes) of the
##' variables in the data that was used to fit the model.  To declare
##' some variables that the user wants to focus on, the user should
##' supply a fitted model "model" and a focus list "fl" of variable
##' values. The fl list must be a named list, using names of variables
##' from the regression formula.  It is not needed to call this
##' directly if one is satisfied with the results from predictOMatic.
##' @param model Required. Fitted regression model
##' @param fl Optional. "focus list" of variables. Named list of variables and values for which to create a new data object.
##' @param emf Optional. data frame used to fit model (not a model
##' frame, which may include transformed variables like
##' log(x1). Instead, use output from function \code{model.data}). It
##' is UNTRANSFORMED variables ("x" as opposed to poly(x,2).1 and
##' poly(x,2).2).
##' @return A data frame of x values that could be used as the data= argument in the original regression model. The attribute "varNamesRHS" is a vector of the predictor values.
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @export
##' @seealso \code{predictOMatic}
##' @example inst/examples/predictOMatic-ex.R
newdata <- function (model = NULL, fl = NULL, emf = NULL){
    if (is.null(emf)) emf <- model.data(model = model)
    varNamesRHS <- attr(emf, "varNamesRHS")
    emf <- emf[ , varNamesRHS]
    modelcv <- centralValues(emf)
    if (is.null(fl)) return(modelcv)
    if (sum(!names(fl) %in% varNamesRHS) > 0) stop(cat(c("Error. The focus list:  fl requests variables that are not included in the original model. The names of the variables in the focus list be drawn from this list: ",  varNamesRHS, "\n")))
    ## TODO: Consider "padding" range of fl for numeric variables so that we
    ## get newdata objects including the min and max values.

    mixAndMatch <- expand.grid(fl)
    ## TODO: Its OK to select columns this way, but no better way add names?
    unames <- colnames(modelcv)[!colnames(modelcv) %in% colnames(mixAndMatch)]
    newdf <- cbind(mixAndMatch, modelcv[  , unames])
    colnames(newdf) <- c(colnames(mixAndMatch), unames)
    newdf
}


##' Creates a "raw" (UNTRANSFORMED) data frame equivalent
##' to the input data that would be required to fit the given model.
##'
##' Unlike model.frame and model.matrix, this does not return transformed
##' variables.
##'
##' @param model A fitted regression model in which the data argument
##' is specified. This function will fail if the model was not fit
##' with the data option.
##' @return A data frame
##' @export
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @example inst/examples/model.data-ex.R
model.data <- function(model){
    #from nls, returns -1 for missing variables
    lenVar <- function(var, data) tryCatch(length(eval(as.name(var),
                         data, env)), error = function(e) -1)
    fmla <- formula(model)
    varNames <- all.vars(fmla) ## all variable names
    ## varNames includes d in poly(x,d), possibly other "constants"
    ## varNamesRHS <- all.vars(formula(delete.response(terms(model))))
    ## previous same as nls way?
    fmla2 <- fmla
    fmla2[[2L]] <- 0
    varNamesRHS <- all.vars(fmla2)
    varNamesLHS <- setdiff(varNames, varNamesRHS)
    env <- environment(fmla)
    if (is.null(env))
        env <- parent.frame()

    dataOrig <-  eval(model$call$data, environment(formula(model)))
    rndataOrig <- row.names(dataOrig)
    n <- sapply(varNames, lenVar, data=dataOrig)
    targetLength <- length(eval(as.name(varNamesLHS[1]), dataOrig, env))
    varNames <- varNames[ n == targetLength ]
    ldata <- lapply(varNames, function(x) eval(as.name(x), dataOrig, env))
    names(ldata) <- varNames
    data <- data.frame(ldata[varNames])
    if (!is.null(rndataOrig)) row.names(data) <- rndataOrig
    ## remove rows listed in model's na.action
    ## TODO: question: what else besides OMIT might be called for?
    if ( !is.null(model$na.action)){
        data <- data[ -as.vector(model$na.action),  , drop=FALSE]
    }
    ## keep varNamesRHS that exist in datOrig
    attr(data, "varNamesRHS") <- setdiff(colnames(data), varNamesLHS)
    invisible(data)
}



##' predictOMatic creates predicted values for a fitted regression model.
##'
##' If a "focus list" is supplied, predictOMatic supplies predicted
##' values only for those selected input values.
##
##' If no "focus list" is supplied, predictOMatic supplies a
##' prediction summary for each separate independent variable. That
##' is, in a model with formula y ~ x1 + x2 + x3, then separate tables of predicted values will be supplied, one for each of x1, x2, and x3.
##'
##' It may be important to make sure that diagnostic plots and
##' summaries of predictions are calculated with the exact same data
##' that was used to fit the model. The function \code{model.data} is
##' intended to facilitate that comparison. One can fit a model, use
##' model.data to get the data that was used, and then use that
##' extracted data to decide on values to be set in the focus list.
##'
##' For example, create a copy of the data from a model m1 with
##'
##' m1dat <- model.data(m1)
##'
##' and then use m1dat to select values that might be predicted in
##' a command like
##'
##' predictOMatic( m1, fl =
##' list("x1" = median(m1dat$x1), "x2"=c(1,2,3), "x3" = quantile(m1dat$x3))
##'
##' @param model Required. A fitted regression model
##' @param fl (focus list). Optional. A named list of variables and
##' values, such as fl = list("x1" = c(1,2,3), "x2" = c("M","F"), "x3"
##' = quantile(dat$x3)). Must name only predictors that are fitted in
##' \code{model}.  Need not include all predictors in a model.
##' Predictor variables in \code{model} that are not named in fl will
##' be set to mean or mode values. See details and examples.
##' @param divider Optional. If fl is not specified, automatic selection of
##' predictor values is employed. \code{divider} determines the method of
##' selection. Should be one of c("quantile","std.dev","table"). This
##' determines whether values selected are quantile values, values
##' based on the mean plus-or-minus standard deviation values, or a
##' table of most frequently occurring values. Documentation for the
##' details can be found in the functions \code{cutByTable},
##' \code{cutByQuantile}, and \code{cutBySD}.
##' @param n If fl is not specified, automatic selection of predictor
##' values is employed. This determines the number of values for which
##' predictions are sought.
##' @param ... Optional arguments to be passed to the predict function
##' @return A data frame or a list of data frames.
##' @export
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @example inst/examples/predictOMatic-ex.R
predictOMatic <- function(model = NULL, fl = NULL, divider = "quantile", n = 3,  ...) {
    dots <- list(...)
    dotnames <- names(dots)
    ## next should give c('digits', 'alphaSort')
    nnames <- names(formals(rockchalk::summarizeNumerics))[-1L]
    ## names that need keeping if in dots:
    keepnames <- dotnames %in% nnames

    emf <- model.data(model = model)
    varNamesRHS <- attr(emf, "varNamesRHS")

    if(missing(fl) || is.null(fl)){
        flxxx <- list()
        nd <- lapply (varNamesRHS, function(x) {
            if (is.numeric(emf[ ,x])) {
                divider <- match.arg(tolower(divider),
                                  c("quantile", "std.dev.","table"))
                flxxx[[x]] <- switch(divider,
                         table = rockchalk:::cutByTable(emf[,x], n),
                         quantile = rockchalk:::cutByQuantile(emf[,x], n),
                         "std.dev." = rockchalk:::cutBySD(emf[,x], n),
                         stop("unknown 'divider' algorithm"))
            } else {
                flxxx[[x]] <- names(rockchalk:::cutByTable(emf[ ,x], n))
            }
            ndnew <- newdata(model, fl=flxxx[x], emf = emf)
            fit <- predict(model, newdata = ndnew, ...)
            ndnew <- cbind(fit, ndnew)
            attr(ndnew, "flnames") <- x
            row.names(ndnew) <- names(flxxx[[x]])
            ndnew
        } )
       names(nd) <- varNamesRHS
    }else{
        flnames <- names(fl)
        nd <- newdata(model, fl, emf = emf)
        fit <- predict(model, newdata = nd, ...)
        nd <- cbind(fit, nd)
        attr(nd, "flnames") <- flnames
    }
    invisible(nd)
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


