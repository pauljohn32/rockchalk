
##' Creates a cross tabulation with counts and column percents
##'
##' This function is pronounced "presentable"!
##' 
##' The original purpose was to create a particular kind of cross
##' tabulation that I ask for in class: counts with column
##' percentages. Requests from users have caused a bit more generality
##' to be built into the function. Now, optionally, it will provide
##' row percents.
##'
##' Please bear in mind the following. This function creates a list of
##' tables of partial information, which are then assembled in various
##' ways by the print method that is supplied for objects of type
##' pctable. Hence, if one were to run
##'
##' tab <- pctable(y ~ x, data = dat)
##'
##' or
##' 
##' tab <- pctable("y", "x", data = dat)
##' 
##' a lovely table will appear on the screen, but the thing
##' tab itself has more information and a less beautiful structure.
##' 
##' In order to see the individual pieces it is necessary to run
##' tab[[1]], or tab[[2]], or tab[[3]], because if one types simply
##'
##' tab
##'
##' then the method print.pctable(tab) is called, and that assembles
##' the object into (my opinion of) a presentable form. The print method
##' has argumnets \code{rowpct} and \code{colpct} that determine which
##' percentages are included in the presentation. 
##' @param rv A row variable name or a two-sided formula.
##' @param ...
##' @export
##' @return A list with tables (count, column percent, row percent) as
##' well as a copy of the call.
##' @author Paul Johnson <pauljohn@@ku.edu>
pctable <- function(rv, ...)
{
    UseMethod("pctable")
}
NULL


##' 
##' @param rv row variable name
##' @param cv col variable name
##' @param data dataframe
##' @param rvlab Optional: row variable label
##' @param cvlab Optional: col variable label
##' @param colpct Default TRUE: are column percentags desired in the
##' presentation of this result?
##' @param rowpct Default FALSE: are row percentages desired in the
##' presentaiton of this result
##' @param exclude Default NULL, all values displayed (incl missing)
##' @param rounded Default FALSE, rounds to 10's for privacy purposes
##' @return list of tables: Counts (with margins), row percent, column
##' percent. Caution: See details about printing and inspecting the
##' contents of this returned object.
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @examples
##' dat <- data.frame(x = gl(4, 25),
##'                   y = sample(c("A", "B", "C", "D", "E"), 100, replace= TRUE))
##' pctable.default(y, x, dat)
##' pctable.default("y", "x", dat)
##' tab <- pctable.default(y, x, dat)
##' print(tab, rowpct = TRUE, colpct = TRUE)
##'
pctable.default <- function(rv, cv, data = parent.frame(),
                            rvlab = NULL, cvlab = NULL,
                            colpct = TRUE, rowpct = FALSE,
                            exclude = c(NA, NaN), rounded = FALSE)
{
    rvlabel <- if (!missing(rv)) deparse(substitute(rv))
    cvlabel <- if (!missing(cv)) deparse(substitute(cv))
    rvlab <- if (is.null(rvlab)) rvlabel else rvlab
    cvlab <- if (is.null(cvlab)) cvlabel else cvlab

    rvin <- eval(substitute(rv), envir = data, enclos = parent.frame())
    cvin <- eval(substitute(cv), envir = data, enclos = parent.frame())
    
    t1 <- table(rvin, cvin, dnn = c(rvlab, cvlab), exclude = exclude)
    rownames(t1)[is.na(rownames(t1))] <- "NA" ## symbol to letters
    colnames(t1)[is.na(colnames(t1))] <- "NA"
    if (rounded) t1 <- round(t1, -1)
    t2 <- addmargins(t1, c(1,2))
    t1colpct <- round(100*prop.table(t1, 2), 1)
    t1rowpct <- round(100*prop.table(t1, 1), 1)
    t1colpct <- apply(t1colpct, c(1,2), function(x) gsub("NaN", "", x))
    t1rowpct <- apply(t1rowpct, c(1,2), function(x) gsub("NaN", "", x))
    ## t3 <- t2
    ## for(j in rownames(t1colpct)){
    ##     for(k in colnames(t1colpct)){
    ##         t3[j, k] <- paste0(t2[j, k], "(", t1colpct[j, k], "%)")
    ##     }
    ## }
    res <- list("count" = t2, "colpct" = t1colpct, "rowpct" = t1rowpct, call = match.call())
    class(res) <- "pctable"
    print(res, colpct = colpct, rowpct = rowpct)
    invisible(res)
}
NULL







##' Creates a cross tabulation with counts and column percents
##'
##' Pronounced "presentable". The original purpose was to create a
##' more pleasant user interface to ask for a particular kind of cross
##' tabulation: counts with column percentages.
##'
##' Please bear in mind the following. When the function runs, it
##' displays one presentation of the inner parts that are created.
##' The inner parts are also saved to an output object, which can then
##' be re-printed with various styles.
##'
##' The returned object is a list of tables of partial information:
##' counts, column percents, row percents.  Those bits are then
##' assembled for presentation in various ways by the print method
##' that is supplied for objects of type pctable. Hence, if one were
##' to run
##'
##' tab <- pctable(y ~ x, dat)
##'
##' the display will be the column percent table I want people to see,
##' but tab actually has more information inside it. In order to see
##' the individual pieces it is necessary to run tab[[1]], or
##' tab[[2]], or tab[[3]].
##'
##' Note, to alter the display of this result, the following are allowed
##'
##' print(tab, rowpct = TRUE, colpct = FALSE)
##' print(tab, rowpct = TRUE, colpct = TRUE)
##'
##' That is to say, the settings for rowpct and colpct that are used
##' in the original call to pctable are not "permanent"
##' characteristics of the table. The print method can override them.
##'
##' @param formula A two sided formula.  
##' @param data A data frame
##' @param rvlab Optional character string for row variable label
##' @param cvlab Optional character string for column variable label
##' @param colpct Default TRUE: Are column percents desired in the presentation?
##' @param rowpct Default FALSE: Are row percents desired in the presentation?
##' @param exclude As in base::table, this is a vector of labels that
##' are to be excluded from the calculation of counts, row or column
##' percents. To see all labels, including missings, set exclude = NULL.
##' @param rounded If TRUE, rounds observed counts to nearest 10 (to
##' protect privacy).
##' @param ... Additional arguments that might adjust printout.
##' @param subset As in many R functions. Should be the format
##' required by functions like model.frame.
##' @export
##' @return A list of tables, one each for counts, column percents,
##' and row percents.
##' @author Paul Johnson <pauljohn@@ku.edu>>
##' @examples
##' dat <- data.frame(x = gl(4, 25),
##'                   y = sample(c("A", "B", "C", "D", "E"), 100, replace= TRUE))
##' pctable(y ~ x, dat)
##' pctable(y ~ x, dat, exclude = NULL)
##' pctable(y ~ x, dat, rvlab = "My Outcome Var", cvlab = "My Columns")
##' pctable(y ~ x, dat, rowpct = TRUE, colpct = FALSE)
##' pctable(y ~ x, dat, rowpct = TRUE, colpct = TRUE)
##' pctable(y ~ x, dat, rowpct = TRUE, colpct = TRUE, exclude = NULL)
##' tab <- pctable(y ~ x, dat, rvlab = "Outcome", cvlab = "Predictor")
##'
pctable.formula <- function(formula, data = NULL,  rvlab = NULL,
                            cvlab = NULL, colpct = TRUE, rowpct = FALSE,
                            exclude = c(NA, NaN), rounded = FALSE,
                            ..., subset = NULL)
    
{
    if (missing(data) || !is.data.frame(data)) stop("pctable requires a data frame")
    if (missing(formula) || (length(formula) != 3L))
        stop("pctable requires a two sided formula")
    mt <- terms(formula, data = data)
    if (attr(mt, "response") == 0L) stop("response variable is required")
    mf <- match.call(expand.dots = FALSE)
    keepers <- match(c("formula", "data", "subset", "na.action"), names(mf), 0L)
    mf <- mf[c(1L, keepers)]
    mf$drop.unused.levels <- FALSE
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())
    ## response is column 1
    rvlab <- if (missing(rvlab)) colnames(mf)[1] else rvlab
    cvlab <- if (missing(cvlab)) colnames(mf)[2] else cvlab
    
    res <- pctable.default(mf[[1L]], mf[[2L]], data = mf,
                           rvlab = rvlab, cvlab = cvlab,
                           colpct = colpct, rowpct = rowpct,
                           exclude = exclude, rounded = rounded)
    invisible(res)
}
NULL


##' Method for variable names as character strings 
##'
##' rowvar must be a quoted string. colvar may be a string
##' or just a variable name (which this method will coerce to a string)
##' @param rowvar string in quotes!
##' @param colvar character 
##' @param data 
##' @param rvlab 
##' @param cvlab 
##' @param colpct 
##' @param rowpct 
##' @param exclude 
##' @param rounded 
##' @param ... 
##' @param subset 
##' @return 
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @examples
##' dat <- data.frame(x1 = gl(4, 25, labels = c("Good", "Bad", "Ugly", "Indiff")),
##'                  x2 = gl(5, 20, labels = c("Denver", "Cincy", "Baltimore", "NY", "LA")), 
##'                  y = sample(c("A", "B", "C", "D", "E"), 100, replace= TRUE))
##'##' pctable("y", "x1", dat)
##' pctable("y", x1, dat)
##'
pctable.character <- function(rowvar, colvar, data = NULL, rvlab = NULL,
                              cvlab = NULL, colpct = TRUE,
                              rowpct = FALSE, exclude = c(NA, NaN), rounded = FALSE, 
                              ..., subset = NULL)
    
{
    if (missing(data) || !is.data.frame(data)) stop("pctable requires a data frame")
    ## colvar <- if (!is.character(colvar)) deparse(substitute(colvar)) else colvar
    colvar <- as.character(substitute(colvar))[1L]
    
    rvlab <- if (missing(rvlab)) rowvar else rvlab
    cvlab <- if (missing(cvlab)) colvar else cvlab
    
    t1 <- with(data, table(data[[rowvar]], data[[colvar]], dnn = c(rvlab, cvlab), exclude = exclude))
    rownames(t1)[is.na(rownames(t1))] <- "NA" ## symbol to letters
    colnames(t1)[is.na(colnames(t1))] <- "NA"
    if (rounded) t1 <- round(t1, -1)
    t2 <- addmargins(t1, c(1,2))
    t1colpct <- round(100*prop.table(t1, 2), 1)
    t1rowpct <- round(100*prop.table(t1, 1), 1)
    t1colpct <- apply(t1colpct, c(1,2), function(x) gsub("NaN", "", x))
    t1rowpct <- apply(t1rowpct, c(1,2), function(x) gsub("NaN", "", x))
   
    res <- list("count" = t2, "colpct" = t1colpct, "rowpct" = t1rowpct, call = match.call())
    class(res) <- "pctable"
    print(res, colpct = colpct, rowpct = rowpct)
    invisible(res)
}
NULL




##' Display pctable objects
##'
##' This is not very fancy.
##' @param tab A pctable object
##' @param colpct Default TRUE: include column percentages?
##' @param rowpct Default FALSE: include row percentages?
##' @return none
##' @author Paul Johnson <pauljohn@@ku.edu>
print.pctable <- function(tab, colpct = TRUE, rowpct = FALSE){
    count <- tab[["count"]]
   
    t3 <- count
    if (colpct && !rowpct) {
        cpct <- tab[["colpct"]]
        for(j in rownames(cpct)){
            for(k in colnames(cpct)){
                t3[j, k] <- paste0(count[j, k], "(", cpct[j, k], "%)")
            }
        }
        cat("Count (column %)\n")
        print(t3)
        return(invisible(t3))
    }

    ## rowpct == TRUE< else would have returned
    rpct <- tab[["rowpct"]]
    for(j in rownames(rpct)){
        for(k in colnames(rpct)){
            t3[j, k] <- paste0(count[j, k], "(", rpct[j, k], "%)")
        }
    }
    
    if (!colpct) {
        cat("Count (row %)\n")
        print(t3)
        return(invisible(t3))
    } else {
        cpct <- tab[["colpct"]]
        t4 <- array("", dim = c(1, 1) + c(2,1)*dim(tab$colpct))
        t4[seq(1, NROW(t4), 2), ] <- t3
        rownames(t4)[seq(1, NROW(t4), 2)] <- rownames(t3)
        rownames(t4)[is.na(rownames(t4))] <- "" 
        colnames(t4) <- colnames(t3)
        for(j in rownames(tab[["colpct"]])) {
            for(k in colnames(tab[["colpct"]])){
                t4[1 + which(rownames(t4) == j) ,k] <- paste0(tab[["colpct"]][j, k], "%")
            }

        }
        
        names(dimnames(t4)) <- names(dimnames(count))
               
        cat("Count (row %)\n")
        cat("column %\n")
        print(t4, quote = FALSE)
        return(invisible(t4))
    }
}
NULL
