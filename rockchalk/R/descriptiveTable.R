##' Summary stats table-maker for regression users
##'
##' rockchalk::summarize does the numerical calculations
##' 
##' This is, roughly speaking, doing the right thing, but
##' not in a clever way. For the categorical variables, the
##' only summary is proportions.
##'
##' @param object A fitted regression or an R data.frame, or any
##'      other object type that does not fail in code{model.frame(object)}.
##' @param stats Default is a vector c("mean", "sd", "min", "max"). Other
##'      stats reported by rockchalk::summarize should work fine as well
##' @param digits 2 decimal points is default
##' @param probs Probability cut points to be used in the calculation
##'      of summaries of numeric variables.  Default is c(0, 0.5, 1), meaning
##'      \code{min, median, max}.
##' @param varLabels A named vector of variables labels, as in outreg function.
##'      Format is c("oldname"="newlabel").
##' @param ... Other arguments passed to rockchalk::summarizeNumerics and
##'      summarizeFactors.
##' @importFrom kutils mgsub
##' @export
##' @return a character matrix
##' @author Paul Johnson \email{pauljohn@@ku.edu}
##' @examples
##' dat <- genCorrelatedData2(1000, means=c(10, 10, 10), sds = 3, 
##'                           stde = 3, beta = c(1, 1, -1, 0.5))
##' dat$xcat1 <- factor(sample(c("a", "b", "c", "d"), 1000, replace=TRUE))
##' dat$xcat2 <- factor(sample(c("M", "F"), 1000, replace=TRUE), levels = c("M", "F"),
##' labels = c("Male", "Female"))
##' dat$y <- dat$y + contrasts(dat$xcat1)[dat$xcat1, ] %*% c(0.1, 0.2, 0.3)
##' m4 <- lm(y ~ x1 + x2  + x3 + xcat1 + xcat2, dat)
##' m4.desc <- descriptiveTable(m4)
##' m4.desc
##' ## Following may cause scientific notation, want to avoid.
##' dat <- genCorrelatedData2(1000, means=c(10, 100, 400), 
##'                  sds = c(3, 10, 20), stde = 3, beta = c(1, 1, -1, 0.5))
##' m5 <- lm(y ~ x1 + x2  + x3, dat)
##' m5.desc <- descriptiveTable(m5, digits = 4)
##' m5.desc
##' 
descriptiveTable <- function(object, stats = c("mean", "sd", "min", "max"), 
                            digits = 4, probs = c(0, .5, 1), varLabels, ...){
    mc <- match.call(expand.dots = TRUE)
    dots <- list(...)
    options.orig <- options()
    options(scipen=20)
    on.exit(options(options.orig))
    
    dat <- model.frame(object)
    arglist <- list(dat = dat, stats = stats, digits = digits, maxLevels = 20)
    arglist <- modifyList(arglist, dots)
    summ.dat <- do.call(rockchalk::summarize, arglist)
    reslt <- data.frame(variable = rownames(summ.dat[["numerics"]]), 
                                    summ.dat[["numerics"]][stats[stats %in% names(summ.dat[["numerics"]])]],
                        stringsAsFactors = FALSE)
    numbers <- names(which(sapply(reslt, is.numeric)))
    for(j in numbers) reslt[ , j] <- formatC(reslt[ , j], digits = digits)
    
    reslt2 <- vector("list", length = length(summ.dat[["factors"]]))
    names(reslt2) <- names(summ.dat[["factors"]])
    for(j in names(summ.dat[["factors"]])){
        tab <- summ.dat[["factors"]][[j]]
        tab.prop <- prop.table(tab[["table"]])
        reslt2[[j]] <- data.frame(variable=names(tab.prop), 
                                  mean = formatC(tab.prop, digits = digits), 
                                  stringsAsFactors = FALSE)
        reslt2[[j]] <- rbind(data.frame(variable = j, mean = "", 
                                        stringsAsFactors = FALSE), 
                             reslt2[[j]])
    }
   
    reslt3 <- do.call(rbind, reslt2)
    
    reslt4 <- rbindFill(reslt, reslt3)
    reslt4[is.na(reslt4)] <- ""
    if(!missing(varLabels) && !is.null(varLabels)){
        reslt4$variable <- kutils::mgsub(names(varLabels), varLabels, reslt4$variable)
    }
    reslt4
}
