##' Estimate leave-one-variable-out regressions
##'
##' This is a convenience for analysis of multicollinearity in regression.
##' 
##' @param model a fitted regression model 
##' @return a list of fitted regressions, one for each omitted variable.
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @export
lmAuxiliary <- function(model){
  dat <- as.data.frame(model.matrix(model))
  ## ivnames <- attr(delete.response(terms(model)), "term.labels")
  ## previous does not work with transforms like poly
  hasIntercept <- attr(terms(model), "intercept")
  if (hasIntercept) dat <- dat[ , -1] # removes intercept. assumes intercept in column 1
  ivnames <- colnames(dat)
  cat("The following auxiliary models are being estimated and returned in a list:\n")

  results <- list()
  colnames(dat) <- gsub("`","",  colnames(dat)) #rm ticks on varnames
  for (i in ivnames) {
    fmla <- paste( "`",i,"`", " ~ ." , sep="") #add ticks in fmla
    fmla <- gsub("``","`",  fmla) # remove double ticks
    lmcall <- call("lm", formula(fmla, data=dat), quote(dat))
    results[[ i ]] <- maux <- eval(lmcall)
    print(formula(maux))
  }
 results
}

##' retrieves estimates of the coefficient of determination from a list of regressions
##'
##' Asks each regression model in a list for a summary and then reports the R-squares. 
##' @param auxRegs a list of fitted regression objects 
##' @return a numeric vector of the same length as auxRegs.
##' @author Paul E. Johnson <pauljohn@@ku.edu>
getAuxRsq <- function(auxRegs){
  auxRsq <- numeric(length=length(auxRegs))
  j <- 0
  for ( i in auxRegs ){
    j <- j + 1
    sumry <- summary(i)
    auxRsq[j] <- sumry$r.squared
  }

  names(auxRsq) <- names(auxRegs)
  
  invisible(auxRsq)
}

##' Converts the R-square to the variance inflation factor
##'
##' calculates vif = 1/(1-R-square)
##' 
##' @param rsq a vector of real values, presumably fitted R-squares 
##' @return a vector of vif estimates
##' @author Paul E. Johnson <pauljohn@@ku.edu>
getVIF <- function(rsq){
  vif <- 1/(1-rsq)
  
  invisible(vif)
}

##' Calculates the delta R-squares, also known as squared
##' semi-partial correlation coefficients.
##' 
##' The change in the R-square when a variable is removed from a
##' regression is called delta R-square. It is sometimes suggested as
##' a way to determine whether a variable has a substantial effect on
##' an outcome. This is also known as the squared semi-partial correlation
##' coefficient. 
##'
##' @export
##' @param model a fitted regression model 
##' @return a vector of estimates of the delta R-squares
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @examples
##' dat1 <- genCorrelatedData(N=250, means=c(100,100),
##' sds=c(30,20), rho=0.0,  stde = 7, beta=c(1.1, 2.4, 4.1, 0))
##' m1 <- lm(y ~ x1 + x2, data=dat1)
##' getDeltaRsquare(m1)
##' ## more problematic in presence of collinearity
##' dat2 <- genCorrelatedData(N=250, means=c(100,100),
##' sds=c(30,20), rho=0.6,  stde = 7, beta=c(1.1, 2.4, 4.1, 0))
##' m2 <- lm(y ~ x1 + x2, data=dat2)
##' getDeltaRsquare(m2)
getDeltaRsquare <- function(model){
  modeldrop1 <- drop1(model)
  RSS <- modeldrop1[1, "RSS"] ##Residual Sum of Squares
  deltaSS <- modeldrop1[ , "Sum of Sq"]
  SST = sum((model$model[, 1] - mean(model$model[, 1]))^2)
  deltaRsquare <- deltaSS/SST
  names(deltaRsquare) <- row.names(modeldrop1)
  na.omit(deltaRsquare)
}


##' Multi-collinearity diagnostics
##'
##' Conducts a series of checks for multicollinearity.
##' @param model a fitted regression model 
##' @return a list of the "auxiliary regressions" that were
##' fitted during the analysis
##' @export
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @example inst/examples/mcDiagnose-ex.R
mcDiagnose <- function(model){
  if (any(is.na(coef(model)))) stop("There are redundant variables in the model. Fix the specification before diagnosing multicollinearity")
  auxRegs <- lmAuxiliary(model)
  auxRsq <- getAuxRsq(auxRegs)
  vif <- getVIF(auxRsq)

  cat("Drum roll please! \n")
  cat("\n")
  cat("And your R_j Squareds are (auxiliary Rsq)\n")
  print(auxRsq)
  cat("The Corresponding VIF, 1/(1-R_j^2)\n")
  print(vif)
  cat("Bivariate Correlations for design matrix \n")
  mm <- model.matrix(model)[ , -1] ## data, omit intercept
  print(round(cor(mm[,]), 2))
  invisible(auxRegs)
}
