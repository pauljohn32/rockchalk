### Paul Johnson
### Adapted from ideas in post in r-help by Dave Armstrong May 8, 2006


###tight means one column per fitted model
###not tight means 2 columns per fitted model

###incoming= either one regression model or a list of regresion models
###title = a string
###modelLabels= a VECTOR of character strings
### varLabels= a LIST of labels linked to variable names (see examples)
### tight= BOOLEAN, indicates results should be on one tight column or two for each model
### showAIC= BOOLEAN should the AIC be displayed for each model?
### lyx=create a table suitable for inclusion in a lyx float.



#' outreg
#'
#' Creates a publication quality regression result table for models fitted by
#' lm and glm. Can be called within Sweave documents.
#'
#' Uses a bunch of tedious "cat" statements to display the regression model in
#' LaTeX output.
#'
#' @param incoming A single regression model or an R list of regression models.
#' @param title A title to be displayed on the top of the LaTeX regression
#' table.
#' @param label A string to be used as a LaTeX label in the table to be
#' created.
#' @param modelLabels If "incoming" is a list of fitted models, this can supply
#' strings to separately label each one in the output table.
#' @param varLabels To beautify the words associated with the fitted variables,
#' use this argument.
#' @param tight If T, parameter estimates and standard errors are printed in a
#' single column, thus allowing several models to be displayed side by side.
#' If F, parameter estimates and standard errors are printed side by side.
#' @param showAIC if TRUE, the AIC estimate is included with the diagnostic values
#' @param lyx If the LaTeX output is going into a LaTeX table that already
#' exists, as, for example, it might be in LyX when the Sweaved code is inside
#' a floating table object, set this to TRUE. Otherwise, the value of FALSE causes the
#' output to include the output that creates more of the LaTeX table
#' boilerplate.
#' @export outreg
#' @return None
#' @keywords regression
#' @note There are many R packages that can be used to create LaTeX regression
#' tables. memisc, apsrtable, xtables, and rms are some. This "outreg" version
#' was in use in our labs before we were aware that those packages were in
#' development. It is not intended as a competitor, it is just a slightly
#' different version of the same that is more suited to our needs.
#' @author Paul E. Johnson \email{<pauljohn@@ku.edu>}
#' @references Citation:
#' @examples
#' x1 <- rnorm(100)
#' x2 <- rnorm(100)
#' y1 <- 5*rnorm(100)+3*x1 + 4*x2
#'
#' y2 <- rnorm(100)+5*x2
#' m1 <- lm (y1~x1)
#' m2 <- lm (y1~x2)
#' m3 <- lm (y1 ~ x1 + x2)
#' gm1 <- glm(y1~x1)
#'
#' outreg(m1,title="My One Tightly Printed Regression", lyx=FALSE )
#'
#' outreg(m1,tight=FALSE,modelLabels=c("Fingers"), title="My Only Spread Out Regressions" ,lyx=FALSE)
#'
#' outreg(list(m1,m2),modelLabels=c("Mine","Yours"),varLabels=list(x1="Billie"), title="My Two Linear Regressions Tightly Printed" ,lyx=FALSE)
#'
#' outreg(list(m1,m2),modelLabels=c("Whatever","Whichever"), title="My Two Linear Regressions Not Tightly  Printed", showAIC=FALSE, lyx=FALSE)
#'
#' outreg(list(m1,m2,m3),title="My Three Linear Regressions", lyx=FALSE)
#'
#' outreg(list(m1,m2,m3),tight=FALSE,modelLabels=c("I Love love love really long titles","Hate Long","Medium"), lyx=FALSE)
#'
#' outreg(list(gm1),modelLabels=c("GLM"), lyx=FALSE)
#'
#' outreg(list(m1,gm1),modelLabels=c("OLS","GLM"), lyx=FALSE)
#'
outreg <- function(incoming, title, label, modelLabels=NULL, varLabels=NULL, tight=TRUE, showAIC=FALSE, lyx=TRUE){

  modelList <- NULL

  ## was input just one model, or a list of models?  ###
  if ( "lm" %in% class(incoming)) { ##just one model input
    nmodels <- 1
    modelList <- list(modl1=incoming)
  } else {
     modelList <- incoming
  }
  nmodels <- length(modelList)
  ##TODO modelLabels MUST have same number of items as "incoming"
  ## but following crashes
  ## if(modelLabels != NULL){
  ##     if (length(modelLabels) != nmodels ) stop("the number of elements in modelLabels must match the the number of models in the incoming model list")
  ## }
  ## Get a regression summary object for each fitted model
  summaryList <- list()
  fixnames <- vector()
  myModelClass <- vector()
  coefsum <- list()

  i <-  1
  for (model in modelList){
    summaryList[[i]] <- ssm <- summary(model)
    coefsum[[i]] <- coef(ssm)
    fixnames <- unique( c( fixnames, names(coef(model))))
    myModelClass[i] <- class(model)[1]
    i <- i+1
  }



  ###If you are just using LaTeX, you need these
if (lyx == FALSE || !missing(title) || !missing(label)){
 cat("\\begin{table}\n ")
   if (missing(title)) title <- "A Regression"
   if (missing(label)) label <- "regrlabl"
   cat("\\caption{",title,"}\\label{",label,"}\n ")
 }
  cat("\\begin{center}\n ")
  nColumns <- ifelse(tight, 1+nmodels, 1 + 2*nmodels)
  cat(paste("\\begin{tabular}{*{",nColumns,"}{l}}\n ", sep=""))
  cat("\\hline\n ")

  ### Put model labels on top of each model column, if modelLabels were given
  if (!is.null(modelLabels)){
    cat("     ")
    for (modelLabel in modelLabels){
      if (tight == TRUE) {
        cat(paste("&", modelLabel))
      }else{
        cat(paste("&\\multicolumn{2}{c}{",modelLabel,"}",sep=""))
      }
    }
    cat (" \\\\\n ")
  }

  ### Print the headers "Estimate" and "(S.E.)", output depends on tight or other format
  if (tight == TRUE){
    cat("               ", rep (" &Estimate ", nmodels), "\\\\\n")
    #for (i in 1:nmodels) { cat (" & Estimate ") }
    #cat(" \\\\\n")
    cat("               ", rep (" &(S.E.) ", nmodels), "\\\\\n")

#    cat("             ")
#    for (i in 1:nmodels) {  cat (" & (S.E.) ") }
#    cat(" \\\\\n")
  }else{
      cat("               ", rep (" & Estimate & (S.E.) ", nmodels), "\\\\\n", fill= FALSE)
  #  cat("             ")
  #  for (i in 1:nmodels) { cat (" & Estimate & S.E.") }
  #  cat(" \\\\\n")
  }


  cat("\\hline \n\\hline\n ")


  ### Here come the regression coefficients
  for (regname in fixnames){
    if ( !is.null(varLabels[[regname]]) ) { cat(paste("",varLabels[[regname]]), sep="")}

    else {cat(paste("", regname), sep="")}

    for (model in modelList) {
      est <- coef(model)[regname]
      se <- sqrt(diag(vcov(model)))[regname]
      if ( !is.na(est) ) {
        cat (paste("   &   ", round(est,3)))
        pval <- pt(abs(est/se), lower.tail=FALSE, df = model$df.residual)
        if (pval < 0.025) cat("*")

        if (tight == FALSE) {
          cat (paste("   &   (", round(se,3),")",sep=""))
        }
      } else {
        cat ("   & . ")
        if (tight == FALSE) cat (" & " )
      }
    }
    cat (" \\\\\n ")

    if (tight == TRUE){
      for (model in modelList) {
        est <- coef(model)[regname]
        if (!is.na(est)) cat (paste("   &   (",round(sqrt(diag(vcov(model)))[regname],3)),")",sep="")
        else cat("   &  ")
      }
      cat (" \\\\\n ")
    }
   }
  cat("\\hline \n")


  ### Print a row for the number of cases
  cat(paste("N"), sep="")
  for (model in summaryList) {
    myDF <- sum( model$df[-3] ) #omit third value from df vector
    cat (paste("   &   ", myDF))
    if (tight == FALSE) cat("    &")
  }
  cat (" \\\\\n ")


  ### Print a row for the root mean square error
  if ("lm" %in% myModelClass) {
       cat(paste("$RMSE$"),sep="")
       for (model in summaryList) {
         cat( paste("       &", if(is.numeric(model$sigma)) round(model$sigma,3)))
         if (tight == FALSE) cat("    &")
       }
       cat ("  \\\\\n ")
     }


  ### Print a row for the R-square
  if ("lm" %in% myModelClass) {
     cat(paste("$R^2$"),sep="")
     for (model in summaryList) {
       cat( paste("       &", if(is.numeric(model$r.square))round(model$r.square,3)))
       if (tight == FALSE) cat("    &")
     }
     cat ("  \\\\\n ")
   }

#"adj.r.squared"

    ### Print a row for the adj-R-square
  if ("lm" %in% myModelClass) {
     cat(paste("adj $R^2$"), sep="")
     for (model in summaryList) {
       cat( paste("       &", if(is.numeric(model$adj.r.square))round(model$adj.r.square,3)))
       if (tight == FALSE) cat("    &")
     }
     cat ("  \\\\\n ")
   }


  ## Print a row for the model residual deviance
   if ("glm" %in% myModelClass) {
    cat(paste("$Deviance$"),sep="")
    for (model in summaryList) {
      cat (paste("      &", if(is.numeric(model$deviance))round(model$deviance,3)))
      if (tight == FALSE) cat("      &")
    }
    cat ("  \\\\\n ")
  }

  ### Print a row for the model's fit, as -2LLR
  if ("glm" %in% myModelClass) {
    cat (paste("$-2LLR (Model \\chi^2)$"),sep="")
    for (model in modelList) {
      if (is.numeric(model$deviance)){
        n2llr <- model$null.deviance - model$deviance
        cat (paste("      &", round(n2llr,3)))
        gmdf <- model$df.null - model$df.residual + 1

        if (pchisq(n2llr, df= gmdf, lower.tail=FALSE) < 0.05) {cat ("*")}
      }

      else {
        cat ("    &")
      }
      if (tight == FALSE) cat("      &")
    }
    cat ("  \\\\\n ")
  }



  ## Print a row for the model's fit, as -2 LLR
  ### Can't remember why I was multiplying by -2

  if (showAIC == TRUE) {
    cat(paste("$AIC$"),sep="")
    for (model in modelList) {
      cat (paste("      &", if(is.numeric(AIC(model)))round(AIC(model),3)))
      if (tight == FALSE) cat("      &")
    }
    cat ("  \\\\\n ")
  }



   cat("\\hline\\hline\n")
   cat ("* $p \\le 0.05$")
   cat("\\end{tabular}\n")
   cat("\\end{center}\n")
  if (lyx == FALSE || !missing(title) || !missing(label)){
      cat("\\end{table}\n")
  }

}
