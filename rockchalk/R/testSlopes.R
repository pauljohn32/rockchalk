##' Hypothesis tests for Simple Slopes Objects
##'
##' The user must first run plotSlopes, and then supply that output object
##' to this function.
##'
##' Consider a regression, for example
##'
##' y <- b0 + b1*x1 + b2*x2 + b3*(x1*x2) + b4*x3 + ... + error
##'
##' If \code{plotSlopes} has been run with the variable plotx="x1" and the
##' variable modx="x2", then there will be several plotted lines,
##' one for various values of x2.  We wonder if the combined
##' effect of x1 is statistically significantly different from 0
##' for each of those values of x2.
##'
##' This function performs a test of the null hypothesis of the slope
##' of each fitted line in a \code{plotSlopes} object is statistically
##' significant from zero. A simple t-test for each line is offered.
##' No correction for the conduct of multiple hypothesis tests (no
##' Bonferroni correction).
##'
##' In addition, the so-called Johnson-Neyman  (Johnson-Neyman, 1936;
##' Preacher, Curran, and Bauer, 2006) interval is calculated and a
##' couple of efforts are made to render it graphically.  Where the
##' t-test considers the question, is the slope of the line (b1 +
##' b3*x2) different from 0, the J-N approach asks "for which values
##' of x2 will that plotted line be statistically significant.
##'
##' @param plotSlopesObject Output from the plotSlopes function
##' @return A list including the hypothesis test table. For numeric modx variables, also the Johnson-Neyman (J-N) interval boundaries.
##' @export
##' @import car
##' @seealso plotSlopes
##' @author Paul E. Johnson <pauljohn@@ku.edu>
##' @references
##' Preacher, Kristopher J, Curran, Patrick J.,and Bauer, Daniel J. (2006).
##' Computational Tools for Probing Interactions in Multiple Linear
##' Regression, Multilevel Modeling, and Latent Curve Analysis.
##' Journal of Educational and Behavioral Statistics. 31,4, 437-448.
##'
##' Johnson, P.O. and Neyman, J. (1936). "Tests of certain linear
##' hypotheses and their applications to some educational problems.
##' Statistical Research Memoirs, 1, 57-93.

##' @examples
##' library(car)
##' m3 <- lm(statusquo ~ income * sex, data = Chile)
##' m3ps <-plotSlopes(m3, modx = "sex", plotx = "income")
##' testSlopes(m3ps)
##'
##' m4 <- lm(statusquo ~ region * income, data= Chile)
##' m4ps <- plotSlopes(m4, modx = "region", plotx = "income", plotPoints = FALSE)
##' testSlopes(m4ps)
##'
##'
##' m5 <- lm(statusquo ~ region * income + sex + age, data= Chile)
##' m5ps <- plotSlopes(m5, modx = "region", plotx = "income")
##' testSlopes(m5ps)
##'
##' m6 <- lm(statusquo ~ income * age + education + sex + age, data=Chile)
##' m6ps <- plotSlopes(m6, modx = "income", plotx = "age")
##' testSlopes(m6ps)
##'
##' ## Finally, if model has no relevant interactions, testSlopes does nothing.
##' m9 <- lm(statusquo ~ age + income * education + sex + age, data=Chile)
##' m9ps <- plotSlopes(m9, modx = "education", plotx = "age", plotPoints = FALSE)
##' testSlopes(m9ps)

testSlopes <- function(plotSlopesObject) {
  pso <- plotSlopesObject ##shorter variable name
  model <-  eval(parse(text=pso$call$model))
  modx <- pso$call$modx
  plotx <- pso$call$plotx
  modxVar <- pso$newdata[ , modx]
  modxVals <- pso$modxVals
  ivs <- attr(terms(model), "term.labels")
  bs <- coef(model)
  V <- vcov(model)
  res <- NULL

  relevantInteractions <- c(paste(plotx, ":", modx, sep = ""),
                            paste(modx, ":", plotx, sep = ""))

  if (any(relevantInteractions %in% ivs)) {
    interactionsIn <- relevantInteractions[which(relevantInteractions %in%
                                                 ivs)]
    if (!is.factor(modxVar)) {
      bmodx <- NULL
      bplotx <- bs[plotx]

      bmodx <- bs[interactionsIn]
      bsimple <- bplotx + bmodx * modxVals
      covbsimple <- cbind(1, modxVals^2, 2 * modxVals) %*%
        c(V[plotx, plotx], V[names(bmodx), names(bmodx)], V[plotx, names(bmodx)])
      tbsimple <- bsimple/sqrt(covbsimple)
      testSlopes <- data.frame(modx = modxVals, b = bsimple, se = sqrt(covbsimple),
                               t = tbsimple, p = 2 * pt(abs(tbsimple), df = model$df.residual,
                                               lower.tail = FALSE))
      colnames(testSlopes) <- c(deparse(modx), "slope", "Std. Error", "t value", "Pr(>|t|)")
    } else {
      mcoef <- coef(model)
      modxContrastNames <- c(grep(plotx,  grep(modx, names(mcoef), value = TRUE),  value=TRUE))
      slope <- mcoef[modxContrastNames] + coef(model)[plotx]
      seslope <- sqrt( V[plotx,plotx] +  diag(V[modxContrastNames, modxContrastNames]) + 2* V[modxContrastNames, plotx])

      modxContrastNames <- c(plotx, modxContrastNames)
      slope <- c(mcoef[plotx], slope)
      seslope <- c(sqrt(V[plotx, plotx]) , seslope)
      tbsimple <- slope/seslope
      testSlopes <- data.frame(modx = modxContrastNames, b = slope, se = seslope,
                               t = tbsimple, p = 2 * pt(abs(tbsimple), df = model$df.residual,
                                               lower.tail = FALSE))
      row.names(testSlopes) <-  levels(model.frame(model)[ , modx])
      colnames(testSlopes) <- c(deparse(modx), "slope", "Std. Error", "t value", "Pr(>|t|)")
    }
    cat(paste("These are the straight-line \"simple slopes\" of the variable", plotx, " \n for the selected moderator values. \n"))
    print(testSlopes)
    res <- list("hypo-tests" = testSlopes)
  }

  ##Just for numeric variables, the J-N calculation
  roots <- NULL
  if (!is.factor(modxVar)) {
    mm <- model.matrix(model)
    modxVar <- pso$call$modx
    plotx <- pso$call$plotx
    ivs <- attr(terms(model), "term.labels")
    bs <- coef(model)
    V <- vcov(model)
    relevantInteractions <- c(paste(plotx, ":", modx, sep = ""),
                              paste(modx, ":", plotx, sep = ""))
    b2 <- bmodx <- NULL
    b1 <- bplotx <- bs[plotx]

    if (any(relevantInteractions %in% ivs)) {
      interactionsIn <- relevantInteractions[which(relevantInteractions %in%
                                                   ivs)]
      b2 <- bs[interactionsIn]
    }


    if(!is.null(b2)) {
      Tcrit <- qt(0.975, model$df)
      jna <- b2^2 - (Tcrit^2) * V[interactionsIn, interactionsIn]
      jnb <- 2*b1*b2 - (Tcrit^2) * 2 * V[plotx, interactionsIn]
      jnc <- b1^2 - (Tcrit^2) * V[plotx, plotx]
      inroot <- (jnb^2) - 4 * jna * jnc

      if ( inroot <= 0 ) {
        print(paste("There are no real roots to the quadratic equation that represents regions of statistical significance."))
        if (jna > 0) {
          paste("That means the slope (b1 + b2modx)plotx is statistically significant for all values of modx")
        } else {
          paste("In this case, that means the  slope (b1 + b2modx)plotx is never statistically significant")
        }
      } else {
        roots <- c( (-jnb - sqrt(inroot))/(2*jna),
                   (-jnb + sqrt(inroot))/(2*jna) )
        roots <- sort(roots)
        names(roots) <- c("lo","hi")
        if (jna > 0) {
          cat(paste("Values of modx OUTSIDE this interval:\n"))
          print(roots)
          cat(paste("cause the slope of (b1 + b2modx)plotx to be statistically significant\n"))
        } else {
          cat(paste("Values of modx INSIDE this interval:\n"))
          print(roots)
          cat(paste("cause the slope of (b1 + b2modx)plotx to be statistically significant\n"))
        }

        res <- ( list("hypo-tests" = testSlopes, "J-N-interval" = roots))
       # print(paste("b1 = b2x at", -b1/b2))
       # print(paste("quadratic minimum/maximum point at", -jnb/(2*jna)))
       # if(jna > 0) print("that is a minimum") else print("that is a maximum")

        modxRange <- magRange(mm[ ,modx], 1.1)
        modxSeq <- plotSeq(modxRange, length.out=100)

        depVarVals <- model.response(model.frame(model))
        modyRange <- magRange(depVarVals, 1.1)

        ## origmfcol <- par("mfcol")
        ## par(mfcol=c(2,1))
        ## plot(modxSeq, bs[plotx] + bs[interactionsIn]*modxSeq, type="l", xlab=paste("Values of the Moderator:", modx), ylab=paste("Predicted Simple Slope Effect of", plotx))
        ## if (jna > 0) {
        ##   polygon( x=c(modxRange[1],roots[1],roots[1], modxRange[1]),   c(modyRange[1], modyRange[1], modyRange[2], modyRange[2]), col="pink", border=NA)
        ##   polygon( x=c(roots[2], modxRange[2], modxRange[2],roots[2]),   c(modyRange[1], modyRange[1], modyRange[2], modyRange[2]), col="pink", border=NA)
        ## } else {
        ##   polygon( x=c(roots[1],roots[2], roots[2], roots[1]), y=c(modyRange[1],modyRange[1], modyRange[2],modyRange[2]), col="pink", border=NA)
        ## }
        ## lines(modxSeq, bs[plotx] + bs[interactionsIn]*modxSeq)
        ## box()
      }


      ##print("A plot of the quadratic equation should appear now, just for fun")

      if (jna > 0) {
          xps <- plotSeq(magRange(roots, 1.3), length.out=100)
         # cat(paste("Values of modx OUTSIDE this interval:\n"))
      }else{
          xps <- plotSeq(magRange(mm[, modx], 1.5), length.out=100)
      }

      y <- jna * xps^2 + jnb*xps + jnc
      plot(xps, y, type="l", xlab=paste("Values of the Moderator:", modx), ylab="slope/se - Tcrit")
      if( !is.null(roots) ){
          if(jna < 0 ){
              arrows( roots[1], 0, roots[2], 0, col="red", angle=90, lwd=3, code=3, length=0.1)
              text( mean(range(xps)), range(y)[1], pos=3, label=expression(paste((b[plotx] + b[modx:plotx]*modx)*plotx, " is significant in the red zone")))
          } else {
              arrows(min(xps), 0, roots[1], 0, col="red", angle=90, lwd=3, code=2, length=0.1)
              arrows(roots[2], 0, max(xps), 0, col="red", angle=90, lwd=3, code=1, length=0.1)
              text( mean(range(xps)), range(y)[2], pos=1, label=expression(paste((b[plotx] + b[modx:plotx]*modx)*plotx, " is significant in the red zone")))
          }
      }
      abline(h=0, col="gray80")

      ##par("mfcol" = origmfcol)
    } ## !is.null(b2)
  } ## !is.factor(modxVar)
  if (is.null(res)) cat("There were no interactions in the plotSlopes object, so testSlopes can't offer any advice.\n")
  invisible(res)
}
