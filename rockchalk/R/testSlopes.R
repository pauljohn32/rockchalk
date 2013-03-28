##' Hypothesis tests for Simple Slopes Objects
##'
##' Conducts t-test of the hypothesis that the "simple slope" line for
##' one predictor is statistically significantly different from zero
##' for each value of a moderator variable. The user must first run
##' \code{plotSlopes()}, which generates an object that testSlopes
##' can investigate.
##'
##' This function scans the input object to detect the focal values of
##' the moderator variable (the variable declared as \code{modx} in
##' \code{plotSlopes}. Consider a regression with interactions
##'
##' y <- b0 + b1*x1 + b2*x2 + b3*(x1*x2) + b4*x3 + ... + error
##'
##' If \code{plotSlopes} has been run with the argument plotx="x1" and
##' the argument modx="x2", then there will be several plotted lines,
##' one for each of the chosen values of x2.  The slope of each of
##' these lines depends on x1's effect, b1, as well as the interactive
##' part, b3*x2.
##'
##' This function performs a test of the null hypothesis of the slope
##' of each fitted line in a \code{plotSlopes} object is statistically
##' significant from zero. A simple t-test for each line is offered.
##' No correction for the conduct of multiple hypothesis tests (no
##' Bonferroni correction).
##'
##' When \code{modx} is a numeric variable, it is possible to conduct
##' further analysis. We can ask not only, "is this particular line's
##' slope statistically significant?" but also "for which values of
##' \code{modx} would the effect of \code{plotx} be statistically
##' significant?"  This is called a Johnson-Neyman (Johnson-Neyman,
##' 1936) approach in Preacher, Curran, and Bauer (2006). The interval
##' is calculated here.  A plot method is provided that displays the
##' result in a couple of ways.   Where the t-test considers the question, is the
##' @param pso (short for plotSlopesObject) Output from the plotSlopes function
##' @return A list including the hypothesis test table. For numeric
##' modx variables, also the Johnson-Neyman (J-N) interval boundaries.
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
##' m3ps <- plotSlopes(m3, modx = "sex", plotx = "income")
##' testSlopes(m3ps)
##'
##' m4 <- lm(statusquo ~ region * income, data= Chile)
##' m4ps <- plotSlopes(m4, modx = "region", plotx = "income", plotPoints = FALSE)
##' ##' testSlopes(m4ps)
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

testSlopes <- function(pso)
{
    model <-  eval(parse(text=pso$call$model))
    plotx <- pso$call$plotx
    modx <- pso$call$modx

    ivs <- attr(terms(model), "term.labels")
    relevantInteractions <- c(paste(plotx, ":", modx, sep = ""),
                              paste(modx, ":", plotx, sep = ""))

    interactionsIn <- relevantInteractions[which(relevantInteractions %in% ivs)]

    if (!any(relevantInteractions %in% ivs)) {

        if (is.null(res)) cat("There were no interactions in the plotSlopes object, so testSlopes can't offer any advice.\n")
        return(NULL)
    }


    ## Whew. We did not return, so let's get down to business.
    modxVar <- pso$newdata[ , modx]
    modxVals <- pso$modxVals
    bs <- coef(model)
    V <- vcov(model)

    ## If modx is a factor, only need to test a particular set of lines.
    if (is.factor(modxVar)) {
        mcoef <- coef(model)
        modxContrastNames <- c(grep(plotx,  grep(modx, names(mcoef), value = TRUE),  value=TRUE))
        slope <- mcoef[modxContrastNames] + coef(model)[plotx]

        ## 2013-02-19 Sunthud Pornprasertmanit spots bug here:
        ## Problem: diag doesn't work when argument is a single real number.
        ## Fix by inserting drop=FALSE (wrote a blog post about the "drop gotcha"
        seslope <- sqrt(V[plotx,plotx, drop = FALSE] +  diag(V[modxContrastNames, modxContrastNames, drop = FALSE]) + 2* V[modxContrastNames, plotx, drop = FALSE])

        modxContrastNames <- c(plotx, modxContrastNames)
        slope <- c(mcoef[plotx], slope)
        seslope <- c(sqrt(V[plotx, plotx]) , seslope)
        tbsimple <- slope/seslope
        testSlopes <- data.frame(modx = modxContrastNames, b = slope, se = seslope,
                                 t = tbsimple, p = 2 * pt(abs(tbsimple), df = model$df.residual,
                                               lower.tail = FALSE))
        row.names(testSlopes) <-  levels(model.frame(model)[ , modx])
        colnames(testSlopes) <- c(deparse(modx), "slope", "Std. Error", "t value", "Pr(>|t|)")

        cat(paste("These are the straight-line \"simple slopes\" of the variable", plotx, " \n for the selected moderator values. \n"))
        print(testSlopes)
        res <- list("hypotests" = testSlopes, pso = pso)
        invisible(res)
    } else {
        bmodx <- NULL
        bplotx <- bs[plotx]

        bmodx <- bs[interactionsIn]
        bsimple <- bplotx + bmodx * modxVals
        covbsimple <- cbind(1, modxVals^2, 2 * modxVals) %*%
            c(V[plotx, plotx], V[names(bmodx), names(bmodx)], V[plotx, names(bmodx)])
        tbsimple <- bsimple/sqrt(covbsimple)

        testSlopes <- data.frame( modx = modxVals, b = bsimple,
                                 se = sqrt(covbsimple), t = tbsimple,
                                 p = 2 * pt(abs(tbsimple),
                                 df = model$df.residual, lower.tail = FALSE))

        colnames(testSlopes) <- c(deparse(modx), "slope", "Std. Error", "t value", "Pr(>|t|)")



        ## Just for numeric variables, the J-N calculation
        roots <- NULL

        mm <- model.matrix(model)
        b2 <- bmodx <- NULL
        b1 <- bplotx <- bs[plotx]

        b2 <- bs[interactionsIn]

        if(is.null(b2)) stop("b2 is null, there's no interation! Logic error")

        Tcrit <- qt(0.975, model$df)

        ## Quadratic formula. Solve
        ## Tcrit < T = (b1 + b2*modx)/s.e.(b1 + b2*modx)
        ## Tcrit < T = (b1 + b2*modx)/sqrt(Var(b1) + modx^2*Var(b2) + 2 Var[b1,b2])

        jn <- list()
        jn$a <- b2^2 - (Tcrit^2) * V[interactionsIn, interactionsIn]
        jn$b <- 2*b1*b2 - (Tcrit^2) * 2 * V[plotx, interactionsIn]
        jn$c <- b1^2 - (Tcrit^2) * V[plotx, plotx]
        inroot <- (jn$b^2) - 4 * jn$a * jn$c

        ##complex root check, if yes, exit with message.
        if (inroot <= 0) {
            print(paste("There are no real roots to the quadratic equation that represents regions of statistical significance."))
            if (jn$a > 0) {
                paste("That means the slope (b1 + b2modx)plotx is statistically significant for all values of modx")
            } else {
                paste("In this case, that means the  slope (b1 + b2modx)plotx is never statistically significant")
            }
            res <- list("hypotests" = testSlopes, "jn" = jn, pso = pso)
            invisible(res)
        }

        ## Whew. Roots not complex, otherwise would have returned
        jn$roots <- c( (-jn$b - sqrt(inroot))/(2*jn$a),
                      (-jn$b + sqrt(inroot))/(2*jn$a) )
        jn$roots <- sort(jn$roots)
        names(jn$roots) <- c("lo","hi")
        if (jn$a > 0) {
            cat(paste("Values of", modx, "OUTSIDE this interval:\n"))
            print(jn$roots)
            cat(paste("cause the slope of (b1 + b2*", modx,")", plotx, " to be statistically significant\n", sep = ""))
        } else {
            cat(paste("Values of", modx, "INSIDE this interval:\n"))
            print(jn$roots)
            cat(paste("cause the slope of (b1 + b2*", modx,")", plotx, " to be statistically significant\n", sep = ""))
        }


        ## print(paste("b1 = b2x at", -b1/b2))
        ## print(paste("quadratic minimum/maximum point at", -jn$b/(2*jn$a)))
        ## if(jn$a > 0) print("that is a minimum") else print("that is a maximum")
        res <- list("hypotests" = testSlopes, "jn" = jn, pso = pso)
        invisible(res)
    }
}


plot.testSlopes <- function(tso){
    model <-  eval(parse(text = tso$pso$call$model))
    mm <- model.matrix(model)

    plotx <- tso$pso$call$plotx
    modx <- tso$pso$call$modx

    modxRange <- magRange(mm[ ,modx], 1.1)
    modxSeq <- plotSeq(modxRange, length.out=100)

    depVarVals <- model.response(model.frame(model))
    modyRange <- magRange(depVarVals, 1.1)
    Tcrit <- qt(0.975, model$df)

    ivs <- attr(terms(model), "term.labels")
    relevantInteractions <- c(paste(plotx, ":", modx, sep = ""),
                              paste(modx, ":", plotx, sep = ""))
    interactionsIn <- relevantInteractions[which(relevantInteractions %in% ivs)]
    if (length(interactionsIn) > 1) stop("sorry, I haven't figured out what do to do if interactionsIn > 1")

    bs <- coef(model)
    V <- vcov(model)

    bsslope <-  bs[plotx] + bs[interactionsIn]*modxSeq
    bsse <- sqrt(V[plotx,plotx] + modxSeq^2 * V[interactionsIn,interactionsIn] + Tcrit * modxSeq * V[plotx, interactionsIn])

    ## MM marginal matrix, similar to return of the predict() function
    MM <- cbind(fit = bsslope, lwr = bsslope - Tcrit * bsse, upr = bsslope + Tcrit * bsse, modxSeq = modxSeq, se = bsse, p = 2*pt(abs(bsslope/bsse), lower.tail = FALSE, df = model$df))
    MMlwr <- MM[(tso$jn$roots[1] >= modxRange[1]) & (MM[ , "p"] < 0.05), , drop = F]
    MMupr <- MM[(tso$jn$roots[2] <= modxRange[2]) & (MM[ , "p"] < 0.05), , drop = F]

    ## Thin by keeping only even-numbered rows
    if (nrow(MMlwr) >= 3) MMlwr <- MMlwr[ seq(1, nrow(MMlwr), by = 2),  ]
    if (nrow(MMupr) >= 3) MMupr <- MMupr[ seq(1, nrow(MMupr), by = 2),  ]


    ylab <- substitute("Marginal Effect of" ~~ AA: ~~ "(" * hat(b)[AA] + hat(b)[BB:AA]*BB *")", list(AA=plotx, BB=modx))
    ylim <- magRange(range(c(MM[ , "lwr"],MM[, "upr"])), c(1.15, 1))

    op1 <- par(no.readonly = TRUE)
    par(mar = par("mar") + c(0,1,0,0))

    plot(fit ~ modxSeq, data = MM, type="l", xlab = paste("The Moderator: ", modx, ")"), ylim = ylim, ylab = ylab)
    par <- op1

    abline(h = 0, col = gray(.80))
    lines(upr ~ modxSeq, data = MM, lty = 2, col = gray(.50))
    lines(lwr ~ modxSeq, data = MM, lty = 2, col = gray(.50))

    ## can't imagine a useful case where a < 0.
    if (tso$jn$a > 0) {
        if (k <- nrow(MMlwr) > 0) {
            arrows(x0 = MMlwr[ ,"modxSeq"], y0 = MMlwr[ ,"lwr"], x1 = MMlwr[ ,"modxSeq"], y1 = MMlwr[ ,"upr"], angle = 90, length = 0.05, code = 3, col = gray(.70))

            lines(x = rep(MMlwr[k, "modxSeq"],2), y = c(MMlwr[k ,"lwr"], ylim[1]), lty=4, col = gray(.80) )
            mtext(text = round(MMlwr[k, "modxSeq"], 2), at = MMlwr[k, "modxSeq"],  side = 1, line = 2)
        }


        if (k <- nrow(MMupr) > 0) {
            arrows(x0 = MMupr[ ,"modxSeq"], y0 = MMupr[ ,"lwr"], x1 = MMupr[ ,"modxSeq"], y1 = MMupr[ ,"upr"], angle = 90, length = 0.05, code = 3, col = gray(.70))
            lines(x = rep(MMupr[1, "modxSeq"],2), y = c(MMupr[1 ,"lwr"], ylim[1]), lty=4, col = gray(.80))
            mtext(text = round(MMupr[1, "modxSeq"], 2), at = MMupr[1, "modxSeq"],  side = 1, line = 2)
        }
    }

    ## shaded on either end.
    if (tso$jn$a > 0) {

        polygon( x = c(MMlwr[ ,"modxSeq"], rev(MMlwr[ ,"modxSeq"])),
                y = c(MMlwr[ ,"lwr"], rev(MMlwr[ , "lwr"]) ),
                col = rgb(1, 0, 0, 0.10), border = gray(.80))


        polygon( x = c(MMupr[ ,"modxSeq"], rev(MMupr[ ,"modxSeq"])),
                y = c(MMupr[ ,"upr"], rev(MMupr[ , "lwr"])),
                col = rgb(1, 0, 0, 0.10), border = gray(.80))

    }

    legend("topleft", legend = c("Marginal Effect", "95% Conf. Int."),
           lty = c(1, 2), col = c(1, gray(.50)))

    legend("bottomright", title = "Shaded Region: Null Hypothesis",
       legend = substitute(b[AA] + b[BB:AA]*BB == 0 ~~ "rejected", list(AA=plotx, BB=modx)),
           fill = c(rgb(1,0,0, 0.10)))


    ## Now draw the quadratic equation, to show the solution of slope/se - Tcrit = 0

    ##print("A plot of the quadratic equation should appear now, just for fun")
    if (tso$jn$a > 0) {
        xps <- plotSeq(magRange(tso$jn$roots, 1.3), length.out=100)
        ## cat(paste("Values of modx OUTSIDE this interval:\n"))
    } else {
        xps <- plotSeq(magRange(mm[, modx], 1.5), length.out=100)
    }

    y <- tso$jn$a * xps^2 + tso$jn$b*xps + tso$jn$c
    plot(xps, y, type="l", xlab=paste("Values of the Moderator:", modx), ylab="slope/se - Tcrit")
    if( !is.null(tso$jn$roots) ){
        if(tso$jn$a < 0 ){
            arrows( tso$jn$roots[1], 0, tso$jn$roots[2], 0, col="red", angle=90, lwd=3, code=3, length=0.1)
            text( mean(range(xps)), range(y)[1], pos=3, label=expression(paste((b[plotx] + b[modx:plotx]*modx)*plotx, " is significant in the red zone")))
        } else {
            arrows(min(xps), 0, tso$jn$roots[1], 0, col="red", angle=90, lwd=3, code=2, length=0.1)
            arrows(tso$jn$roots[2], 0, max(xps), 0, col="red", angle=90, lwd=3, code=1, length=0.1)
            text( mean(range(xps)), range(y)[2], pos=1,
                 label = expression(paste((b[plotx] + b[modx:plotx]*modx)*plotx,
                     " is significant in the red zone")))
        }
    }
    abline(h=0, col="gray80")
}



a <- 1.1
b <- -3

plot(1:10, 1:10, type = "n", axes = FALSE)

text( 5, 5,  label = expression(paste((b[plotx] + b[modx:plotx]*modx)*plotx,
                      " is significant in the red zone")))

text( 5, 3,  label = expression(paste((b[plotx] + b[modx:plotx]*modx)*plotx,
                      "\n is significant in the red zone")))

## This is OK
legend("topleft", legend = c(expression(paste(hat(b)[1] == 7, " that's beta to you")), expression(hat(b)[1])))

legend("bottomleft", legend = c(expression(paste(widehat(b[1]) == 8, " that's beta to you")), "Regular Text"))

legend("topright", legend = as.expression(c(substitute(hat(b)[1] == AA, list(AA = a)), "Regular Text")))

legend("left", legend= substitute(widehat(b[1]) == AA, list(AA = a)))

legend("bottomright", legend = c(substitute(hat(b[1]) == AA, list(AA = a)), substitute(hat(b[2]) == BB, list(BB = b))))


xlab <- paste0("beta", seq(2, 10, by=2))


