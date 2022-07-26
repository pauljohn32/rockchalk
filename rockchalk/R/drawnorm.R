##' draw a normal distribution with beautiful illustrations
##'
##' This was developed for the R Working Example collection
##' in my website, pj.freefaculty.org/R/WorkingExamples
##' @param mu The mu parameter
##' @param sigma The sigma parameter
##' @param xlab Label for x axis
##' @param ylab Label for Y axis
##' @param main Title for plot. OK to ignore this, we'll make a nice one for you
##' @param ps pointsize of text
##' @param ... arguments passed to par
##' @return NULL
##' @importFrom stats dnorm
##' @importFrom stats pnorm
##' @importFrom stats median
##' @export
##' @author Paul Johnson \email{pauljohn@@ku.edu}
##' @examples
##' drawnorm(mu = 10, sigma = 20)
##' drawnorm(mu= 0, sigma = 1)
##' drawnorm(mu = 102, sigma = 313)
##' drawnorm(mu = 0, sigma = 1, main = "A Standard Normal Distribution, N(0,1)",
##'          xlab = "X", ylab = "Density", ps = 7)
##' drawnorm(mu = 0, sigma = 1, ylab = "Density", ps = 14) 
drawnorm <- function(mu = 0, sigma = 1, xlab = "A Normally Distributed Variable",
                     ylab = "Probability Density", main, ps = par("ps"), ...){
    dotargs <- list(...)
    dotnames <- names(dotargs)
    dots.par <- dotargs[names(dotargs)[names(dotargs) %in% c(names(par()), formalArgs(plot.default))]]
        
    sigma.rounded <- if(1 == sigma[1]) 1 else round(sigma[1],2)
    mu.rounded <- round(mu, 2)
  
    myx <- seq( mu - 3.5*sigma,  mu+ 3.5*sigma, length.out=500)
    myDensity <- dnorm(myx,mean=mu,sd=sigma)
    if(missing(main)) {
        main <- bquote(x %~% Normal~group("(", list(mu == .(mu.rounded),
                       sigma^2 == .(if(sigma[1] == 1) 1 else sigma.rounded^2)),")"))
    }
    ## xpd needed to allow writing outside strict box of graph
    par.orig <-  par(xpd=TRUE, ps = ps)
    on.exit(par(par.orig))

    plot.parms <- list(x = myx, y = myDensity, type = "l", xlab = xlab, ylab = ylab, main = main, axes = FALSE)
    plot.parms <- modifyList(plot.parms, dots.par)
    do.call(plot, plot.parms)
    axis(2, pos = mu - 3.6*sigma)
    ticksat <- c(mu - 2.5 * sigma, mu,  mu - sigma, mu + sigma, mu + 2.5 * sigma)
    axis(1, pos = 0, at = ticksat)
    lines(c(myx[1],myx[length(myx)]),c(0,0)) ### closes off axes

    ## bquote creates an expression that text plotters can use
    t1 <-  bquote(mu== .(mu))
    ## Find a value of myx that is "very close to" mu
    centerX <- max(which (myx <= mu))
    ## plot light vertical line under peak of density
    lines( c(mu, mu), c(0, myDensity[centerX]), lty= 14, lwd=.2)
    ## label the mean
    text(mu, 0.4 * max(myDensity), labels = bquote( mu == .(mu.rounded)), pos = 2)

    ### find position 20% "up" vertically, to use for arrow coordinate
    ss = 0.2 * max(myDensity)
    ## Insert interval to represent width of one sigma
    arrows( x0=mu, y0= ss, x1=mu+sigma, y1=ss, code=3, angle=90, length=0.1)
    ## Write the value of sigma above that interval
    t2 <-  bquote( sigma== .(round(sigma,2)))
    text( mu+0.5*sigma, 1.15*ss, t2)

    ## Create a formula for the Normal
    normalFormula <- expression (f(x) == frac (1, sigma* sqrt(2*pi)) * ~~ e^{~-~frac(1,2)~bgroup("(", frac(x-mu,sigma),")")^2})
    ## Draw the Normal formula
    text ( mu + 0.5*sigma, max(myDensity)- 0.10 * max(myDensity),  normalFormula, pos=4)
    
    ## Theory says we should have 2.5% of the area to the left of: -1.96 * sigma.
    ## Find the X coordinate of that "critical value"
    criticalValue <- mu -1.96 * sigma
    ## Then grab all myx values that are "to the left" of that critical value.
    specialX <-  myx[myx <= criticalValue]
    
    ## mark the critical value in the graph
    text ( criticalValue, 0 , label= paste(round(criticalValue,2)), pos=1, cex = .7)
    ## Take sequence parallel to values of myx inside critical region
    specialY <- myDensity[myx < criticalValue]
    ##  Polygon makes a nice shaded illustration
    polygon(c(specialX[1], specialX, specialX[length(specialX )]), c(0, specialY, 0), density=c(-110),col="lightgray" )
    shadedArea <- round(pnorm(mu - 1.96 * sigma, mean=mu, sd=sigma), 4)
    criticalValue.rounded <- round(criticalValue, 3)
    ## I want to insert annotation about area on left side.
    al1 <- bquote(atop(Prob(x <= .(criticalValue.rounded)),
                        F(.(criticalValue.rounded)) == .(shadedArea)))
    ## Get center position in shaded area
    medX <- median(specialX) 
    indexMed <- max(which(specialX < medX))
    ## density at that center point:
    medY <- specialY[indexMed]
    denMax <- max(specialY)
    text(medX, denMax, labels=al1, pos = 3, cex = 0.7)
    indexMed <- max(which(specialX < medX))
    ## left side arrow
    x1 <-  medX + .1 *abs(max(specialX) - min(specialX))
    y1 <- 1.2 * myDensity[max(which(specialX < x1))]
    arrows(x0=medX, y0=denMax, x1= x1, y1 = y1,   length=0.1)
    ss <- 0.1 * max(myDensity)
    ## Mark interval from mu to mu-1.96*sigma
    arrows( x0=mu, y0= ss, x1=mu-1.96*sigma, y1=ss, code=3, angle=90, length=0.1)
    ## Put text above interval
    text( mu - 2.0*sigma, 1.15*ss, bquote(paste(.(criticalValue.rounded)==mu-1.96 * sigma,sep="")),pos=4 )

    ## Now work on right side critical value and area
    criticalValue <- mu +1.96 * sigma
    criticalValue.rounded <- round(criticalValue, 3)
    ## Then grab all myx values that are "to the left" of that critical value.
    specialX <-  myx[myx >= criticalValue]
    ## mark the critical value in the graph
    text ( criticalValue, 0 , label= paste(criticalValue.rounded), pos=1, cex = 0.7)
    ## Take sequence parallel to values of myx inside critical region
    specialY <- myDensity[myx >= criticalValue]
    ##  Polygon for shaded illustration
    polygon(c(specialX[1], specialX, specialX[length(specialX )]), c(0, specialY, 0), density=c(-110), col="lightgray" )
    shadedArea <- round(pnorm(mu + 1.96 * sigma, mean=mu, sd=sigma, lower.tail=F),4)
    ## Insert comment on right side.
    al2 <- bquote(atop(1 - F( .(criticalValue.rounded)),
                       phantom(0) == .(shadedArea)))
    medX <- median(specialX)
    ## denAtMedX <- myDensity[indexMed <- max(which(specialX < medX))]
    denMax <-  max(specialY)
    text(medX, denMax, labels=al2, pos = 3, cex = 0.7)
    ## point from text toward shaded area
    x1 <-  medX - .1 *abs(max(specialX) - min(specialX))
    y1  <- 1.2 * specialY[max(which(specialX < x1))]
    ## arrows( x0=medX, y0=denMax, x1= medX - 0.1*abs(max(specialX) - min(specialX)), y1= 1.02 * myDensity[indexMed] ,   length=0.1)
    arrows( x0=medX, y0=denMax, x1= x1, y1 = y1,  length=0.1)
    
    ss <- 0.05 * max(myDensity)
    ## Mark interval from mu to mu+1.96*sigma
    arrows( x0=mu, y0= ss, x1=mu+1.96*sigma, y1=ss, code=3, angle=90, length=0.1)
    ## Put text above interval
    text( mu + 1.96*sigma,1.15*ss, bquote(paste(.(criticalValue.rounded)==mu+1.96 * sigma,sep="")), pos=2 )
}
