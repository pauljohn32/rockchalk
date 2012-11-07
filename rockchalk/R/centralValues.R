##' Central Tendency estimates for variables
##'
##' This is needed for the creation of summaries and predicted values
##' of regression models. It takes a data frame and returns a new data
##' frame with one row in which the mean or mode of the columns is
##' reported.
##'
##' @param x a data frame
##' @return a data frame with the same variables and one row, the summary indicators.
##' @author Paul E. Johnson \email{pauljohn@@ku.edu}
##' @export
##' @examples
##' myDat <- data.frame(x=rnorm(100), y=rpois(100,l=4), z = cut(rnorm(100), c(-10,-1,0,10)))
##' centralValues(myDat)

centralValues <- function(x){
  if( !is.data.frame(x)) stop("represent: x must be a data frame!")
  nc <- NCOL(x)
  nams <- colnames(x)
  represents <- list()
  for (i in 1: nc) {
    represents[[nams[i]]] <- if (is.numeric(x[ ,i])){
      mean(x[ ,i], na.rm=T)
    } else  levels(x[,i]) [which.max(table(x[ ,i]))]
  }
  as.data.frame(represents)
}
