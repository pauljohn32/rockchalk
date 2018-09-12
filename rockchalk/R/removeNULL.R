
##' Remove NULL values variables from a list
##'
##' Unlike vectors, lists can hold objects with
##' value NULL. This gets rid of them.
##'
##' This version is NOT recursive
##'
##' plyr::rbind.fill uses an experimental function
##' that I choose to avoid. This is the "safe" version.
##' 
##' @param aList A list
##' @return Same list with NULL's removed
##' @author Paul Johnson
##' @export removeNULL
##' @examples
##' ## Note it is non-recursive, NULL remains in e
##' x <- list(a = rnorm(5), b = NULL, c = rnorm(5), d = NULL,
##'      e = list(f = rnorm(2), g = NULL))
##' x
##' removeNULL(x)
removeNULL <- function (aList){ 
    Filter(Negate(is.null), aList)
}
