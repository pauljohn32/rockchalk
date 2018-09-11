

##' makeVec for checking or creating vectors
##'
##' This is a convenience for handling function arguments. If x is a
##' single value, it makes a vector of length d in which all values
##' are equal to x. If x is a vector, check that its length is d.
##'
##' @param x A single value or a vector
##' @param d An integer, the desired size of the vector
##' @return A vector of length d
##' @author Paul E. Johnson <pauljohn@@ku.edu>
makeVec <-
    function(x = NULL, d = NULL)
{
    if (length(x) == 1) {
        x <- rep(x, d) #assign same for all
    } else if (length(x) != d){
        msg <- paste(deparse(substitute(x)), "has too",
                     if(length(x) > d) "many" else "few",
                     "elements. Please specify", d, "elements.
                     Or just specify 1, we will use that one value to",
                     "manufacture a vector for you.")
        stop(msg)
    }
    x
}
NULL
