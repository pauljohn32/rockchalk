##' Stack together data frames
##' 
##' In the end of the code for plyr::rbind.fill, the author explains
##' that is uses an experimental function to build the data.frame.  I
##' would rather not put any weight on an experimental function, so I
##' sat out to create a new rbindFill. This function uses no
##' experimental functions. It does not rely on any functions from
##' packages that are not in base of R, except, of course, for functions
##' in this package.
##'
##' Along the way, I noticed a feature that seems to be a flaw in both
##' rbind and rbind.fill.  In the examples, there is a demonstration
##' of the fact that base R rbind and plyr::rbind.fill both have
##' undesirable properties when data sets containing factors and
##' ordered variables are involved. This function introduces a
##' "data consistency check" that prevents corruption of variables
##' when data frames are combined.  This "safe" version will notice
##' differences in classes of variables among data.frames and stop
##' with an error message to alert the user to the problem.
##' @param ... Data frames
##' @return A stacked data frame
##' @author Paul Johnson
##' @name rbindFill
##' @export rbindFill
##' @examples
##' set.seed(123123)
##' N <- 10000
##' dat <- genCorrelatedData2(N, means = c(10, 20, 5, 5, 6, 7, 9), sds = 3,
##'            stde = 3, rho = .2,  beta = c(1, 1, -1, 0.5))
##' dat1 <- dat
##' dat1$xcat1 <- factor(sample(c("a", "b", "c", "d"), N, replace=TRUE))
##' dat1$xcat2 <- factor(sample(c("M", "F"), N, replace=TRUE),
##'                     levels = c("M", "F"), labels = c("Male", "Female"))
##' dat1$y <- dat$y +
##'           as.vector(contrasts(dat1$xcat1)[dat1$xcat1, ] %*% c(0.1, 0.2, 0.3))
##' dat1$xchar1 <- rep(letters[1:26], length.out = N)
##' dat2 <- dat
##' dat1$x3 <- NULL
##' dat2$x2 <- NULL
##' dat2$xcat2 <- factor(sample(c("M", "F"), N, replace=TRUE),
##'                      levels = c("M", "F"), labels = c("Male", "Female"))
##' dat2$xcat3 <- factor(sample(c("K1", "K2", "K3", "K4"), N, replace=TRUE))
##' dat2$xchar1 <- "1"
##' dat3 <- dat
##' dat3$x1 <- NULL
##' dat3$xcat3 <-  factor(sample(c("L1", "L2", "L3", "L4"), N, replace=TRUE)) 
##' dat.stack <- rbindFill(dat1, dat2, dat3)
##' str(dat.stack)
##'
##' ## Possible BUG alert about base::rbind and plyr::rbind.fill
##' ## Demonstrate the problem of a same-named variable that is factor in one and
##' ## an ordered variable in the other
##' dat5 <- data.frame(ds = "5", x1 = rnorm(N),
##'                    xcat1 = gl(20, 5, labels = LETTERS[20:1]))
##' dat6 <- data.frame(ds = "6", x1 = rnorm(N),
##'                    xcat1 = gl(20, 5, labels = LETTERS[1:20], ordered = TRUE))
##' ## rbind reduces xcat1 to factor, whether we bind dat5 or dat6 first.
##' stack1 <- base::rbind(dat5, dat6)
##' str(stack1)
##' ## note xcat1 levels are ordered T, S, R, Q
##' stack2 <- base::rbind(dat6, dat5)
##' str(stack2)
##' ## xcat1 levels are A, B, C, D
##' ## stack3 <- plyr::rbind.fill(dat5, dat6)
##' ## str(stack3)
##' ## xcat1 is a factor with levels T, S, R, Q ...
##' ## stack4 <- plyr::rbind.fill(dat6, dat5)
##' ## str(stack4)
##' ## oops, xcat1 is ordinal with levels A < B < C < D
##' ## stack5 <- rbindFill(dat5, dat6)
rbindFill <- function(...) 
{
    ## small copy removeNULL
    removeNULL <- function (aList){ 
        Filter(Negate(is.null), aList)
    }

    dfs <- list(...)
    if (length(dfs) == 0) 
        stop("no data frame found")
    if (is.list(dfs[[1]]) && !is.data.frame(dfs[[1]])) {
        dfs <- dfs[[1]]
    }
    dfs <- removeNULL(dfs)
    if (length(dfs) == 0) 
        stop("no data frame found")
    if (length(dfs) == 1) 
        return(dfs[[1]])
    is_df <- vapply(dfs, is.data.frame, logical(1))
    if (any(!is_df)) {
        stop("All inputs to rbindFill must be data.frames", 
            call. = FALSE)
    }

    allclasses <- lapply(dfs, function(x) lapply(x, class))
    allnames <- unique(unlist(lapply(allclasses, names)))

    ## scan data frames and accumulate the levels of factor variables
    agglevels <- setNames(vector("list", length = length(allnames)), allnames)
    for(k in dfs){
        for(j in allnames){
            kjlevels <- if(any(j %in% names(k))) levels(k[[j]]) else NULL
            if(is.null(kjlevels)) next()
            agglevels[[j]] <- union(agglevels[[j]], kjlevels)
        }
    }
    agglevels <- removeNULL(agglevels)

    ## Classes of variables must be identical!
    ## if different data frames use same variable name for data of
    ## different classes, STOP with error
    aggclasses <- setNames(vector("list", length = length(allnames)), allnames)
    for(k in dfs){
        for(j in allnames){
            kjclasses <- if(any(j %in% names(k))) class(k[[j]]) else NULL
            if(is.null(kjclasses)) next()
            
            if (!is.null(aggclasses[[j]]) && !identical(aggclasses[[j]], kjclasses)){
                MESSG <- paste("Class mis-match between data.frames\n",
                               "Variable: ", j, "\n",
                               "(", paste(aggclasses[[j]], collapse = ", "), ")",
                               "versus",
                               "(", paste(kjclasses, collapse = ", "), ")")
                stop(MESSG)
            }
            aggclasses[[j]] <-kjclasses
        }
    }
        
    rowcount <- sapply(dfs, NROW)
    rowcountsum <- sum(rowcount)

    ## I have tested methods to allocate space for new stacked data
    ## Choose by if() statement here
    if(TRUE){
        ## Allocate a big block matrix, then
        ## coerce to a data.frame, then set the factor/ordered vars levels
        datastack <- as.data.frame(matrix(NA, nrow = rowcountsum,
                                        ncol = length(allnames),
                                        dimnames = list(NULL, allnames)))
        ## factor variables must have expanded level list
        for(k in names(agglevels)){
            datastack[ , k] <- factor(NA, levels = agglevels[[k]],
                                    ordered = "ordered" %in% aggclasses[k])
        }
    } else {
        ## Allocates storage for vectors of correct type in first
        ## step, then casts that a data frame. 
        datastack <- setNames(vector("list", length = length(allnames)), allnames)
        for(j in allnames) {
            varclass <- aggclasses[[j]][1]
            if (!varclass %in% c("factor", "ordered")){
                datastack[[j]] <- vector(aggclasses[[j]][1], length = rowcountsum)
            } else {
                datastack[[j]] <- factor(NA, levels = agglevels[[j]],
                                       ordered = "ordered" %in% aggclasses[j])
            }
        }
        datastack <- as.data.frame(datastack, stringsAsFactors = FALSE)
    }    

    ## place the data frames into the datastack object, using row and column indexing.
    for(i in seq_along(dfs)){
        start <- if(i == 1) 1 else start + rowcount[i-1]
        end <- if(i == 1) rowcount[i] else end + rowcount[i]
        if(i > 1){
            commonnames <- intersect(names(allclasses[[i-1]]),
                                     names(allclasses[[i]]))
            ## This is a duplicate column-class check, was written first, but
            ## previous check should make it unnecessary. If we trust previous check
            if(!identical(allclasses[[i-1]][commonnames], allclasses[[i]][commonnames])){
                MESSG <- paste("Classes of data frames are not identical:\n",
                               paste(allclasses[i-1], collapse = " "),
                               paste(allclasses[i], collapse = " "))        
                stop(MESSG)
            }
        }
        datastack[start: end, colnames(dfs[[i]])] <- dfs[[i]]
    }
    datastack
}
NULL
