
##' Stack together data frames
##' 
##' plyr::rbind.fill uses an experimental function
##' that I choose to avoid. This is the "safe" version. It
##' is re-written to eliminate calls to functions in the
##' tidyverse
##' @param ... Data frames
##' @return A stacked data frame
##' @author Paul Johnson
##' @examples
##' set.seed(123123)
##' N <- 100
##' dat <- genCorrelatedData2(N, means=c(10, 10, 10), sds = 3, stde = 3, beta = c(1, 1, -1, 0.5))
##' dat1 <- dat
##' dat1$xcat1 <- factor(sample(c("a", "b", "c", "d"), N, replace=TRUE))
##' dat1$xcat2 <- factor(sample(c("M", "F"), N, replace=TRUE), levels = c("M", "F"), labels = c("Male", "Female"))
##' dat1$y <- dat$y + as.vector(contrasts(dat1$xcat1)[dat1$xcat1, ] %*% c(0.1, 0.2, 0.3))
##' dat2 <- dat
##' dat1$x3 <- NULL
##' dat2$x2 <- NULL
##' dat2$xcat2 <- factor(sample(c("M", "F"), N, replace=TRUE), levels = c("M", "F"), labels = c("Male", "Female"))
##' dat2$xcat3 <- factor(sample(c("K1", "K2", "K3", "K4"), N, replace=TRUE))
##' dat3 <- dat
##' dat3$x1 <- NULL
##' dat3$xcat3 <-  factor(sample(c("L1", "L2", "L3", "L4"), N, replace=TRUE)) 
##' dat.stack <- rbind.fill(dat1, dat2, dat3)
##'
##' dat5 <- data.frame(x1 = rnorm(N), xcat1 = gl(20, 5))
##' dat6 <- data.frame(x1 = rnorm(N), xcat1 = gl(20, 5, ordered = TRUE))
##' stack1 <- rbind(dat5, dat6)
##' str(stack1)
##' stack2 <- rbind(dat6, dat5)
##' str(stack2)
##' stack3 <- plyr::rbind.fill(dat5, dat6)
##' str(stack3)
##' stack4 <- plyr::rbind.fill(dat6, dat5)
##' str(stack4)
##' stack5 <- rbind.fill(dat5, dat6)
rbind.fill <- function (...) 
{
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
        stop("All inputs to rbind.fill must be data.frames", 
            call. = FALSE)
    }

    rbind.pj <- function(aList){
        allclasses <- lapply(aList, function(x) lapply(x, class))
        allnames <- unique(unlist(sapply(allclasses, names)))

        agglevels <- setNames(vector("list", length = length(allnames)), allnames)
        for(k in aList){
            for(j in allnames){
                kjlevels <- if(any(j %in% names(k))) levels(k[[j]]) else NULL
                if(is.null(kjlevels)) next()
                agglevels[[j]] <- union(agglevels[[j]], kjlevels)
            }
        }
        agglevels <- removeNULL(agglevels)

        aggclasses <- setNames(vector("list", length = length(allnames)), allnames)
        for(k in aList){
            for(j in allnames){
                kjclasses <- if(any(j %in% names(k))) class(k[[j]]) else NULL
                if(is.null(kjclasses)) {
                ##     if(!is.null(aggclasses[[j]])){
                ##         browser()
                ##         MESSG <- "Class mis-match stage 1"
                ##         stop(MESSG)
                ##     }
                     next()
                }
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
        
        
        rowcount <- sapply(aList, NROW)
        rowcountsum <- sum(rowcount)
        bigdata <- as.data.frame(matrix(NA, nrow = rowcountsum,
                                        ncol = length(allnames),
                                        dimnames = list(NULL, allnames)))
        for(k in names(agglevels)){
            bigdata[ , k] <- factor(NA, levels = agglevels[[k]])
        }
        
        for(i in 1:length(aList)){
            start <- if(i == 1) 1 else start + rowcount[i-1]
            end <- if(i == 1) rowcount[i] else end + rowcount[i]
            if(i > 1){
                commonnames <- intersect(names(allclasses[[i-1]]),
                                         names(allclasses[[i]]))
                if(any(!allclasses[[i-1]][commonnames] == allclasses[[i]][commonnames])){
                    MESSG <- paste("Classes of data frames are not identical:\n",
                          paste(allclasses[i-1], collapse = " "),  paste(allclasses[i], collapse = " "))        
                    stop(MESSG)
                }
            }
            
            bigdata[start: end, colnames(aList[[i]])] <- aList[[i]]
        }
        bigdata
    }
    
    ## rows <- unlist(lapply(dfs, .row_names_info, 2L))
    ## nrows <- sum(rows)
    ## ot <- output_template(dfs, nrows)
    ## setters <- ot$setters
    ## getters <- ot$getters
    ## if (length(setters) == 0) {
    ##     return(as.data.frame(matrix(nrow = nrows, ncol = 0)))
    ## }
    ## pos <- matrix(c(cumsum(rows) - rows + 1, rows), ncol = 2)
    ## for (i in seq_along(rows)) {
    ##     rng <- seq(pos[i, 1], length.out = pos[i, 2])
    ##     df <- dfs[[i]]
    ##     for (var in names(df)) {
    ##         setters[[var]](rng, df[[var]])
    ##     }
    ## }
    ## as.data.frame(lapply(getters, function(x) x()))
    
    rbind.pj(dfs)
}
