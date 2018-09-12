##' Group Mean Center: Generate group summaries and individual deviations within groups
##'
##' Multilevel modelers often need to include predictors like the
##' within-group mean and the deviations of individuals around the
##' mean. This function makes it easy (almost foolproof) to calculate
##' those variables.
##'
##' This was originally just for "group mean-centered" data, but now
##' is more general, can accept functions like median to calculate
##' center and then deviations about that center value within the
##' group.
##'
##' Similar to Stata egen, except more versatile and fun! Will create
##' 2 new columns for each variable, with suffixes for the summary and
##' deviations (default suffixes are "_mn" and "_dev". Rows will match
##' the rows of the original data frame, so it will be easy to merge
##' or cbind them back together.
##'
##' @param dframe a data frame.
##' @param x Variable names or a vector of variable names. Do NOT
##'     supply a variable like dat$x1, do supply a quoted variable
##'     name "x1" or a vector c("x1", "x2")
##' @param by A grouping variable name or a vector of grouping
##'     names. Do NOT supply a variable like dat$xfactor, do supply a
##'     name "xfactor", or a vector c("xfac1", "xfac2").
##' @param FUN Defaults to the mean, have not tested alternatives
##' @param suffix The suffixes to be added to column 1 and column 2
##' @param fulldataframe Default TRUE. original data frame is returned
##'     with new columna added (which I would call "Stata style"). If
##'     FALSE, this will return only newly created columns, the
##'     variables with suffix[1] and suffix[2] appended to names.
##'     TRUE is easier (maybe safer), but also wastes memory.
##' @return Depending on \code{fulldataframe}, either a new data frame
##'     with center and deviation columns, or or original data frame
##'     with "x_mn" and "x_dev" variables appended (Stata style).
##' @author Paul Johnson
##' @importFrom stats aggregate
##' @export
##' @examples
##' ## Make a data frame out of the state data collection (see ?state)
##' data(state)
##' statenew <- as.data.frame(state.x77)
##' statenew$region <- state.region
##' statenew$state <- rownames(statenew)
##' head(statenew.gmc1 <- gmc(statenew, c("Income", "Population"), by = "region"))
##' head(statenew.gmc2 <- gmc(statenew, c("Income", "Population"), by = "region",
##'    fulldataframe = FALSE))
##' ## Note dangerous step: assumes row alignment is correct.
##' ## return has rownames from original set to identify danger
##' head(statenew2 <- cbind(statenew, statenew.gmc2))
##' if(!all.equal(rownames(statenew), rownames(statenew.gmc2))){
##'    warning("Data row-alignment probable error")
##' }
##' ## The following box plots should be identical
##' boxplot(Income ~ region, statenew.gmc1)
##' boxplot((Income_mn + Income_dev) ~ region, statenew.gmc1)
##' ## Multiple by variables
##' fakedat <- data.frame(i = 1:200, j = gl(4, 50), k = gl(20, 10),
##'                       y1 = rnorm(200), y2 = rnorm(200))
##' head(gmc(fakedat, "y1", by = "k"), 20)
##' head(gmc(fakedat, "y1", by = c("j", "k"), fulldataframe = FALSE), 40)
##' head(gmc(fakedat, c("y1", "y2"), by = c("j", "k"), fulldataframe = FALSE))
##' ## Check missing value management
##' fakedat[2, "k"] <- NA
##' fakedat[4, "j"] <- NA##' head(gmc(fakedat, "y1", by = "k"), 20)
##' head(gmc(fakedat, "y1", by = c("j", "k"), fulldataframe = FALSE), 40)
gmc <- function(dframe, x, by, FUN = mean, suffix = c("_mn", "_dev"),
                fulldataframe = TRUE) {
    xmean <- aggregate(dframe[ , x, drop = FALSE],
                       dframe[ , by, drop = FALSE], FUN,
                       na.rm = TRUE)
    dframe.orignrow <- NROW(dframe)
    dframe.origrownames <- rownames(dframe)
    dframe$rownames <-  dframe.origrownames
    dframe$index <- 1:NROW(dframe)
    dframenamez <- setdiff(colnames(dframe), by)
    xmeannamez <- setdiff(colnames(xmean), by)
    xmeannamez_new <- paste0(xmeannamez, suffix[1])
    for (i in seq_along(xmeannamez)){
        colnames(xmean) <-  gsub(xmeannamez[i], xmeannamez_new[i],
                                 colnames(xmean), fixed = TRUE)
    }
    
    df2 <- merge(dframe, xmean, by = by, all.x = TRUE,  sort = FALSE)
    ## put rows back in original order!
    df2 <- df2[order(df2$index), ]
    ## Calculate deviations
    for(i in x){
        df2[ , paste0(i, suffix[2])] <- df2[ , i] - df2[ , paste0(i, suffix[1])]
    }
    rownames(df2) <- df2$rownames
    df2$index <- NULL
    if (dframe.orignrow != NROW(df2)) stop("wrong data frame row count in gmc")
    if (!all.equal(rownames(df2), dframe.origrownames)) stop("wrong data frame row names in gmc")
    if(!isTRUE(fulldataframe)){
        df3 <- df2[c(by, colnames(df2)[!colnames(df2) %in% c(by, dframenamez)])]
        attr(df3, "meanby") <- by
        return(df3)
    }
    ## else
    df2$rownames <- NULL
    attr(df2, "meanby") <- by
    return(df2)
}
