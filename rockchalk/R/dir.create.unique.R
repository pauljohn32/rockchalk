##' Create a uniquely named directory. Appends number & optionally date to directory
##' name.
##' 
##' Checks if the requested directory exists. If so, will create new
##' directory name. My favorite method is to have the target directory
##' with a date-based subdirectory, but set usedate as FALSE if you
##' don't like that. Arguments showWarnings, recursive, and mode are
##' passed along to R's dir.create, which does the actual work here.
##'
##' Default response to dir = "../output/" fixes the directory name
##' like this, "../output/20151118-1/" because usedate is assumed
##' TRUE. If usedate = FALSE, then output names will be like
##' "../output-1/", "../output-2/", and so forth.
##' @param path A character string for the base name of the directory.
##' @param usedate TRUE or FALSE: Insert YYYYMMDD information?
##' @param showWarnings default TRUE. Show warnings? Will be passed on
##'     to dir.create
##' @param recursive default TRUE. Will be passed on to dir.create
##' @param mode Default permissions on unix-alike systems. Will be
##'     passed on to dir.create
##' @export
##' @return a character string with the directory name
##' @author Paul E Johnson \email{pauljohn@@ku.edu}
dir.create.unique <- function(path, usedate = TRUE,
                              showWarnings = TRUE,
                              recursive = TRUE,
                              mode = "0777")
{
    dts <- function(name) gsub("/$", "", name)
    if (!file.exists(dts(path))){
        dir.create(path, recursive = recursive, showWarnings = showWarnings,
                   mode = mode)
        return(paste0(dts(path)), "/")
    } else {
        today <- format(Sys.time(), "%Y%m%d")
        j <- 1
        ocandidate <- paste0(dts(path),
                             ifelse(usedate, paste0("/", today), ""), "-",  j, "/")
        while (file.exists(dts(ocandidate))) {
            j <- j + 1
            ocandidate <- paste0(dts(path),
                                 ifelse(usedate, paste0("/", today), ""), "-",  j, "/")
        }
        path <- ocandidate
        dir.create(path, recursive = recursive, showWarnings = showWarnings,
                   mode = mode)
        return(path)
    }
}

