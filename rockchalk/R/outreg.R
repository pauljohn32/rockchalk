### Paul Johnson ### Adapted from ideas in post in r-help by Dave Armstrong May 8, 2006


##' Creates a publication quality result table for
##' regression models. Works with models fitted with lm, glm, as well
##' as lme4.
##'
##' This provides "markup" that the user is will copy into a LaTeX
##' document. As of rockchalk 1.8.4, can also create HTML markup.
##' The rockchalk vignette demonstrates use of outreg in Sweave.
##' 
##' \code{outreg} returns a string vector. It is suggested that users
##' should save the outreg result and then use cat to save it. That is
##' myMod <- outreg(m1, ...)  cat(myMod, file = "myMod.html") or
##' cat(myMod, file = "myMod.tex".  In version 1.8.66, we write the
##' html file to a temporary location and display it in a web
##' browser. Many word processors will not accept a cut-and paste
##' transfer from the browser, they will, however, be able to open the
##' html file itself and automatically re-format it in the native
##' table format.
##'
##' In version 1.8.111, an argument \code{print.results} was introduced.
##' This is TRUE by default, so the marked-up table is printed into
##' the session, and it is returned as well.  If the function should
##' run silently (as suggested in the last few versions), include
##' \code{print.results = TRUE}. 
##'
##' The table includes a minimally sufficient (in my opinion) model
##' summary.  It offers parameter estimates, standard errors, and
##' minimally sufficient goodness of fit.  My tastes tend toward
##' minimal tables, but users request more features, and
##' \code{outreg}'s interface hass been generalized to allow
##' specialized requests. See \code{request} and \code{runFuns}
##' arguments.
##'
##' I don't want to write a separate table function for every
##' different kind of regression model that exists (how
##' exhausting). So I've tried to revise \code{outreg()} to work with
##' regression functions that follow the standard R framework. It is
##' known to work \code{lm} and \code{glm}, as well as \code{merMod}
##' class from \code{lme4}, but it will try to interact with other
##' kinds of regression models.  Those models should have methods
##' \code{summary()}, \code{coef()}, \code{vcov()} and \code{nobs()}.
##' Package writes should provide those, its not my job.
##'
##' Do you want "robust standard errors"? P values calculated
##' according to some alternative logic?  Go ahead, calculate them in
##' your code, outreg will now accept them as arguments. As of Version
##' 1.8.4, users can provide their own standard errors and/or p-values
##' for each model. Thus, if a model answers in the usual way to the
##' standard R request \code{coef(summary(model))}, outreg can work if
##' users supply standard errors.
##'
##' About the customizations \code{request}.  The \code{request}
##' argument supplies a list of names of summary output elements that
##' are desired. The format is a pair, a value to be retrieved from
##' \code{summary(model)}, and a pretty name to be printed for
##' it. With the \code{lm()} regression, for example, one might want
##' the output of the F test and the adjusted R-square: Include
##' \code{request = c(adj.r.squared = "adj. $R^2$", "fstatistic" =
##' "F")}. The value on the left is the name of the desired
##' information in the summary object, while the value on the right is
##' \emph{any} valid LaTeX (or HTML) markup that the user desires to
##' display in the table. \code{request} terms that generate a single
##' numerical value will generally work fine, while requests that ask
##' for more structured information, such as the F test (including the
##' 2 degrees of freedom values) may work (user feedback needed).
##'
##' The \code{runFuns} argument is inspired by a user request: could
##' this include the BIC or other summaries that can be easily
##' calculated?  Any R function, such as \code{AIC} or \code{BIC},
##' should work, as long as it returns a single value.  This is a
##' two-part specification, a function name and a pretty label to be
##' used in printing. For example, \code{runFuns = c("AIC" = "Akaike
##' Criterion", "BIC" = "Schwartz Criterion", "logLik" = "LL")}.
##'
##' About centering with dcolumn or siunitx. It appears now that
##' results are better with \code{siunitx} but \code{dcolumn} is more
##' familiar to users.  The user has the duty to make sure that the
##' document preamble includes the correct package,
##' \code{\\usepackage\{dcolumn\}} or \code{\\usepackage\{siunitx\}}.
##' In this version, I have eliminated the need for the user to
##' specify document-wide settings for \code{siunitx}. All of the
##' details are explicitly written in the header of each tabular.
##' It is done that way to more easily allow user customizations.
##'
##' @param modelList A regression model or an R list of regression
##'     models. Default model names will be M1, M2, and so forth. User
##'     specified names are allowed, such as \code{list("My Model" =
##'     m1, "Her Model" = m2)}.  This is the currently recommended way
##'     to supply model lables. This is less error prone than the use
##'     of the modelLabels argument.
##' @param type Default = "latex". The alternatives are "html" and
##'     "csv"
##' @param modelLabels This is allowed, but discouraged. A vector of
##'     character string variables, one for each element in
##'     modelList. Will override the names in modelList.
##' @param varLabels To beautify the parameter names printed.  Must be
##'     a named vector in the format c(parmname = "displayName",
##'     parmname = "displayName"). Include as many parameters as
##'     desired, it is not necessary to supply new labels for all of
##'     the parameters.
##' @param tight Table format. If TRUE, parameter estimates and
##'     standard errors are printed in a single column.  If FALSE,
##'     parameter estimates and standard errors are printed side by
##'     side.
##' @param centering Default is "none", but may be "siunitx" or
##'     "dcolumn". No centering has been the only way until this
##'     version. User feedback requested.  Don't forget to insert
##'     usepackage statment in document preamble for siunitx or
##'     dcolumn. If user specifies \code{centering=TRUE}, the
##'     \code{siunitx} method will be used. The \code{dcolumn}
##'     approach assumes that the values reported in the column use
##'     fewer than 3 integer places and 3 decimal places. Additional
##'     room is allocated for the significance stars.
##' @param showAIC This is a legacy argument, before the
##'     \code{request} argument was created.  If TRUE, the AIC
##'     estimate is included with the diagnostic values. It has the
##'     same effect as described by \code{request}.
##' @param float Default = FALSE. Include boilerplate for a LaTeX
##'     table float, with the tabular markup inside it. Not relevant
##'     if type = "html".
##' @param request Extra information to be retrieved from the
##'     summary(model) and displayed. This must be a vector of named
##'     arguments, such as c(adj.r.squared = "adj $R^2$", fstatistic =
##'     "F"). The name must be a valid name of the output object, the
##'     value should be the label the user wants printed in the
##'     table. See details.
##' @param runFuns A list of functions
##' @param digits Default = 3. How many digits after decimal sign are
##'     to be displayed.
##' @param alpha Default = c(0.05, 0.01, 0.001). I think stars are
##'     dumb, but enough people have asked me for more stars that I'm
##'     caving in.
##' @param SElist Optional. Replacement standard errors. Must be a
##'     list of named vectors. \code{outreg} uses the R \code{summary}
##'     to retrieve standard errors, but one might instead want to use
##'     robust or bootstrapped standard errors.  This argument may
##'     supply a new SE vector for each fitted regression model, but
##'     it is also allowed to supply the SE replacement for just one
##'     of the models. The format should be \code{list("A Model Label"
##'     = c(0.1, 0.3, 0.4), "Another Model Label" = c(0.4, 0.2, 0.3)}.
##'     On the left, one must use the same names that are used in the
##'     modelList argument.
##' @param PVlist Optional. A list of replacement "p values". It must
##'     be a list of named vectors, similar in format to SElist. The
##'     which the elements are the "p values" that the user wants to
##'     use for each model.
##' @param Blist Optional. This is only needed in the rare case where
##'     a model's parameters cannot be discerned from its
##'     summary. List must have names for models, and vectors slope
##'     coefficient. See discussion of SElist and PVlist.
##' @param title A LaTeX caption for the table. Not relevant if type =
##'     "html".
##' @param label A string to be used as a LaTeX label in the table to
##'     be created. Not relevant if type = "html".
##' @param gofNames Optional pretty names. R regression summaries use
##'     names like "sigma" or "r.squared" that we might want to revise
##'     for presentation. I prefer to refer to "sigma" as "RMSE", but
##'     perhaps you instead prefer something like \code{gofnames =
##'     c("sigma" = "That Estimate I don't understand", "deviance" =
##'     "Another Mystery")}. The words that you might replace are
##'     "sigma", "r.squared", "deviance", "adj.r.squared",
##'     "fstatistic".
##' @param print.results Default TRUE, marked-up table will be
##'     displayed in session.  If FALSE, same result is returned as an
##'     object.
##' @param browse Display the regression model in a browse? Defaults
##'     to TRUE if type = "html"
##' @export outreg
##' @importFrom lme4 VarCorr
##' @importFrom utils getFromNamespace
##' @import grDevices
##' @rdname outreg
##' @return A character vector, one element per row of the regression
##'     table.
##' @keywords regression
##' @note There are many R packages that can be used to create LaTeX
##'     regression tables. memisc, texreg, apsrtable, xtables, and rms
##'     are some. This "outreg" version was in use in our labs before
##'     we were aware that those packages were in development. It is
##'     not intended as a competitor, it is just a slightly different
##'     version of the same that is more suited to our needs.
##' @author Paul E. Johnson \email{<pauljohn@@ku.edu>}
##' @examples
##' set.seed(2134234)
##' dat <- data.frame(x1 = rnorm(100), x2 = rnorm(100))
##' dat$y1 <- 30 + 5 * rnorm(100) + 3 * dat$x1 + 4 * dat$x2
##' dat$y2 <- rnorm(100) + 5 * dat$x2
##' m1 <- lm(y1 ~ x1, data = dat)
##' m2 <- lm(y1 ~ x2, data = dat)
##' m3 <- lm(y1 ~ x1 + x2, data = dat)
##' gm1 <- glm(y1 ~ x1, family = Gamma, data = dat)
##' outreg(m1, title = "My One Tightly Printed Regression", float = TRUE)
##' ex1 <- outreg(m1, title = "My One Tightly Printed Regression",
##'                float = TRUE, print.results = FALSE, centering = "siunitx")
##' ## Show markup, Save to file with cat()
##' cat(ex1)
##' ## cat(ex1, file = "ex1.tex")
##'  
##' ex2 <- outreg(list("Fingers" = m1), tight = FALSE, 
##'     title = "My Only Spread Out Regressions", float = TRUE,
##'     alpha = c(0.05, 0.01, 0.001)) 
##' 
##' ex3 <- outreg(list("Model A" = m1, "Model B label with Spaces" = m2),
##'     varLabels = list(x1 = "Billie"), 
##'     title = "My Two Linear Regressions", request = c(fstatistic = "F"),
##'     print.results = TRUE)
##' cat(ex3)
##' 
##' ex4 <- outreg(list("Model A" = m1, "Model B" = m2),
##'     modelLabels = c("Overrides ModelA", "Overrides ModelB"),
##'     varLabels = list(x1 = "Billie"),
##'     title = "Note modelLabels Overrides model names")
##' cat(ex4)
##' ##'
##' ex5 <- outreg(list("Whichever" = m1, "Whatever" = m2),
##'     title = "Still have showAIC argument, as in previous versions",
##'     showAIC = TRUE, float = TRUE, centering = "siunitx")
##' 
##' ex5s <- outreg(list("Whichever" = m1, "Whatever" = m2),
##'     title = "Still have showAIC argument, as in previous versions",
##'     showAIC = TRUE, float = TRUE, centering = "siunitx")
##' 
##' \donttest{
##' ## Launches HTML browse
##' ex5html <- outreg(list("Whichever" = m1, "Whatever" = m2),
##'     title = "Still have showAIC argument, as in previous versions",
##'     showAIC = TRUE, type = "html")
##' ## Could instead, make a file:
##' ## fn <- "some_name_you_choose.html"
##' ## cat(ex5html, file = fn)
##' ## browseURL(fn)
##' ## Open that HTML file in LibreOffice or MS Word
##' }
##' 
##' ex6 <- outreg(list("Whatever" = m1, "Whatever" =m2),
##'     title = "Another way to get AIC output",
##'     runFuns = c("AIC" = "Akaike IC"))
##' cat(ex6)
##' 
##' ex7 <- outreg(list("Amod" = m1, "Bmod" = m2, "Gmod" = m3),
##'        title = "My Three Linear Regressions", float = FALSE)
##' cat(ex7)
##' 
##' ## A new feature in 1.85 is ability to provide vectors of beta estimates
##' ## standard errors, and p values if desired. 
##' ## Suppose you have robust standard errors!
##' if (require(car)){
##'    newSE <- sqrt(diag(car::hccm(m3)))
##'    ex8 <- outreg(list("Model A" = m1, "Model B" = m2, "Model C" = m3, "Model C w Robust SE" = m3),
##'         SElist= list("Model C w Robust SE" = newSE))
##'    cat(ex8)
##' }
##' 
##' ex11 <- outreg(list("I Love Long Titles" = m1,
##'           "Prefer Brevity" = m2,
##'           "Short" = m3), tight = FALSE, float = FALSE)
##' cat(ex11)
##' ##'
##' ex12 <- outreg(list("GLM" = gm1), float = TRUE)
##' cat(ex12)
##' 
##' ex13 <- outreg(list("OLS" = m1, "GLM" = gm1), float = TRUE,
##'         alpha = c(0.05, 0.01))
##' cat(ex13)
##' ##'
##' ex14 <- outreg(list(OLS = m1, GLM = gm1), float = TRUE,
##'     request = c(fstatistic = "F"), runFuns = c("BIC" = "BIC"))
##' cat(ex14)

##' ex15 <- outreg(list(OLS = m1, GLM = gm1), float = TRUE,
##'     request = c(fstatistic = "F"), runFuns = c("BIC" = "BIC"),
##'     digits = 5, alpha = c(0.01))
##'
##' ex16 <- outreg(list("OLS 1" = m1, "OLS 2" = m2,  GLM = gm1), float = TRUE,
##'     request = c(fstatistic = "F"),
##'     runFuns = c("BIC" = "BIC", logLik = "ll"),
##'     digits = 5, alpha = c(0.05, 0.01, 0.001))
##'
##' ex17 <- outreg(list("Model A" = gm1, "Model B label with Spaces" = m2),
##'     request = c(fstatistic = "F"),
##'     runFuns = c("BIC" = "Schwarz IC", "AIC" = "Akaike IC",
##'     "nobs" = "N Again?"))
##'
##' ## Here's a fit example from lme4.
##' if (require(lme4) && require(car)){
##'   fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
##'   ex18 <- outreg(fm1)
##'   cat(ex18)
##'   ## Fit same with lm for comparison
##'   lm1 <- lm(Reaction ~ Days, sleepstudy)
##'   ## Get robust standard errors
##'   lm1rse <- sqrt(diag(car::hccm(lm1)))
##' 
##'   if(interactive()){
##'   ex19 <- outreg(list("Random Effects" = fm1, 
##'        "OLS" = lm1, "OLS Robust SE" = lm1),
##'        SElist = list("OLS Robust SE" = lm1rse), type = "html")
##'   }
##'   ## From the glmer examples
##'   gm2 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
##'                    data = cbpp, family = binomial)
##'   lm2 <- lm(incidence/size ~ period,  data = cbpp)
##'   lm2rse <- sqrt(diag(car::hccm(lm2)))
##'   ## Lets see what MASS::rlm objects do? Mostly OK
##'   rlm2 <- MASS::rlm(incidence/size ~ period, data = cbpp)
##'   \donttest{
##'   ex20 <- outreg(list("GLMER" = gm2, "lm" = lm2, "lm w/robust se" = lm2,
##'             "rlm" = rlm2), SElist = list("lm w/robust se" = lm2rse),
##'             type = "html")
##'   } 
##' }
outreg <-
    function(modelList, type = "latex", modelLabels = NULL,  varLabels = NULL,
             tight = TRUE, centering = c("none", "siunitx", "dcolumn"),
             showAIC = FALSE, float = FALSE, request,
             runFuns, digits = 3, alpha = c(0.05, 0.01, 0.001),  SElist = NULL,
             PVlist = NULL,  Blist = NULL, title, label,
             gofNames, print.results = TRUE, 
             browse = identical(type, "html") && interactive())
{

    if (is.character(centering)) {
        centering <- match.arg(tolower(centering), c("none", "siunitx", "dcolumn"))
    } else if (is.logical(centering)){
        centering <- if(isTRUE(centering)) "siunitx" else "none"
    }
    
    myGofNames <- c(sigma = "RMSE",
                    r.squared = paste("_R2_"),
                    adj.r.squared = paste("adj", "_R2_"),
                    fstatistic = "_Fmarkup_",
                    deviance = "Deviance")
 
    if (missing(gofNames)) {
        gofNames <- myGofNames
    } else {
        myGofNames[names(gofNames)] <- gofNames
        gofNames <- myGofNames
    }

    
    if (!missing(request)) gofNames <- c(gofNames, request)

    if (type == "html") float = FALSE
    
    ## Required methods
    req <- (c("coef", "nobs", "vcov", "summary"))

    checkFunctions <- function(obj, req){
        for (i in req){
            if (inherits(get(i)(obj), "try-error")) stop(paste("A model has no", i, "method"))
        }
    }
    
    checkReg <- function(modlist){
        ##Pre-approved model classes
        knownTypes <- c("lm", "glm", "merMod")
        approved <- sapply(modlist, inherits, knownTypes)
        ## Ask trouble modl for a list of all methods that apply to it
        problematic <-  modlist[!approved]
        errors <- lapply(problematic, checkFunctions, req)
    }

   latex.markup <- c(
        "_LB_" = "\\\n",                                                       
        "_EOC_" =  "",                                                                              
        "_BOC_" = "&",                                                                          
        "_EOMC_" = "}",                                                                       
        "_EOR_" = "\\\\tabularnewline",  
        "_BRU_" = "",
        "_BRT_" = "",                                                                       
        "_BOCU_" = "&",                             
        "_BR_" = "",   
        "_EOL_" = "\n",                                                               
        "_HL_" = "\\\\hline", 
        "_UL_" = "\\\\underline{",                                                   
        "_EOUL_" = "}",                                                                   
        "_SEPU_" = " &",                                                              
       "_SEP_" = " &",
       "_ETABULAR_" = "\\\\end{tabular}",  
        "_BOMR1_" = "& \\\\multirow{1}{c}{",
        "_BOMR2_" = "& \\\\multirow{2}{c}{",
        "_BOMC1_" = "\\\\multicolumn{1}{c}{",
        "_BOMC2_" = "\\\\multicolumn{2}{c}{",
        "_BOMC3_" = "\\\\multicolumn{3}{c}{",
        "_BOMC4_" = "\\\\multicolumn{4}{c}{",
        "_BOMC5_" = "\\\\multicolumn{5}{c}{",
        "_BOMC6_" = "\\\\multicolumn{6}{c}{",
        "_BOMC7_" = "\\\\multicolumn{7}{c}{",
        "_BOMC8_" = "\\\\multicolumn{8}{c}{",
        "_BOMC9_" = "\\\\multicolumn{9}{c}{",
        "_BOML1_" = "\\\\multicolumn{1}{l}{",
        "_BOML2_" = "\\\\multicolumn{2}{l}{",
        "_BOML3_" = "\\\\multicolumn{3}{l}{",
        "_BOML4_" = "\\\\multicolumn{4}{l}{",
        "_BOML5_" = "\\\\multicolumn{5}{l}{",
        "_BOML6_" = "\\\\multicolumn{6}{l}{",
        "_BOML7_" = "\\\\multicolumn{7}{l}{",
        "_BOML8_" = "\\\\multicolumn{8}{l}{",
        "_BOML9_" = "\\\\multicolumn{9}{l}{",
        "_BOMCT1_" = "\\\\multicolumn{1}{c}{",
        "_BOMCT2_" = "\\\\multicolumn{2}{c}{",
        "_BOMCT3_" = "\\\\multicolumn{3}{c}{",
        "_BOMCT4_" = "\\\\multicolumn{4}{c}{",
        "_HTMLHL_" = "",
        "_CHI2_" = "$\\\\chi^{2}(\\\\mathrm{df})$",
        "_R2_" = "$R^2$",
        "_X2_" = "$-2LLR (Model \\\\chi^2)$",
        "_Fmarkup_" = "F($df_{num}$,$df_{denom}$)",
        "_DOT_" = paste0("\\\\_"),
        "_LEQ_" = "$\\\\leq$",
        "_SIGMA_" = "$\\\\sigma$",
        "_NBSP_" = " ",
        "_FIXED_" = "$^+$",
        "_STAR0_" = "$\\\\phantom{{^{***}}}$",
        "_STAR1_" = "$^{*}\\\\phantom{{^{**}}}$",
        "_STAR2_" = "$^{**}\\\\phantom{{^{*}}}$",
        "_STAR3_" = "$^{***}$"
    )

    ## Replacement strings for HTML output
    ## TODO: 20171102: refactor abbreviations
    ## Problem in output is duplicate <td><td ..>, workaround in last item"
    html.markup <- c(                                                                                          
        "_LB_" = "<br>",  
        "_EOC_" = "</td>",   
        "_BOC_" = "<td>",   
        "_EOMC_" = "</td>",
        "_EOR_" = "</tr>", 
        "_BRU_" = paste("<tr><td style=\"border-bottom: solid thin black; border-collapse:collapse;\">"), 
        "_BRT_" = paste("<tr><td style=\"border-top: solid thin black; border-collapse:collapse;\">"),
        "_BOCU_" = paste("<td style=\"border-bottom: solid thin black; border-collapse:collapse;\">"),  
        "_BR_" = "<tr><td>",      
        "_BTABULAR_" =  "<table style=\"padding-right:20px;padding-left:20px;\">\n",                           
        "_BT_" =  "<table>\n",
        "_EOL_" = "\n",                                                                                        
        "_HL_" =  "",                                                                                          
        "_UL_" =  "<span style=\"text-decoration: underline;\">",                                              
        "_EOUL_" = "</span>",                                                                                  
        "_SEPU_" = "</td><td style=\"border-bottom: solid thin black; border-collapse:collapse;\">&nbsp;",     
        "_SEP_" = "</td><td>",                                                                                 
        "_ETABULAR_" = "</table>",                                                                             
        "_BOMR1_" = "<td rowspan = '1'>",                                                                      
        "_BOMR2_" = "<td rowspan = '2'>",                                                                      
        "_BOMC1_" = "<td colspan = '1'; align = 'center'>",                                                    
        "_BOMC2_" = "<td colspan = '2'; align = 'center'>",                                                    
        "_BOMC3_" = "<td colspan = '3'; align = 'center'>",                                                    
        "_BOMC4_" = "<td colspan = '4'; align = 'center'>",
        "_BOMC5_" = "<td colspan = '5'; align = 'center'>",
        "_BOMC6_" = "<td colspan = '6'; align = 'center'>",
        "_BOMC7_" = "<td colspan = '7'; align = 'center'>",
        "_BOMC8_" = "<td colspan = '8'; align = 'center'>",
        "_BOMC9_" = "<td colspan = '9'; align = 'center'>",
        "_BOML1_" = "<td colspan = '1'; align = 'left'>",
        "_BOML2_" = "<td colspan = '2'; align = 'left'>",
        "_BOML3_" = "<td colspan = '3'; align = 'left'>",
        "_BOML4_" = "<td colspan = '4'; align = 'left'>",
        "_BOML5_" = "<td colspan = '5'; align = 'left'>",
        "_BOML6_" = "<td colspan = '6'; align = 'left'>",
        "_BOML7_" = "<td colspan = '7'; align = 'left'>",
        "_BOML8_" = "<td colspan = '8'; align = 'left'>",
        "_BOML9_" = "<td colspan = '9'; align = 'left'>",
        "_BOMCT1_" = "<td colspan = '1'; style=\"border-top: solid thin black; border-collapse:collapse;\">&nbsp;",
        "_BOMCT2_" = "<td colspan = '2'; style=\"border-top: solid thin black; border-collapse:collapse;\">&nbsp;",
        "_BOMCT3_" = "<td colspan = '3'; style=\"border-top: solid thin black; border-collapse:collapse;\">&nbsp;",
        "_BOMCT4_" = "<td colspan = '4'; align = 'center'; ; style=\"border-top: solid thin black; border-collapse:collapse;\">&nbsp;",
        "_HTMLHL_" = "<tr><td colspan = '5'; align = 'center'; ; style=\"border-top: solid thin black; border-collapse:collapse;\">&nbsp;</tr>",
        "_DOT_" = "_",    
        "_CHI2_" = "&chi;<sup>2</sup>",
         "_X2_" =  "&chi;<sup>2</sup>",  
        "_R2_" = "R<sup>2</sup>",
        "_LEQ_" = "&le;",
        "_Fmarkup_" = "F(df1,df2)",                   
        "_SIGMA_" = "&sigma;",
        "_NBSP_" = "&nbsp;",
        "_FIXED_" = "<sup>+</sup>",
        "_STAR0_" = "&nbsp;",
        "_STAR1_" = "<sup>*</sup>",
        "_STAR2_" = "<sup>**</sup>",
        "_STAR3_" = "<sup>***</sup>",
        "<td>\\s*<td" = "<td"
    )
    
    ## Replacement strings for CSV output
    csv.markup <- c(
        "_LB_" = "\n",                           
        "_EOC_" =  ",",       
        "_BOC_" = "",                             
        "_EOMC_" = ",",                               
        "_EOR_" = "\n",                              
        "_BRU_" = "",                             
        "_BRT_" = "",                              
        "_BOCU_" = ",",                          
        "_BR_" = "",                               
        "_BTABULAR_" = "",                        
        "_EOL_" = "\n",                             
        "_HL_" = "",                          
        "_UL_" = "",                           
        "_EOUL_" = "",                           
        "_SEPU_" = "",                      
        "_SEP_" = ",",                            
        "_ETABULAR_" = "",                        
        "_BOMR1_" = "",                          
        "_BOMR2_" = "",                              
        "_BOMC1_" = "",                         
        "_BOMC2_" = "",                       
        "_BOMC3_" = "",                             
        "_BOMC4_" = "",
        "_BOMC5_" = "",
        "_BOMC6_" = "",
        "_BOMC7_" = "",
        "_BOMC8_" = "",
        "_BOMC9_" = "",
        "_BOML1_" = "",
        "_BOML2_" = "",
        "_BOML3_" = "",
        "_BOML4_" = "",
        "_BOML5_" = "",
        "_BOML6_" = "",
        "_BOML7_" = "",
        "_BOML8_" = "",
        "_BOML9_" = "",
        "_BOMCT1_" = "",
        "_BOMCT2_" = "",
        "_BOMCT3_" = "",
        "_BOMCT4_" = "",
        "_HTMLHL_" = "",
         "_DOT_" = ".",   
        "_CHI2_" = "chi^2",
        "_R2_" = "R^2",
        "_LEQ_" = "<=",
        "_SIGMA_" = "sigma",
         "_Fmarkup_" = "F",  
        "_NBSP_" = " ",
        "_FIXED_" = "+",
        "_STAR0_" = "", 
        "_STAR1_" = "*",
        "_STAR2_" = "**",
        "_STAR3_" = "**"
    )

   
    markup <- function(x, type){
        if (type == "latex"){
            markup <- latex.markup
        } else if (type == "html"){
            markup <- html.markup
        } else {
            markup <- csv.markup
        }
        
        for(i in names(markup)){
            x <- gsub(i, markup[i], x)
        }
        x
    }

    ## cs = column start
    ## uses environment for tight, defaults as centered
    ## 20190214: TODO figure out the underlining
    bomc <- function(ctr = TRUE, n, underline = FALSE){
        if(missing(n)) n <- if(tight)1 else 2
        if(ctr) {
            return(paste0("_BOMC", n, "_"))
        } else {
            return(paste0("_BOML", n, "_"))
        }
    }
    
    ## TESTME: grabs param from object by name, rounds, simplifies
    ## returns text. For getting r.square, adj.r.square, fstatistic.
    harvest <- function(sl, name) {
        res <- vector("character", length = length(sl))
        names(res) <- names(sl)
        
        for(i in seq_along(sl)) {
            y <- sl[[i]][[name]]
            
            if (length(y) <= 1 && (is.null(y) || is.null(y[1]) ||  is.na(y))) {
                y <- "_BOC__EOC_"
            } else if (name == "fstatistic"){                
                staty <- paste(format(c(y["value"]), digits = digits),
                               "(", format(y["numdf"], digits = digits),
                               ",", format(y["dendf"], digits = digits), ")", sep = "")

                nstars <- sum(pf(y["value"], df1 = y["numdf"], df2 = y["dendf"], lower.tail = FALSE) < alpha)
                y <- paste0("_BOC_", bomc(), staty, paste(rep("*", nstars), collapse = ""), "_EOMC_")
            } else if (is.numeric(y)) {
                if (length(y) > 1){
                    messg <- paste0("outreg: ", 
                                    "asked for: ", paste0("\"", name,"\"."),
                                    "These values were received: \n ",
                                    paste(y, collapse = " "), "\n",  
                                    "Just the first returned value is used for ", name)
                    warning(messg)
                    y <- y[1]
                }
                y <- paste0("_BOC_", format(round(y, digits), nsmall = digits), "_EOC_")
            } 
            if (is.null(y) ||is.na(y)) res[i] <- "" else res[i] <- y
        }
        if (any(res != "")) nonNull <- TRUE else nonNull <- FALSE
        attr(res, "nonNull") <- nonNull
        res
    }

    gofRow <- function(x, xname = "fixme") {
        ## x has markup already! don't re-insert
        zline <- c("_BR_", xname, "_EOC_")
        zline <- paste0(paste0(zline, collapse="") , paste0(x, collapse = paste0(if(!tight) "_BOC__EOC_" else " ")), "_EOR__EOL_")
        zline
    }


    gofPrint <- function(sl, name){
        y <- harvest(sl, name)
        xname <- ifelse(is.na(gofNames[name]), name, gofNames[name])
        if (attr(y, "nonNull")) {
            res <- gofRow(y, xname)
        } else {
            res <- ""
        }
    }

    ## Insert a horizontal line, or as close as we can get in an html table
    SL <- function(n, type) {
        if (type == "latex") x <- "\\hline\n"
        else if (type == "latex") x <- paste0("<tr><td colspan=\'", n, "\'", " style=\"border-bottom:solid thin black;\">&nbsp;</td></tr>\n")
        else x <- "\n"
        x
    }

    ## Double line
    DL <- function(n, type) {
        if (type == "latex") x <- "\\hline\n\\hline\n"
        else if (type == "html") x <- paste0("<tr  style=\"height:5px;\"><td colspan=\'", n, "\'", " style=\"border-bottom:double thin black;\">&nbsp;</td></tr>\n")
        else x <- "\n"
        x
    }
    
    ##Problem: some models have class output like maxLik:
    ## > class(res)
    ## [1] "maxLik" "maxim"  "list"
    ## So we can't just ask modelList if it is a list or an object.
   
    ## So as if the thing is ONLY a list with setequal
    if ( !setequal(class(modelList), "list") ){
        ## modelList is not a list only, so put it in a list
        modelList <- list(modelList)
    }
    checkReg(modelList) ###PJPJP FIXME. OK 20140109
    
    
    nmodels <- length(modelList)
    if (missing(modelLabels) || is.null(modelLabels)) {
        if(!is.null(names(modelList))){
            modelLabels = names(modelList)
        } else {
            modelLabels <- paste0("M", 1:nmodels)
        }
        for (i in seq_along(modelLabels)) {
            modlname <- modelLabels[i]
            if ((modlname == "" || is.na(modlname))) {
                modelLabels[i] <- if(names(modelList[i]) == "") paste0("M", i) else names(modelList[i])
            }
        }
    } else {
        if(length(modelList) != length(modelLabels)) {
            MESSG <- "modelLabels must be provided for each model"
            stop(MESSG)
        }
    }
    
    ##Ugh. nonunique labels. brute force fix
    names(modelList) <- make.unique(modelLabels)

    parmnames <- vector()
    myModelClass <- vector()

    getBSE <- function(modl, alpha, modLab = NULL) {
        if (is.null(best <- tryCatch(Blist[[modLab]], error = function(e) NULL))) {
            if (!is.null(estTable <- coef(summaryList[[modLab]], digits = 11))) {
                validColNum <-  which(colnames(estTable) %in% c("Estimate", "Value", "Param"))
                if (length(validColNum) > 1) stop(paste("Model ", modLab,
                      " has a summary table with unusual column names. They are ",
                      colnames(estTable)))
                if (length(validColNum) == 0) {
                    warning(paste("Model ", modLab, " summary table does not have a column named Estimate, Value, or Param, so we are guessing on column 1"))
                    validColNum <- 1
                }
                best <- estTable[ , validColNum]
            } else {
                best <- coef(modl)
            }
        }
        
        if (is.null(se <- tryCatch(SElist[[modLab]], error = function(e) NULL))) {
            if (!is.null(estTable <- coef(summaryList[[modLab]], digits = 11))) {
                se <- estTable[ , "Std. Error"]
            } else if (!is.null(vcov <- vcov(modl))){
                se <- sqrt(diag(vcov))
            } else {
                stop("outreg:getBSE can't get standard errors")
            }
        }
         
        if (!is.null(DF <- tryCatch(df.residual(modl), error = function(e) NULL))
            | !is.null(DF <- tryCatch(nobs(modl) - length(best), error = function(e) NULL))
            | !is.null(DF <- tryCatch(NROW(model.matrix(modl)), error = function(e) NULL))
            ) {##diagnostic## print("whew, there is a DF")
        } else {
            stop("Sorry, can't find the degrees of freedom on a a model")
        }
            
        if (is.null(PT <- tryCatch(PVlist[[modLab]], error = function(e) NULL))) {
            if (!is.null(estTable <- coef(summaryList[[modLab]], digits = 11)) & is.null(SElist[[modLab]])) {
                if (!is.na(x <-pmatch("Pr", colnames(estTable)))) {
                    PT <- estTable[ , colnames(estTable)[x]]
                } else if ("t value" %in% colnames(estTable)) {
                    PT <- pt(abs(estTable[ , "t value"]), lower.tail = FALSE, df = DF) * 2
                } else if ( "z value" %in% colnames(estTable)) {
                    PT <- pt(abs(estTable[ , "z value"]), lower.tail = FALSE, df = DF) * 2
                    ##fixme. Use Normal?
                }
            } else if(!is.null(best) & !is.null(se) & !is.null(DF)) {
                T <- best/se
                PT <- pt(abs(T), lower.tail = FALSE, df = DF) * 2
            } else {
                print(paste("Sorry, outreg can't figure a way to guess the p values for model", modLab))
            }
        }

       
        stars <- function(x, alpha) {xxx <- sum(abs(x) < alpha)
                                     paste0("", rep("*", xxx), collapse = "")}
        nstars <- sapply(PT, stars, alpha)
        addStar <- function(b, nstar) paste0(format(round(b, digits), nsmall = digits), nstar)
        BSTAR <- best
        for (i in seq_along(best)) BSTAR[i] <- addStar(best[i], nstars[i])
        names(BSTAR) <- names(best)
        se <- paste0("(", format(round(se, digits), nsmall = digits), ")")
       
        res <- data.frame(B = BSTAR, SE = se, stringsAsFactors = FALSE)
        rownames(res) <- names(best) 
        res
    }


    myModelClass <- lapply(modelList, function(x) {class(x)[1]})
    
    ## Get a regression summary object for each fitted model
    summaryList <- lapply(modelList, summary)
    ##    summaryList <- lapply(modelList, function(x) tryCatch(summary(x), error = NULL))
    
    ##    BSEs <- lapply(modelList, getBSE, alpha)
    BSEs <- vector("list", length = length(modelList))
    for(i in seq_along(modelList)){
        BSEs[[i]] <- getBSE(modelList[[i]], alpha, names(modelList)[i])
    }
    
    names(BSEs) <- modelLabels
  
    parmnames <- unique(unlist(lapply(BSEs, function(bse) rownames(bse))))
    B <- SE <- matrix(NA, nrow = length(parmnames), ncol =
                length(modelList), dimnames = list(parmnames, modelLabels))
    for (j in seq_along(modelList)){
        modl <- BSEs[[j]]
        modlnames <- rownames(BSEs[[j]])
        B[modlnames, j] <-  modl[modlnames, "B"]
        SE[modlnames, j] <- modl[modlnames, "SE"]
    }

    displayNames <- as.character(parmnames)
    names(displayNames) <- as.character(parmnames)
    displayNames[names(varLabels)] <- varLabels

    ## 20190214: new idea to fiddle lines HTML
    fudgeLength <- (1 + !tight) * length(modelList)
    blankline <- paste0("_BRU__EOC_", paste0(rep(paste0("_BOCU__EOC_"), fudgeLength), collapse=""), "_EOR__EOL_")
    
    getVC.merMod <- function(modl){
        if(inherits(modl, "merMod")){
            vc <- lme4::VarCorr(modl)
            formatVC <- getFromNamespace("formatVC", "lme4")
            vcfmt <- formatVC(vc, 3, "Std.Dev.")
            vcfmt[ ,2] <- gsub("\\(Intercept\\)", "", vcfmt[ ,2])
            
            for ( i in seq_along(vcfmt[ ,1])){
                if (i == 1) next
                if (vcfmt[i, 1] == "") vcfmt[i, 1] <- vcfmt[ (i-1), 1]
            }
            vcfmt[ ,1] <- paste0(vcfmt[ ,1], ":", vcfmt[ ,2])
            vcfmt[ ,1] <- gsub(":$", "", vcfmt[ ,1])
            vcfmt <- vcfmt[ , c("Groups", "Std.Dev."), drop = FALSE]
        } else {
           ## vcfmt <- cbind("Groups" = "", "Std.Dev." = "")
            vcfmt <- NULL
        }
        vcfmt
    }


    getVCmat <- function(modelList, modelLabels){
        VCs <-lapply(modelList, getVC.merMod)
       
        ##names(VCs) <- modelLabels
        vcnames <- unique(unlist(lapply(modelLabels, function(bsen) VCs[[bsen]][, "Groups", drop = FALSE])))
        
        if(any("Residual" %in% vcnames)) {
            if(!is.na(indx <- match("Residual" , vcnames))) {
                vcnames <- vcnames[ -indx ]
                vcnames <- c("Residual", vcnames)
        }}
        
        VCmat <- matrix("   ", nrow = length(vcnames), ncol =
                        length(modelList), dimnames = list(vcnames, modelLabels))
        for (i in seq_along(VCs)) {
            vc <- VCs[[i]]
            labl <- modelLabels[i]
            VCmat[vc[ , "Groups", drop = F], labl] <- vc[ , "Std.Dev."] 
        }
        VCmat
    }
    
    VCmat <- getVCmat(modelList, modelLabels)

    printVC <- function(VCmat){
        if (!any(VCmat != "")) return()
        aline <- paste0("_BR_", "Random Effects (_SIGMA_)", "_EOC__EOR__EOL_")
        if (tight) hereSep <- " _SEP_ " else hereSep <-  "_SEP_ _SEP_ "
 
        bline1 <- paste0("_BR__NBSP_", paste0(rownames(VCmat)))
        bline2 <- paste(rep(" ", max(2, (5 - nchar(rownames(VCmat))))), collapse = "")
        bline3 <- paste(apply(VCmat, 1, paste, collapse = hereSep), "_EOC__EOR__EOL_")
        bline <- paste(bline1, bline2, "_SEP_ ",  bline3)
        c(aline, bline)
    }
    
    z <- c()
    ## If you want a LaTeX table float...
    if (type == "latex") {
        if (float == TRUE || !missing(title) || !missing(label)){
            float <- TRUE
            aline <- "\\begin{table}\n"
            if (missing(title)) title <- "A Regression"
            if (missing(label)) label <- "regrlabl"
            z <- paste0(aline, "\\caption{", title, "}\\label{", label,"}\n")
        }
    }
    
    nColumns <- ifelse(tight, 1 + nmodels, 1 + 2*nmodels)
    ## siunitxmarkup
    ## Smarkup <- paste0("S[table-format=1.", digits, ", table-align-text-post=false]")
    Smarkup <- paste0("{S[
                         input-symbols = ( ),   
                         group-digits = false,   
                         table-number-alignment = center,   
                         %table-space-text-pre = (, 
                         table-align-text-pre = false,
                         table-align-text-post = false,
                         table-space-text-post = {", paste0(rep("*", length(alpha)), collapse=""),"},   
                         parse-units = false]}")
    ## Dcolumn treat all columns same with digits, not quite perfect
    ## Here we guess on 3.6, seems only way out of bad mis-alignment
    Dmarkup  <- paste0("{D{.}{.}{3.", digits + length(alpha), "}}")
    
    BT <- function(n, type = "latex"){
        if (type == "latex") {
            if(centering == "dcolumn"){
                return(paste0("\\begin{tabular}{@{}l*{",n-1,"}", Dmarkup, "@{}}\n", SL(n, type)))
            } else if (centering == "siunitx"){
                return(paste0("\\begin{tabular}{@{}l*{", n-1,"}", Smarkup, "@{}}\n", SL(n, type)))
            }
            else {
                return(paste0("\\begin{tabular}{@{}l*{",n,"}{l}@{}}\n", SL(n, type)))
            }
        }
        if (type == "html")  return(paste("<table>\n", SL(n, type)))
    }

    aline <- paste0(BT(nColumns, type = type))
    z <- c(z, aline)
 
    ## Put model labels on top of each model column, if modelLabels were given
    if (!is.null(modelLabels)){
        aline <- paste0("_BR_","_EOC_", collapse = "")
        for (modelLabel in modelLabels){
                 aline <- c(aline, paste0("_NBSP__BOC_", bomc(centering %in% c("dcolumn", "siunitx")), modelLabel, "_NBSP_ _EOMC_"))
        }
        aline <- c(aline, "_EOR__EOL_")
        z <- c(z, paste0(aline, collapse = ""))
    }

    ## Print the headers "Estimate" and "(S.E.)", output depends on tight or other format
    if (tight == TRUE) {
        aline <- paste0("_BR__EOC_", paste0(rep(paste0("_BOC_", bomc(centering %in% c("dcolumn", "siunitx")), "Estimate_EOMC_"), nmodels), collapse = ""),
                       "_EOR__EOL_", collapse = "") 
        z <- c(z, paste0(aline, collapse = ""))
        ##aline <- c("_BRU_", sprintf("%2s", " "), paste(rep ("_EOC__BOCU_ (S.E.)", nmodels, collapse = "")), "_EOR__EOL_")
        aline <- c("_BR__EOC_", paste0(rep(paste0("_BOC_", bomc(centering %in% c("dcolumn", "siunitx")), "(S.E.)_EOMC_"), nmodels, collapse = "")), "_EOR__EOL_")
        z <- c(z, paste0(aline, collapse = ""))
    } else {
        aline1 <- paste0("_BR__EOC_")
        aline2 <- paste0(rep ("_BOC__BOMC1_Estimate_EOMC__BOC__BOMC1_(S.E.)_EOMC_", nmodels), collapse = "")
        aline3 <- paste0("_EOR__EOL_")
        z <- c(z, paste0(aline1, aline2, aline3, collapse = ""))
    }

    if (type == "latex") z <- c(z, SL(1, "latex"), SL(1, "latex"))
    if (type == "html") z <- c(z, blankline)
   
    ## Here come the regression coefficients
    for (regname in parmnames){
        aline <- paste(paste("_BR_", displayNames[regname], "_EOC_"), collapse = "")
        for (model in modelLabels) {
            est <- B[regname, model]
            se <- SE[regname, model]
            if (!is.na(est)) {
                aline <- c(aline, paste("_BOC_", est, "_EOC_", collapse = " "))
                if (tight == FALSE) {
                    aline <- c(aline, paste("_BOC_", se, "_EOC_", collapse = " "))
                }
            } else {
                aline <- c(aline, paste0("_BOC_", bomc(centering %in% c("dcolumn", "siunitx"), 1), "_DOT_ _EOMC_", if(tight==FALSE) "_BOC__EOC_"))
            }
        }
        aline <- c(aline, "_EOR__EOL_")
        z <- c(z, paste(aline, collapse = ""))

        if (tight == TRUE){
            aline <- "_BR__EOC_"
            for (model in modelLabels) {
                est <- B[regname, model]
                se <- SE[regname, model]
                aline2 <- if (!is.na(est)) c("_BOC_", se, "_EOC_")  else c(" _BOC__EOC_")
                aline <- c(aline, paste(aline2, collapse = ""))
            }
            aline <- c(aline, "_EOR__EOL_")
            z <- c(z, paste(aline, collapse = ""))
        }
    }

    aline <- SL(nColumns, type)
    z <- c(z, aline)

  
    ## Print a row for the number of cases
    ##fiddline here to get line above N
    aline <- c("_BR_", "N", "_EOC_")
    for (model in modelList) {
        myN <- stats::nobs(model)
        columnFudge <- if(centering %in% c("dcolumn", "siunitx")) "_BOMC1_" else "_BOML1_"
        aline <- c(aline, "_BOC_", columnFudge,  myN, "_EOMC_", if(tight==FALSE) "  _BOC__EOC_" else "")
    }
    ##    if (type == "html") z <- c(z, blankline)
    aline <- c(if(type == "html") blankline else "", aline, " _EOR__EOL_")
    z <- c(z, paste(aline, collapse = ""))

    ## The new way
    z <- c(z, gofPrint(summaryList, "sigma"))
   
    ## The new way
    z <- c(z, gofPrint(summaryList, "r.squared"))

    ##"adj.r.squared" if there is more than 1 predictor

    ## Print a row for the adj-R-square
    if (length(parmnames) > 2) {
        z <- c(z, gofPrint(summaryList, "adj.r.squared"))
    }

    if (!missing(request)){
        for (extra in names(request)){
            z <- c(z, gofPrint(summaryList, extra))
        }
    }

    z <- c(z, printVC(VCmat))


    ## Print a row for the model residual deviance
    if ("glm" %in% myModelClass) {
        z <- c(z, gofPrint(summaryList, "deviance"))
    }

    ## Print a row for the model's fit, as -2LLR
    if ("glm" %in% myModelClass) {
        aline <- "_BR__X2_ "
        for (model in modelList) {
            if (is.numeric(model$deviance)){
                n2llr <- model$null.deviance - model$deviance
                aline <- c(aline, paste("_SEP_  ", format(round(n2llr, digits), nsmall = digits)))
                gmdf <- model$df.null - model$df.residual + 1
                nstars <- sum(pchisq(n2llr, df = gmdf, lower.tail = FALSE) < alpha)
                aline <-  paste0(paste(aline, collapse = ""), paste0(rep("*", nstars), collapse=""))
            } else {
                aline <- c(aline, "_SEP_ ")
            }
            if (tight == FALSE) aline <- c(aline, "_SEP_  ")
        }
        aline <- paste(paste(aline, collapse = ""), "_EOR__EOL_")
        z <- c(z, paste(aline))
    }

    ## Print a row for the model's fit, as -2 LLR
    ## Can't remember why I was multiplying by -2
    if (showAIC == TRUE) {
        aicList <- lapply(modelList, function(x) {
            aic.x <- AIC(x)
            y <- if(is.numeric(aic.x)) format(aic.x, digits=digits, nsmall=3) else ""
            paste0("_BOC_", y, "_EOC_")
        })
        z <- c(z, gofRow(aicList, "AIC"))
    }

    ## TODO: round the following output
    if (!missing(runFuns)){
        elist <- vector("list", length = length(runFuns))
        runFunsFn <- names(runFuns)
        for (i in seq_along(runFuns)){
            myfn <- runFunsFn[i]
            if (myfn == "logLik") {
                myresult <- lapply(modelList, function(x) {
                    y <- do.call(myfn, list(x))
                    fstaty <- paste(format(y[1], digits = digits), collapse = ", ",
                                    "(", format(attr(y, "df")), ")", sep = "")
                    fstaty <- paste0(if(tight)"_BOC__BOMC1_" else "_BOMC2_", fstaty, "_EOMC_")
                    invisible(fstaty)
                })
                elist[[i]] <- myresult
            } else {
                myresult <- lapply(modelList, function(x){
                    y <- do.call(myfn, list(x))
                    fstaty <- format(c(y), digits = digits, nsmall = 2)
                    fstaty <- paste0(if(tight)"_BOC__BOMC1_" else "_BOMC2_", fstaty, "_EOMC_")
                })
                elist[[i]] <- myresult
            }
        }
        names(elist) <- runFunsFn

        for(i in seq_along(runFuns)){
            z <- c(z, gofRow(elist[[i]], runFuns[i]))
        }
    }

    aline <- DL(nColumns, type)
    z <- c(z, aline)
    z <- c(z, "_EOL_")

    

    pline <- function(type, alpha){
        aline <- "_NBSP__NBSP_"
        if (type == "latex"){
            for ( i in seq_along(alpha)){
                aline <- paste0(aline, "${", paste0(rep("*", i), collapse = "\\!\\!"), "\  p}",  "\\le ", alpha[i], "$", sep = "")
            }
            aline <- paste0("\\multicolumn{", nColumns, "}{l}{", aline, "_EOMC__EOR__EOL_")
        } else if (type == "html"){
            aline <- paste0("<tr>\n",
                            "<td colspan=\"", nColumns, "\">")
            for ( i in seq_along(alpha)){
                aline <- paste0(aline,  paste0(rep("*", i), collapse = ""), " <it>p</it> &#8804;", alpha[i], sep = "")
            }
            aline <- paste0(aline, "_EOR__EOL_")
        } else {
            for ( i in seq_along(alpha)){
                aline <- paste0(aline,  paste0(rep("*", i), collapse = ""), "p <", alpha[i], sep = ",")
            }
            aline <- paste0(aline, "\n")
        }
        aline
    }

        
    z <- c(z, pline(type, alpha))

    aline <- "_ETABULAR__EOL_"
    z <- c(z, aline)
    if (float == TRUE && type == "latex"){
        aline <- "\\end{table}_EOL_\n"
        z <- c(z, aline)
    }
    
    zresult <- markup(z, type = type)
    matchCall <- match.call()
    matchCall[["type"]] <- "html"
               
    if (type == "latex" || type == "csv") {
        if (print.results) cat(zresult)
        return(invisible(zresult))
    } else if (browse) {
        fn <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".html")
        cat(zresult, file = fn)
        cat(paste("HTML file : ", fn, "\n"))
        browseURL(fn)
        attr(zresult, "file") <- fn
        return(invisible(zresult))
    } else {
        if (print.results) cat(zresult)
        return(invisible(zresult))
    }
}

NULL



##' Convert LaTeX output from outreg to HTML markup
##'
##' This function is deprecated. Instead, please use \code{outreg(type = "html")}
##'
##' This will write the html on the screen, but if a filename argument is
##' supplied, it will write a file. One can
##' then open or insert the file into Libre Office or other popular
##' "word processor" programs.
##'
##' @param outreg output from outreg
##' @param filename A file name into which the regression markup is to be saved. Should end in .html.
##' @return A vector of strings
##' @export
##' @author Paul E. Johnson \email{<pauljohn@@ku.edu>}
##' @examples
##' dat <- genCorrelatedData2(means = c(50,50,50,50,50,50),
##'     sds = c(10,10,10,10,10,10), rho = 0.2, beta = rnorm(7), stde = 50)
##' m1 <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x1*x2, data = dat)
##' summary(m1)
##'
##' m1out <- outreg(list("Great Regression" = m1), alpha = c(0.05, 0.01, 0.001),
##'          request = c("fstatistic" = "F"), runFuns = c(AIC = "AIC"),
##'          float = TRUE)
##' ##html markup will appear on screen
##' outreg2HTML(m1out)
##  ## Run this for yourself to create an output file
##' ## outreg2HTML(m1out, filename = "funky.html")
##' ## I'm not running that for you because you
##' ## need to be in the intended working directory
##'
##' m2 <- lm(y ~ x1 + x2, data = dat)
##'
##' m2out <- outreg(list("Great Regression" = m1, "Small Regression" = m2),
##'                alpha = c(0.05, 0.01, 0.01),
##'                 request = c("fstatistic" = "F"), runFuns = c(BIC = "BIC"))
##' outreg2HTML(m2out)
##' ## Run this for yourself, it will create the output file funky2.html
##' ## outreg2HTML(m2out, filename = "funky2.html")
##' ## Please inspect the file "funky2.html
##'
outreg2HTML <-
    function(outreg, filename)
{
    myz2 <- gsub("^\\n$", "</tr></td>\n", outreg)
    myz2 <- gsub("^", "<tr><td>", myz2)
    myz2 <- gsub(".*\\\\begin\\{tabular\\}.*$", "<table>\n", myz2)
    myz2 <- gsub("\\\\\\\\","</td></tr>", myz2)
    myz2 <- gsub("^.*\\\\hline.*", "", myz2)
    myz2 <- gsub("&", "</td><td>", myz2)
    myz2 <- gsub(".*end\\{tabular\\}", "</table>", myz2)
    myz2 <- gsub("\\\\le", "&#8804;", myz2)
        ## Emacs indentation fooled by previous
        myz2 <- gsub("\\$R\\^2\\$", "R<sup>2</sup>", myz2)

    myz2 <- sub("<td>\\\\mul(.*?)\\$\\{", "<td colspan = '3'>",  myz2)
    myz2 <- gsub("<td>\\\\multicolumn\\{(\\d+)\\}\\{.*?\\}\\{(.*?)\\}", "<td colspan = '\\1'> \\2", myz2)


    myz2 <- gsub("\\$\\{", "", myz2)
    myz2 <- gsub("(\\**)}", "\\1", myz2)
    myz2 <- gsub("\\$\ *", " ", myz2)
    myz2 <- gsub("\\\\chi\\^2", "&chi;<sup>2</sup>", myz2)

    if (!missing(filename)){
        if (!checkIntFormat(filename))
            stop("invalid 'file'")
        cat(myz2, file = filename)
        cat(paste("Saved to ", filename, "\n"))
        } else {
            cat(myz2)
        }
    invisible(myz2)
}
