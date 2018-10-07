### R code from vignette source 'outreg-test-20181005-uniquebackupstring.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: outreg-test-20181005-uniquebackupstring.Rnw:29-30
###################################################
  if(exists(".orig.enc")) options(encoding = .orig.enc)


###################################################
### code chunk number 2: tmpout
###################################################
if(!dir.exists("tmpout")) dir.create("tmpout", showWarnings=FALSE)


###################################################
### code chunk number 3: Roptions
###################################################
opts.orig <- options()
options(width=100, prompt="> ")
options(useFancyQuotes = FALSE) 
set.seed(12345)
options(SweaveHooks=list(fig=function() par(ps=10)))
pdf.options(onefile=FALSE,family="Times",pointsize=10)


###################################################
### code chunk number 4: outreg-test-20181005-uniquebackupstring.Rnw:471-492
###################################################
fn1 <- "theme/logoleft.pdf"
fn2 <- "theme/logo-vert.pdf"
if(!file.exists("theme")) dir.create("theme")
if(!file.exists(fn1)){
    pdf(file = fn1, width=3, height=3, paper="special", 
        onefile=FALSE, pointsize=20)
    par(mar=c(1,1,1,1))
    plot(1:2, 1:2, type = "n", axes=FALSE, xlab="", ylab="")
    text(1.5, 1.5, "left\n logo", axes=FALSE) 
    box(which="plot")
    dev.off()
}
if(!file.exists(fn2)){
    pdf(file = fn2, width=3, height=3, paper="special", 
        onefile=FALSE, pointsize=20)
    par(mar=c(1,1,1,1)) 
    plot(1:2, 1:2, type = "n", axes=FALSE, xlab="", ylab="")
    text(1.5, 1.5, "right \n logo", axes=FALSE)
    box(which="plot")
    dev.off()
}


###################################################
### code chunk number 5: outreg-test-20181005-uniquebackupstring.Rnw:566-570 (eval = FALSE)
###################################################
## CRAN <- "http://rweb.crmda.ku.edu/cran" 
## KRAN <- "http://rweb.crmda.ku.edu/kran"
## options(repos = c(KRAN, CRAN))
## update("rockchalk")


###################################################
### code chunk number 6: outreg-test-20181005-uniquebackupstring.Rnw:680-688
###################################################
    set.seed(2134234)
     dat <- data.frame(x1 = rnorm(100), x2 = rnorm(100))
     dat$y1 <- 30 + 5 * rnorm(100) + 3 * dat$x1 + 4 * dat$x2
     dat$y2 <- rnorm(100) + 5 * dat$x2
     m1 <- lm(y1 ~ x1, data = dat)
     m2 <- lm(y1 ~ x2, data = dat)
     m3 <- lm(y1 ~ x1 + x2, data = dat)
     gm1 <- glm(y1 ~ x1, family = Gamma, data = dat)


###################################################
### code chunk number 7: ex1
###################################################
library(rockchalk)
ex1 <- outreg(m1, title = "My One Tightly Printed Regression",
                    label = "tab:ex1",
                    float = TRUE, print.results = FALSE)
# cat that, don't print it
cat(ex1)


###################################################
### code chunk number 8: ex1w
###################################################
library(rockchalk)
ex1w <- outreg(m1, title = "My Wide Format \"side-by-side\" columns",
               label = "tab:ex1w", tight = FALSE,
               float = TRUE, print.results = FALSE)
cat(ex1w)


###################################################
### code chunk number 9: ex1d
###################################################
library(rockchalk)
ex2d <- outreg(m1, title = "Tight column with dcolumn=TRUE",
               label = "tab:ex2d", dcolumn = TRUE,
               float = TRUE, print.results=FALSE)
cat(ex2d)


###################################################
### code chunk number 10: ex2wd
###################################################
library(rockchalk)
ex1wd <- outreg(m1, title = "Wide (not tight) format with dcolumn = TRUE",
                label = "tab:ex2wd", tight = FALSE, dcolumn = TRUE,
                float = TRUE, print.results = FALSE)
cat(ex1wd)


###################################################
### code chunk number 11: outreg-test-20181005-uniquebackupstring.Rnw:748-754
###################################################
ex2pd <- outreg(list("Fingers" = m1), tight = FALSE, 
         title = "Ability to change p values with decimal-centered columns", 
         label = "tab:ex2pd", dcolumn = TRUE,
         float = TRUE,
         alpha = c(0.1, 0.05, 0.01)) 
## I didn't put print.results=FALSE, so no need to cat result here


###################################################
### code chunk number 12: outreg-test-20181005-uniquebackupstring.Rnw:763-769
###################################################
ex2p <- outreg(list("Fingers" = m1), tight = FALSE, 
         title = "Ability to change p values with decimal-centered columns", 
         label = "tab:ex2p", dcolumn = FALSE,
         float = TRUE,
         alpha = c(0.1, 0.05, 0.01)) 
## I didn't put print.results=FALSE, so no need to cat result here


###################################################
### code chunk number 13: outreg-test-20181005-uniquebackupstring.Rnw:787-793
###################################################
ex3 <- outreg(list("Model A" = m1, "Model B has a longer heading" = m2),
         varLabels = list(x1 = "Billie"), 
         title = "My Two Linear Regressions", label = "tab:ex3",
         request = c(fstatistic = "F"),
         print.results = FALSE)
cat(ex3)


###################################################
### code chunk number 14: outreg-test-20181005-uniquebackupstring.Rnw:803-809
###################################################
ex3b <- outreg(list("Model A" = m1, "Model B" = m2),
         modelLabels = c("Overrides ModelA", "Overrides ModelB"),
         varLabels = list(x1 = "Billie"),
         title = "Note modelLabels Overrides model names",
         label = "tab:ex3b"
)


###################################################
### code chunk number 15: ex5d
###################################################
ex5d <- outreg(list("Whichever" = m1, "Whatever" = m2),
         title = "Still have showAIC argument, as in previous versions",
         label = "tab:ex5d", showAIC = TRUE, float = TRUE)


###################################################
### code chunk number 16: ex6d
###################################################
ex6d <- outreg(list("Whatever" = m1, "Whatever" =m2),
         title = "Another way to get AIC output", label="ex6d",
         runFuns = c("AIC" = "Akaike IC"), dcolumn=TRUE, print.results=FALSE)
cat(ex6d)


###################################################
### code chunk number 17: outreg-test-20181005-uniquebackupstring.Rnw:866-868
###################################################
ex7 <- outreg(list("Amod" = m1, "Bmod" = m2, "Gmod" = m3), dcolumn=FALSE,
              title = "My Three Linear Regressions", label="tab:ex7")


###################################################
### code chunk number 18: outreg-test-20181005-uniquebackupstring.Rnw:874-876
###################################################
ex7d <- outreg(list("Amod" = m1, "Bmod" = m2, "Gmod" = m3), dcolumn=TRUE,
              title = "My Three Linear Regressions (decimal aligned)", label="tab:ex7d")


###################################################
### code chunk number 19: outreg-test-20181005-uniquebackupstring.Rnw:889-893
###################################################
ex11 <- outreg(list("I Love Long Titles" = m1,
               "Prefer Brevity" = m2,
               "Captain. Kirk. Named. This." = m3), tight = FALSE, float = FALSE,
               dcolumn = TRUE)


###################################################
### code chunk number 20: outreg-test-20181005-uniquebackupstring.Rnw:900-904
###################################################
ex1t1 <- outreg(list("I Love Long Titles" = m1,
               "Prefer Brevity" = m2, 
               "Captain. Kirk. Named. This" = m3), float = FALSE,
               dcolumn = TRUE)


###################################################
### code chunk number 21: outreg-test-20181005-uniquebackupstring.Rnw:923-929
###################################################
if (require(car)){
   newSE <- sqrt(diag(car::hccm(m3)))
   ex8 <- outreg(list("Model A" = m1, "Model B" = m2, "Model C" = m3, 
             "Model C w Robust SE" = m3),
             SElist= list("Model C w Robust SE" = newSE))
}


###################################################
### code chunk number 22: outreg-test-20181005-uniquebackupstring.Rnw:942-945
###################################################
ex13 <- outreg(list("OLS" = m1, "GLM" = gm1), float = TRUE,
               title = "OLS and Logit in same table (dcolumn)", 
               label="tab:ex13", alpha = c(0.05, 0.01), dcolumn = TRUE)


###################################################
### code chunk number 23: outreg-test-20181005-uniquebackupstring.Rnw:951-956
###################################################
ex14 <- outreg(list(OLS = m1, GLM = gm1), float = TRUE,
         title = "OLS and Logit with summary report features (dcolumn)",
         label = "tab:ex14",
         request = c(fstatistic = "F"), runFuns = c("BIC" = "BIC"),
         dcolumn = TRUE)


###################################################
### code chunk number 24: outreg-test-20181005-uniquebackupstring.Rnw:962-967
###################################################
ex15 <- outreg(list(OLS = m1, GLM = gm1), float = TRUE,
         title="OLS and GLM with more digits (digits)", 
         label="tab:ex15", 
         request = c(fstatistic = "F"), runFuns = c("BIC" = "BIC"),
         digits = 5, alpha = c(0.01), dcolumn = TRUE)


###################################################
### code chunk number 25: outreg-test-20181005-uniquebackupstring.Rnw:974-980
###################################################
ex16 <- outreg(list("OLS 1" = m1, "OLS 2" = m2,  GLM = gm1), float = TRUE,
           title = "2 OLS and 1 Logit (dcolumn), additional runFuns", 
           label="tab:ex16",
           request = c(fstatistic = "F"),
           runFuns = c("BIC" = "BIC", "logLik" = "ll"),
           digits = 5, alpha = c(0.1, 0.05, 0.01), dcolumn = TRUE)


###################################################
### code chunk number 26: outreg-test-20181005-uniquebackupstring.Rnw:992-996
###################################################
ex17 <- outreg(list("Model A" = gm1, "Model B label with Spaces" = m2),
         request = c(fstatistic = "F"),
         runFuns = c("BIC" = "Schwarz IC", "AIC" = "Akaike IC", "logLik" = "ll",
         "nobs" = "N Again?"), dcolumn=TRUE)


###################################################
### code chunk number 27: session
###################################################
sessionInfo()
if(!is.null(warnings())){
    print("Warnings:")
    warnings()}


###################################################
### code chunk number 28: opts20
###################################################
## Don't delete this. It puts the interactive session options
## back the way they were. If this is compiled within a session
## it is vital to do this.
options(opts.orig)


