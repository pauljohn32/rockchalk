set.seed(12345)
x1 <- rnorm(100)
x2 <- rnorm(100)
x3 <- rnorm(100)
x4 <- rnorm(100)
xcat1 <- gl(2,50, labels=c("M","F"))
xcat2 <- cut(rnorm(100), breaks=c(-Inf, 0, 0.4, 0.9, 1, Inf), labels=c("R", "M", "D", "P", "G"))
dat <- data.frame(x1, x2, x3, x4, xcat1, xcat2)
rm(x1, x2, x3, x4, xcat1, xcat2)

##ordinary regression. 
dat$y <- with(dat, 0.03 + 0.1*x1 + 0.1*x2 + 0.4*x3 -0.1*x4 + 2*rnorm(100))
m1 <- lm(y ~ x1 + x2 +x3 + x4, data=dat)
## These will be parallel lines emf 
plotSlopes(m1, plotx="x1", modx="x2")
plotSlopes(m1, plotx="x1", modx="x2", modxVals=c(-0.5,0,0.5))
plotSlopes(m1, plotx="x1", modx="x2", modxVals="std.dev.", main="A plotSlopes result with \"std.dev.\" values of modx")


plotSlopes(m1, plotx="x1", modx="x2", modxVals="std.dev.", ylab="Call Y What You Want")
plotSlopes(m1, plotx="x1", modx="x2")
plotSlopes(m1, plotx="x4", modx="x1")


## now some numeric interactions worth plotting
dat$y2 <- with(dat, 0.03 + 0.1*x1 + 0.1*x2 + 0.25*x1*x2 + 0.4*x3 -0.1*x4 + 1*rnorm(100))

m2 <- lm(y2 ~ x1*x2 + x3 + x4, data=dat)
summary(m2)
plotSlopes(m2, plotx="x1", modx="x2")
plotSlopes(m2, plotx="x1", modx="x2", modxVals=c( -2, -1, 0, 1))
plotSlopes(m2, plotx="x2", modx="x1", modxVals="std.dev.")
plotSlopes(m2, plotx="x2", modx="x1", modxVals="std.dev.", xlab="Any label You Want")

## Catch output, send to testSlopes

m2ps1 <- plotSlopes(m2, plotx="x1", modx="x2")
testSlopes(m2ps1)

### Examples with categorical Moderator variable

stde <- 8
dat$y3 <- with(dat, 3 + 0.5*x1 + 1.2 * (as.numeric(xcat1)-1) +
-0.8* (as.numeric(xcat1)-1) * x1 +  stde * rnorm(100))

m3 <- lm (y3 ~ x1 + xcat1, data=dat)
plotSlopes(m3, modx = "xcat1", plotx = "x1")

m4 <- lm (y ~ x1 * xcat1, data=dat)
summary(m4)
plotSlopes(m4, modx = "xcat1", plotx = "x1")

dat$xcat2n <- with(dat, contrasts(xcat2)[xcat2, ])
dat$y4 <- with(dat, 3 + 0.5*x1 + xcat2n %*% c(0.1, -0.2, 0.3, 0.05)  + stde * rnorm(100))
m5 <- lm(y4 ~ x1 + xcat2, data=dat)
plotSlopes(m5, plotx="x1", modx="xcat2")
m6 <- lm(y4 ~ x1 * xcat2, data=dat)
plotSlopes(m6, plotx="x1", modx="xcat2")

## Make data with a more pronounced interaction
dat$y5 <- with(dat, 3 + 0.5*x1 + xcat2n %*% c(0.1, -0.2, 0.3, 0.05)  + (xcat2n %*% c(0.-1, 0.2, -0.3, 0.25)  )*x1 + stde * rnorm(100))
m7 <- lm(y4 ~ x1 * xcat2, data=dat)
plotSlopes(m7, plotx="x1", modx="xcat2")
##only plot first and third levels
m7ps <- plotSlopes(m7, plotx="x1", modx="xcat2", modxVals=levels(dat$xcat2)[c(1,3)]) 
##see what testSlopes says about this one
##testSlopes(m7ps)

## Now examples with real data
library(car)
m3 <- lm(statusquo ~ income * sex, data = Chile)
summary(m3)
plotSlopes(m3, modx = "sex", plotx = "income")


m4 <- lm(statusquo ~ region * income, data= Chile)
summary(m4)
plotSlopes(m4, modx = "region", plotx = "income")

plotSlopes(m4, modx = "region", plotx = "income", plotPoints=FALSE)


m5 <- lm(statusquo ~ region * income + sex + age, data= Chile)
summary(m5)
plotSlopes(m5, modx = "region", plotx = "income")

m6 <- lm(statusquo ~ income * age + education + sex + age, data=Chile)
summary(m6)
plotSlopes(m6, modx = "income", plotx = "age")

plotSlopes(m6, modx = "income", plotx = "age", plotPoints=FALSE)


## Should cause error because education is not numeric
## m7 <- lm(statusquo ~ income * age + education + sex + age, data=Chile)
## summary(m7)
## plotSlopes(m7, modx = "income", plotx = "education")

## Should cause error because "as.numeric(education") not same as
## plotx="education"
## m8 <- lm(statusquo ~ income * age + as.numeric(education) + sex + age, data=Chile)
## summary(m8)
## plotSlopes(m8, modx = "income", plotx = "education")

## Still fails. 
## plotSlopes(m8, modx = "income", plotx = "as.numeric(education)")

## Must recode variable first so that variable name is coherent
Chile$educationn <- as.numeric(Chile$education)
m9 <- lm(statusquo ~ income * age + educationn + sex + age, data=Chile)
summary(m9)
plotSlopes(m9, modx = "income", plotx = "educationn")
