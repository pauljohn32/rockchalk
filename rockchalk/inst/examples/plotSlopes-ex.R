## Manufacture some predictors
set.seed(12345)
x1 <- rnorm(100)
x2 <- rnorm(100)
x3 <- rnorm(100)
x4 <- rnorm(100)
xcat1 <- gl(2,50, labels = c("M", "F"))
xcat2 <- cut(rnorm(100), breaks = c(-Inf, 0, 0.4, 0.9, 1, Inf), labels = c("R", "M", "D", "P", "G"))
dat <- data.frame(x1, x2, x3, x4, xcat1, xcat2)
rm(x1, x2, x3, x4, xcat1, xcat2)

## ordinary regression.
dat$y <- with(dat, 0.03 + 0.5 * x1 - 0.45 * x2 + 0.60 * x1 * x2 + 0.4 * x3 - 0.1 * x4 + 2*rnorm(100))

m1 <- lm(y ~ x1 * x2 +x3 + x4, data = dat)
summary(m1)

## New in rockchalk 1.7.x. No modx required:
plotSlopes(m1, plotx = "x1")

## Confidence interval, anybody?
plotSlopes(m1, plotx = "x1", interval = "conf")

## Prediction interval.
plotSlopes(m1, plotx = "x1", interval = "pred")

## Now experiment with a moderator variable
## let default quantile algorithm do its job
plotSlopes(m1, plotx = "x1", modx = "x2")

plotSlopes(m1, plotx = "x1", modx = "x2", modxVals = c(-2, 0, 0.5))

## standard-deviation based algorithm
plotSlopes(m1, plotx = "x1", modx = "x2", modxVals = "std.dev.", main = "A plotSlopes result with \"std.dev.\" values of modx", ylab = "Call Y What You Want", xlab = "My X is Awesomer")

plotSlopes(m1, plotx = "x4", modx = "x1")


## Catch output, send to testSlopes

m2ps1 <- plotSlopes(m1, plotx = "x1", modx = "x2")
m2ps1ts <- testSlopes(m2ps1)
plot(m2ps1ts)

### Examples with categorical Moderator variable

stde <- 8
dat$y3 <- with(dat, 3 + 0.5*x1 + 3.2 * (as.numeric(xcat1)-1) +
-2.8* (as.numeric(xcat1)-1) * x1 +  stde * rnorm(100))

m3 <- lm (y ~ x1 + xcat1, data = dat)
plotSlopes(m3, modx = "xcat1", plotx = "x1")

m4 <- lm (y3 ~ x1 * xcat1, data = dat)
summary(m4)
plotSlopes(m4, modx = "xcat1", plotx = "x1")
plotSlopes(m4, modx = "xcat1", plotx = "x1", interval = "conf")

dat$xcat2n <- with(dat, contrasts(xcat2)[xcat2, ])
dat$y4 <- with(dat, 3 + 0.5*x1 + xcat2n %*% c(0.1, -0.2, 0.3, 0.05)  + stde * rnorm(100))

m5 <- lm(y4 ~ x1 + xcat2, data = dat)
plotSlopes(m5, plotx = "x1", modx = "xcat2")

m6 <- lm(y4 ~ x1 * xcat2, data = dat)
plotSlopes(m6, plotx = "x1", modx = "xcat2")


## Now examples with real data
library(car)
m3 <- lm(statusquo ~ income * sex, data = Chile)
summary(m3)
plotSlopes(m3, modx = "sex", plotx = "income")


m4 <- lm(statusquo ~ region * income, data= Chile)
summary(m4)
plotSlopes(m4, plotx = "income", modx = "region")

plotSlopes(m4, plotx = "income", modx = "region", plotPoints = FALSE)
plotSlopes(m4, plotx = "income", modx = "region", plotPoints = FALSE, interval = "conf")
plotSlopes(m4, plotx = "income", modx = "region", modxVals = c("SA","S", "C"), plotPoints = FALSE, interval = "conf")
## Same, choosing 3 most frequent values
plotSlopes(m4, plotx = "income", modx = "region", n = 3, plotPoints = FALSE, interval = "conf")


m5 <- lm(statusquo ~ region * income + sex + age, data= Chile)
summary(m5)
plotSlopes(m5, modx = "region", plotx = "income")

m6 <- lm(statusquo ~ income * age + education + sex + age, data = Chile)
summary(m6)
plotSlopes(m6, modx = "income", plotx = "age")

plotSlopes(m6, modx = "income", plotx = "age", plotPoints = FALSE)


## Convert education to numeric, for fun
Chile$educationn <- as.numeric(Chile$education)
m9 <- lm(statusquo ~ income * educationn + sex + age, data = Chile)
summary(m9)
plotSlopes(m9, plotx = "educationn",  modx = "income")

data(Prestige)
m1 <- lm(prestige ~ education * type, data = Prestige)

plotSlopes(m1, plotx = "education", modx = "type", interval = "conf")
dev.new()
plotSlopes(m1, plotx = "education", modx = "type", modxVals = c("prof"), interval = "conf")
dev.new()
plotSlopes(m1, plotx = "education", modx = "type", modxVals = c("bc"), interval = "conf")
dev.new()
plotSlopes(m1, plotx = "education", modx = "type", modxVals = c("bc", "wc"), interval = "conf")
