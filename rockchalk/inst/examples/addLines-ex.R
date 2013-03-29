library(rockchalk)


set.seed(12345)
x1 <- rnorm(100)
x2 <- rnorm(100)
x3 <- rnorm(100)
x4 <- rnorm(100)
xcat1 <- gl(2,50, labels=c("M","F"))
xcat2 <- cut(rnorm(100), breaks=c(-Inf, 0, 0.4, 0.9, 1, Inf), labels=c("R", "M", "D", "P", "G"))
y <- 0.03 + 0.1*x1 + 0.1*x2 + 0.4*x3 - 0.1*x4 + 2*rnorm(100)
y2 <- 0.03 + 0.1*x1 + 0.1*x2 + 0.25*x1*x2 + 0.4*x3 -0.1*x4 + 0.2 * as.numeric(xcat1) + contrasts(xcat2)[as.numeric(xcat2), ] %*% c(-0.1, 0.1, 0.2, 0) + 1*rnorm(100)
dat <- data.frame(x1, x2, x3, x4, xcat1, xcat2, y, y2)
rm(x1, x2, x3, x4, y, y2, xcat1, xcat2)

## linear ordinary regression
m1 <- lm(y ~ x1 + x2 + x3 + x4, data = dat)

## These will be parallel lines

plotSlopes(m1, plotx = "x1", modx = "x2", modxVals = "std.dev.", n = 5, main = "A plotSlopes result with \"std.dev.\" values of modx")


m1ps <- plotSlopes(m1, plotx = "x1", modx = "x2", modxVals = c(-2,0,2))
m1pp <- plotPlane(m1, plotx1 = "x1", plotx2 = "x2", ticktype = "detailed", npp = 10)

addLines(from = m1ps, to = m1pp, lty = 1, lwd = 2)
addLines(from = m1ps, to = m1pp, lty = 2, lwd = 5, col = "green")
