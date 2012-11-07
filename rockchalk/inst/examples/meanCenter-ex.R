
library(rockchalk)
N <- 100
dat <- genCorrelatedData(N=N, means=c(100,200), sds=c(20,30), rho=0.4, stde=10)
dat$x3 <- rnorm(100, m=40, s=4)

m1 <- lm(y ~ x1 * x2 + x3, data=dat)
summary(m1)
mcDiagnose(m1)

m1c <- meanCenter(m1)
summary(m1c)
mcDiagnose(m1c)

m2 <- lm(y ~ x1 * x2 + x3, data=dat)
summary(m2)
mcDiagnose(m2)

m2c <- meanCenter(m2, standardize = TRUE)
summary(m2c)
mcDiagnose(m2c)

m2c2 <- meanCenter(m2, centerOnlyInteractors = FALSE)
summary(m2c2)

m2c3 <- meanCenter(m2, centerOnlyInteractors = FALSE, centerDV = TRUE)
summary(m2c3)

dat <- genCorrelatedData(N=N, means=c(100,200), sds=c(20,30), rho=0.4, stde=10)
dat$x3 <- rnorm(100, m=40, s=4)
dat$x3 <- gl(4, 25, labels=c("none","some","much","total"))

m3 <- lm(y ~ x1 * x2 + x3, data=dat)
summary(m3)
## visualize, for fun
plotPlane(m3, "x1", "x2")

m3c1 <- meanCenter(m3)
summary(m3c1)

## Not exactly the same as a "standardized" regression because the
## interactive variables are centered in the model frame,
## and the term "x1:x2" is never centered again.
m3c2 <- meanCenter(m3, centerDV=TRUE, centerOnlyInteractors=FALSE, standardize=TRUE)
summary(m3c2)

m3st <- standardize(m3)
summary(m3st)
