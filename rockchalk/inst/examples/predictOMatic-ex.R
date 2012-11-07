library(rockchalk)

set.seed(12345)
N <- 100
x1 <- rpois(N, l=6)
x2 <- rnorm(N, m=50, s=10)
x3 <- rnorm(N)
xcat1 <- gl(2,50, labels=c("M","F"))
xcat2 <- cut(rnorm(N), breaks=c(-Inf, 0, 0.4, 0.9, 1, Inf), labels=c("R", "M", "D", "P", "G"))
dat <- data.frame(x1, x2, x3, xcat1, xcat2)
rm(x1, x2, x3, xcat1, xcat2)
xcat1n <- with(dat, contrasts(xcat1)[xcat1, ,drop=FALSE])
xcat2n <- with(dat, contrasts(xcat2)[xcat2, ])
STDE <- 15
dat$y <- 0.03 + 0.8*dat$x1 + 0.1*dat$x2 + 0.7*dat$x3 + xcat1n %*% c(2) + xcat2n %*% c(0.1,-2,0.3, 0.1) + STDE*rnorm(N)
rownames(dat$y) <- NULL
## rownames of dat$y don't match rownames of dat. Humphf.
## I've not seen that problem before

dat$x1[sample(N, 5)] <- NA
dat$x2[sample(N, 5)] <- NA
dat$x3[sample(N, 5)] <- NA
dat$xcat2[sample(N, 5)] <- NA
dat$xcat1[sample(N, 5)] <- NA
dat$y[sample(N, 5)] <- NA


m0 <- lm(y ~ x1 + x2 + xcat1, data = dat)
summary(m0)
m0.data <- model.data(m0)
summarize(m0.data)

(m0.p1 <- predictOMatic(m0))
(m0.p2 <- predictOMatic(m0, interval = "confidence"))

(m0.p3 <- predictOMatic(m0, divider="std.dev.", n=5))

(m0.p3 <- predictOMatic(m0, fl = list("x1" = c(6,7), "xcat1" = levels(m0.data$xcat1))))






m1 <- lm(y ~ log(10+x1) + sin(x2) + x3, data=dat)
m1.data <- model.data(m1)
summarize(m1.data)

(newdata(m1))
(newdata(m1, fl = list(x1=c(6, 8, 10))))
(newdata(m1, fl = list(x1 = c(6, 8, 10), x3 = c(-1,0,1))))
(newdata(m1, fl = list(x1 = c(6, 8, 10), x2 = quantile(m1.data$x2), x3 = c(-1,0,1))))

(m1.p1 <- predictOMatic(m1, divider="std.dev", n = 5))
(m1.p1 <- predictOMatic(m1, divider="quantile", n = 5))

(m1.p1 <- predictOMatic(m1, fl=list(x1=c(6, 8, 10), x2 = median(m1.data$x2, na.rm = TRUE))))
(m1.p1 <- predictOMatic(m1, fl=list(x1=c(6, 8, 10), x2 = quantile(m1.data$x2, na.rm = TRUE))))

(m1.p1 <- predictOMatic(m1))
(m1.p1 <- predictOMatic(m1, divider="std.dev."))
(m1.p1 <- predictOMatic(m1, divider="std.dev.", n=5))
(m1.p1 <- predictOMatic(m1, divider="std.dev.", interval="confidence"))




m2 <- lm(y ~ x1 + x2 + x3 + xcat1 + xcat2, data = dat)


##  has only columns and rows used in model fit
m2.data <- model.data(m2)
summarize(m2.data)

newdata(m2, fl = list(xcat1 = levels(m2.data$xcat1)))
## mix and match all combinations of xcat1 and xcat2
newdata(m2, fl = list(xcat1 = levels(m2.data$xcat1), xcat2 = levels(m2.data$xcat2)))

m2.new1 <- newdata(m2, fl = list(xcat1 = levels(m2.data$xcat1), xcat2 = levels(m2.data$xcat2)))

predict(m2, newdata = m2.new1)

## Pick some particular values for focus
m2.new2 <- newdata(m2, fl = list(x1 = c(1,2,3), xcat2 = c("M","D")))
## Ask for predictions
predict(m2, newdata = m2.new2)




## Compare: predictOMatic generates a newdata frame and predictions in one step

(m2.p1 <- predictOMatic(m2, fl = list(xcat1 = levels(m2.data$xcat1), xcat2 = levels(m2.data$xcat2))))


(m2.p2 <- predictOMatic(m2, fl = list(x1 = c(1,2,3), xcat2 = c("M","D"))))

(m2.p3 <- predictOMatic(m2, fl = list(x2 = c(0.25, 1.0), xcat2 = c("M","D"))))

(m2.p4 <- predictOMatic(m2, fl = list(x2 = plotSeq(m2.data$x2, 10) , xcat2 = c("M","D"))))

(m2.p5 <- predictOMatic(m2, fl = list(x2 = c(0.25, 1.0), xcat2 = c("M","D")), interval="conf"))

(predictOMatic(m2, interval="conf"))

(m2.p6 <- predictOMatic(m2, fl = list(x2 = c(49, 51), xcat2 = levels(m2.data$xcat2), x1=plotSeq(dat$x1))))

plot(y ~ x1, data= m2.data)
by(m2.p6, list(m2.p6$xcat2), function(x) {lines(x$x1, x$fit, col=x$xcat2, lty=as.numeric(x$xcat2))})

m2.newdata <- newdata(m2, fl = list(x2 = c(48, 50, 52), xcat2 = c("M","D")))
predict(m2, newdata = m2.newdata)

(m2.p7 <- predictOMatic(m2, fl = list(x2 = c(48, 50, 52), xcat2 = c("M","D"))))

(m2.p8 <- predictOMatic(m2, fl = list(x2 = range(m2.data$x2), xcat2 = c("M","D"))))

(m2.p9 <- predictOMatic(m2, fl = list(x2 = plotSeq(m2.data$x2), x1 = quantile(m2.data$x1, pr =c(0.33, 0.66), na.rm = TRUE), xcat2 = c("M","D"))))
plot(y ~ x2 , data = m2.data)
by(m2.p9, list(m2.p9$x1, m2.p9$xcat2), function(x) {lines(x$x2, x$fit)})



(predictOMatic(m2, fl = list(x2 = c(50, 60), xcat2 = c("M","D")), interval="conf"))


## create a dichotomous dependent variable
y2 <- ifelse(rnorm(N) > 0.3, 1, 0)
dat <- cbind(dat, y2)

m3 <- glm(y2 ~ x1 + x2 + x3 + xcat1, data=dat, family=binomial(logit))
summary(m3)
m3.data <- model.data(m3)
summarize(m3.data)

(m3.p1 <- predictOMatic(m3, divider="std.dev.", type="response"))

(m3.p2 <- predictOMatic(m3, fl = list(x2 = c(40, 50, 60), xcat1 = c("M","F")), divider="response", interval="conf"))

## Want a full accounting for each value of x2?
(m3.p3 <- predictOMatic(m3, fl = list(x2 = unique(m3.data$x2), xcat1 = c("M","F")), interval="conf", type="response"))


## Would like to write a more beautiful print method
## for output object, but don't want to obscure structure from user.
for (i in names(m3.p1)){
    dns <- cbind(m3.p1[[i]][i], m3.p1[[i]]$fit)
    colnames(dns) <- c(i, "predicted")
    print(dns)
}


