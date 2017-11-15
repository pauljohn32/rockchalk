library(rockchalk)


set.seed(23452345)
N <- 100
x1 <- gl(12, 2, labels = LETTERS[1:12])
x2 <- gl(8, 3, labels = LETTERS[12:24])
x1 <- sample(x = x1, size=N, replace = TRUE)
x2 <- sample(x = x2, size=N, replace = TRUE)
z1 <- rnorm(N)
a1 <- rnorm(N, mean = 1.2, sd = 11.7)
a2 <- rpois(N, lambda = 10 + a1)
a3 <- rgamma(N, 0.5, 4)
b1 <- rnorm(N, mean = 211.3, sd = 0.4)
dat <- data.frame(z1, a1, x2, a2, x1, a3, b1)
summary(dat)

summarize(dat)

summarize(dat, digits = 2)

summarize(dat, 
          probs = c(0, 0.20, 0.50),
          stats = c("mean", "entropy"))

## Only quantile values, no summary stats for numeric variables
## Discrete variables get entropy
summarize(dat, 
          probs = c(0, 0.25, 0.50, 0.75, 1.0),
          stats = "entropy", digits = 2)

## Quantiles and the mean for numeric variables.
## No diversity stats for discrete variables (entropy omitted)
summarize(dat, 
          probs = c(0, 0.25, 0.50, 0.75, 1.0),
          stats = "mean")


## Returns un rounded data frame, with
## colnames on rows, values on columns
summarizeNumerics(dat)

summarizeFactors(dat, maxLevels = 5)

## See actual values of factor summaries, without
## beautified printing
unclass(summarizeFactors(dat, maxLevels = 5))

summarize(dat, alphaSort = TRUE) 

summarize(dat, digits = 6, alphaSort = FALSE)


summarize(dat, maxLevels = 2)

datsumm <- summarize(dat, stats = c("mean", "sd", "var"), props = TRUE)

## Unbeautified numeric data frame, variables on the rows
datsumm[["numerics"]]
## Beautified versions 1. shows saved version:
datsumm[["numericsfmt"]]
## 2. Run formatNumericSummaries to re-specify digits:
formatNumericSummaries(datsumm[["numerics"]], digits = 10)

datsumm[["factors"]]

datsummNT <- datsumm[["numerics"]]

plot(datsummNT$mean, datsummNT$var, xlab = "The Means",
    ylab = "The Variances")

plot(datsummNT$mean, datsummNT$var, xlab = "The Means",
    ylab = "The Variances", type = "n")
text(datsummNT$mean, jitter(datsummNT$var), labels = rownames(datsummNT))
## problem with name overlap.
