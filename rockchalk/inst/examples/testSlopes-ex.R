library(car)
m6 <- lm(statusquo ~ income * age + education + sex + age, data = Chile)
m6ps <- plotSlopes(m6, modx = "income", plotx = "age")
m6psts <- testSlopes(m6ps)
## Finally, an interesting one to plot
plot(m6psts)


dat2a <- genCorrelatedData(N=400, rho=.1, stde=300, beta=c(2, 0, 0.1, 0.4))
m2a <- lm(y ~ x1*x2, data=dat2a)
m2aps <- plotSlopes(m2a, plotx="x1", modx="x2")
m2apsts <- testSlopes(m2aps)
plot(m2apsts)

dat2b <- genCorrelatedData(N=400, rho=.1, stde=300, beta=c(2, 0, 0.3, 0.15), means = c(50,0), sds = c(10, 40))
m2b <- lm(y ~ x1*x2, data=dat2b)
m2bps <- plotSlopes(m2b, plotx="x1", modx="x2")
m2bsts <- testSlopes(m2bps)
plot(testSlopes(m2bps))
plot(testSlopes(m2bps), shade = FALSE)

## Finally, if model has no relevant interactions, testSlopes does nothing.
m9 <- lm(statusquo ~ age + income * education + sex + age, data=Chile)
m9ps <- plotSlopes(m9, modx = "education", plotx = "age", plotPoints = FALSE)
m9psts <- testSlopes(m9ps)
