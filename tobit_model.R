# Following https://stats.oarc.ucla.edu/r/dae/tobit-models/

require(ggplot2)
require(GGally)
require(VGAM)
dat <- read.csv("https://stats.idre.ucla.edu/stat/data/tobit.csv")
str(dat)

# Evaluate at all points in the grid `x` the density of a normal distribution
# with the same mean and sd as the variable `var`. Set to be on the scale as a
# histogram of `var` with binwidth `bw`
calc_dens <- function(x, var, bw = 15) {
  bw*length(var)*dnorm(x, mean = mean(var), sd = sd(var))
}

p <- ggplot(dat, aes(x = apt, fill=prog))

p + geom_histogram(binwidth = 15) +
  geom_function(fun = calc_dens, args = list(var = dat$apt))


# Highlight the censoring
p + geom_histogram(binwidth = 1) +
  geom_function(fun = calc_dens, args = list(var = dat$apt, bw = 1))

library(dplyr)
dat %>%
  mutate(top_score = apt == 800) %>%
  count(prog, top_score)


# Tobit model
summary(m <- vglm(apt ~ read + math + prog, tobit(Upper = 800), data = dat))
#First intercept is actual intercept
#Second intercept gives us information about the root mean squared error as we'll see below

# Confidence intervals
b <- coef(m)
se <- sqrt(diag(vcov(m)))
cbind(LL = b - qnorm(0.975) * se, UL = b + qnorm(0.975) * se)

# Root mean squared error
exp(coef(m)["(Intercept):2"])
# compare to
sd(dat$apt)
# pretty good reduction

# Plot predictions. Note that the predictions are on the "uncensored scale" so exceed 800
dat$yhat <- fitted(m)[,1]
ggplot(dat, aes(yhat, apt)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  geom_hline(yintercept = 800, color = "red") +
  labs(y = "True score", "Predicted score")










