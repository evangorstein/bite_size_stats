# How does performance of lme4 estimation change with increase in covariate dimension?
library(lme4)
set.seed(100)

# parameters
g = 50
n = 5
p = 245 # Note than total sample size is n*g > p, but only slightly
pop_intercept = 4
sd_intercepts = 2
pop_slope = 1
sd_slopes = 1
sd_noise = .25

# random effects
intercepts <- rnorm(g, mean = pop_intercept, sd = sd_intercepts)
slopes <- rnorm(g, mean = pop_slope, sd = sd_slopes)

# Create data
X <- matrix(rnorm(g*n*p), ncol = p)
grps <- rep(1:g, each = n)
noises <- rnorm(g*n, sd = sd_noise)
y <- vector("numeric", g*n)
for (i in seq_along(y)) {

  grp <- grps[i]
  # only first column of X has any effect
  y[i] <- intercepts[grp] + slopes[grp]*X[i,1] + noises[i]

}
df <- data.frame(grps, X, y)

# fit low dimensional model
lowd_mod <- lmer(y ~ X1 + (X1 | grps), data = df)
summary(lowd_mod)
# estimated fixed effect for X1
fixef(lowd_mod, )["X1"]
# with standard error
sqrt(diag(vcov(lowd_mod)))[2]
# estimated random effects covariance matrix
VarCorr(lowd_mod)

# lowish dimensional model
lowishd_mod <- lmer(y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + grps + (X1 | grps), data = df)
summary(lowishd_mod)
fixef(lowishd_mod)["X1"]
sqrt(diag(vcov(lowishd_mod)))[2]
VarCorr(lowishd_mod)

# high dimensional model
high_dim_form <- as.formula(paste0("y~", paste0("X", 1:150, collapse = "+"), "+ (X1 | grps)"))
highd_mod <- lmer(high_dim_form, data = df)
summary(highd_mod)
fixef(highd_mod)["X1"]
sqrt(diag(vcov(highd_mod)))[2]
VarCorr(highd_mod)

# what about maximum likelihood estimates?
highd_mod_ml <- lmer(high_dim_form, data = df, REML = F)
summary(highd_mod_ml)
fixef(highd_mod_ml)["X1"]
sqrt(diag(vcov(highd_mod_ml)))[2]
VarCorr(highd_mod_ml)

# highest dimensional model
highestd_mod <- lmer(y ~ . - grps + (X1 | grps), data = df)
summary(highestd_mod)
fixef(highestd_mod)["X1"]
sqrt(diag(vcov(highestd_mod)))[2]
VarCorr(highestd_mod)

# what about maximum likelihood estimates?
highestd_mod_ml <- lmer(y ~ . - grps + (X1 | grps), data = df, REML = F)
summary(highestd_mod_ml)
fixef(highestd_mod_ml)["X1"]
sqrt(diag(vcov(highestd_mod_ml)))[2]
VarCorr(highestd_mod_ml)





