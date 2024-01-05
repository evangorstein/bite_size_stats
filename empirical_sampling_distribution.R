library(rethinking)
set.seed(100)

n=20
beta = 1
x <- rnorm(n)
y <- rnorm(n, beta*x)
mod <- lm(y~x)
se_marg <- coef(summary(mod))[2,2]


sim_regress <- function(n, beta) {
  
  x <- rnorm(n)
  y <- rnorm(n, beta*x)
  mod <- lm(y~x)
  se_marg <- coef(mod)["x"]
  
}

ests <- mcreplicate(1e5, sim_regress(n, beta), mc.cores=8)

dens(ests, lwd = 3)
curve(dt((x-beta)/se_marg, n-2)/se_marg, add = T, col = 4, lwd=3)





