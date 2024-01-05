library(tidyverse)

#Generate two way factorial data
A = gl(2, 5, 200, labels = c("Burbank", "New"))
N = factor(rep(1:5, 40)) 
yield = rnorm(200, 11:20, 1)

df = tibble(A, N, yield, true_mn = rep(11:20, 20))
glimpse(df)


#Same full model with different parameterizations
mod1 <- lm(yield ~ A + N + A:N, data = df)
mod2 <- lm(yield ~ A + A:N, data = df)
mod3 <- lm(yield ~ A:N, data = df)
summary(mod1)
summary(mod2)
summary(mod3)
anova(mod1)
anova(mod2)
anova(mod3)
#Can see the parameterizations by looking at design matrices
model.matrix(mod1)
model.matrix(mod2)
model.matrix(mod3)


#True, additive model
mod_true <- lm(yield ~ A + N, data = df)
summary(mod_true)
anova(mod_true)

