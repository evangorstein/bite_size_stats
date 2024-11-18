# Bayesian analysis of difference in group means
# https://mvuorre.github.io/posts/2017-01-02-how-to-compare-two-groups-with-robust-bayesian-estimation-using-r-stan-and-brms/

library(knitr)
library(kableExtra)
library(scales)
library(broom)
library(brms)
library(tidyverse)
library(tidybayes)


#  Data -------------------------------------------------------------------
y1 = c(101,100,102,104,102,97,105,105,98,101,100,123,105,103,100,95,102,106,
       109,102,82,102,100,102,102,101,102,102,103,103,97,97,103,101,97,104,
       96,103,124,101,101,100,101,101,104,100,101)
y2 = c(99,101,100,101,102,100,97,101,104,101,102,102,100,105,88,101,100,
       104,100,100,100,101,102,103,97,101,101,100,101,99,101,100,100,
       101,100,99,101,100,102,99,100,99)
n1 = length(y1)
n2 = length(y2)
dat <- data.frame(IQ = c(y1, y2),
                  group = factor(rep( c("Treatment", "Control"), c(n1, n2) ) ) )

sum_dat <- group_by(dat, group) %>%
  summarise(mean = mean(IQ), sd = sd(IQ))


ggplot(dat, aes(x = IQ)) +
  geom_histogram(color = "black", binwidth = 1) +
  facet_wrap(~ group) +
  geom_label(
    data = sum_dat,
    aes(label = paste0("mean =  ",
                       round(mean, 1),
                       "\nSD = ",
                       round(sd, 1))),
    x = 90, y = 10,
  ) +
  theme_minimal()


# Modeling ----------------------------------------------------------------

## Equal variance -----
mod_eqvar <- brm(
  IQ ~ group,
  data = dat,
  cores = 4
)
summary(mod_eqvar)

## Unequal variance -----
uneq_var_frm <- bf(IQ ~ group, sigma ~ group)
mod_uneqvar <- brm(
  uneq_var_frm,
  data = dat,
  cores = 4
)
summary(mod_uneqvar)
as_draws_df(mod_uneqvar) %>%
  ggplot(aes(b_groupTreatment)) +
  stat_halfeye()


## Assuming data follows t-distribution for robustness to outliers -----
mod_robust <- brm(
  bf(IQ ~ group, sigma ~ group),
  family = student,
  data = dat,
  cores = 4
)
summary(mod_robust)
as_draws_df(mod_robust) %>%
  ggplot(aes(b_groupTreatment)) +
  stat_halfeye()

mod_uneqvar <- add_criterion(mod_uneqvar, "loo")
mod_robust <- add_criterion(mod_robust, "loo")
loo_compare(mod_uneqvar, mod_robust, criterion = "loo")


