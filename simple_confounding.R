#Nice demonstration of confounding from Demetri Pananos at https://dpananos.github.io/posts/2024-06-23-confound/:
# "Suppose a new drug is introduced to prevent death.
# In truth, the drug decreases the risk of death in both men and women by 10 percentage points,
# but men are more likely to take the drug and men are more likely to die.
# Hence, the effect of the drug is confounded by sex."
library(tidyverse)
library(marginaleffects)
sim_data <- function(x=0, n=1e6){
  withr::with_seed(x, {
    s <- rbinom(n, 1, 0.5)
    d <- rbinom(n, 1, 0.6 + 0.3*s)
    y <- rbinom(n, 1, 0.4 - 0.1*d + 0.4*s)
  })
  data.frame(s, d, y)
}
dat = sim_data()

# Correct analysis
model <- glm(y~d*s, data=dat, family = binomial())
# g formula
dat %>%
  mutate(id = row_number()) %>%
  select(-d) %>%
  expand_grid(d = 0:1) %>%
  modelr::add_predictions(model, type = 'response') %>%
  group_by(d) %>%
  summarise(pred = mean(pred)) %>%
  pull(pred) %>%
  diff

# Alternatively, the following works because sex is balanced.
# If sex weren't balanced, it would not give PATE, but rather the ATE in a hypothetical population with balanced sex
grid <- datagrid(model = model, d=0:1, s=0:1)
grid %>%
  avg_comparisons(model, variables = 'd', newdata = .)

# Incorrect analysis that just looks at difference in group means
model <- glm(y~d, data=dat, family = binomial())
grid <- datagrid(model = model, d=0:1)
grid %>%
  avg_comparisons(model, variables = 'd', newdata = .)
