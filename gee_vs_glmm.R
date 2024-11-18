library(lme4)

set.seed(420)
n_sub = 500
obs_per_sub = 10
pop_param = 1
sub_sd = 2
sub_logits = rnorm(n_sub, pop_param, sub_sd)
sub_probs = exp(sub_logits) / ( exp(sub_logits) + 1 )
data <- rbinom(n_sub, obs_per_sub, sub_probs)

# hist(sub_logits)
# hist(sub_probs)
# hist(data)

# True average prob (Monte Carlo estimate, but should be pretty accurate)
many_logits = rnorm(100000, pop_param, sub_sd)
many_probs = exp(sub_logits) / ( exp(sub_logits) + 1 )
pop_prob = mean(many_probs)

# If we use a gee, we estimate the average across subjects of the probabilities sub_probs
gee_est_prob = mean(data)/obs_per_sub
gee_est_logit = log(gee_ave_prob / (1 - gee_ave_prob))

# If we use a glmm, we estimate the average across subjects of the logits sub_logits
# Aka, we estimate pop_param
df <- data.frame(resp = factor(c( sapply(data,
                                  \(x) c(rep(1, x), rep(0, obs_per_sub - x) )  )
                           )),
                 sub_id = factor(rep(1:n_sub, each = obs_per_sub))
                 )
glmm_mod <- glmer(resp ~ (1 | sub_id), df, family = binomial)
glmm_est_logit = fixef(glmm_mod)
glmm_est_prob <- exp(glmm_ave_logit) / (exp(glmm_ave_logit) + 1)

# Let's see
cat("True average prob: ", pop_prob)
cat("True average logit: ", pop_param)
cat("GEE estimated prob: ", gee_est_prob)
cat("GEE estimated logit: ", gee_est_logit)
cat("GLMM estimated prob: ", glmm_est_prob)
cat("GLMM estimated logit: ", glmm_est_logit)
# The GEE estimates should be converging towards the population prob, but not the population logit
# The GLMM estimates should be converging towards the population logit, not population prob

