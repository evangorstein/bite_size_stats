library(tidyverse)
library(tidybayes)
library(generative)
review_theme()


N <- 60
designs <- list(
  "basic" = basic_design(N),
  "blocked" = blocked_design(N),
  "compromise" = compromise_design(N)
)

designs
effects <- design_effects(N, 3, 25)

# blocked design
x_blocked <- simulate(designs$blocked, effects)
x_df <- bind_cols(designs$blocked, x_blocked) %>%
  pivot_longer(-c("id", "treatment", "subject"), names_to = "feature")
plot_treatments(x_df, 26) +
  geom_line(aes(group = subject), alpha = 0.4, size = 0.2) 


# basic design
x_basic <- simulate(designs$basic, effects)
x_df <- bind_cols(designs$basic, x_basic) %>%
  pivot_longer(-c("id", "treatment", "subject"), names_to = "feature")
plot_treatments(x_df)


# estimate posterior for beta
fit <- multinomial_logistic(x_blocked, designs$blocked)
treatment_estimates <- list()
treatment_estimates[["blocked"]] <- process_estimate(fit, "treatment")
truth <- process_truth(effects$treatment)

comparison <- treatment_estimates$blocked %>%
  group_by(feature, treatment) %>%
  summarise(posterior_mean = mean(value)) %>%
  left_join(truth)

ggplot(comparison) +
  geom_abline(slope = 1) +
  geom_point(aes(value, posterior_mean))

fit <- multinomial_logistic(x_basic, designs$basic)
treatment_estimates[["basic"]] <- process_estimate(fit, "treatment")

p <- bind_rows(treatment_estimates, .id = "design") %>%
  mutate(design = fct_recode(design, unblocked = "basic")) %>%
  ggplot(aes(feature, value, col = as.factor(treatment))) +
  stat_dots() +
  scale_color_brewer(palette = "Set2") +
  geom_point(data = truth, size = 1.6, col = "#0c0c0c") +
  geom_point(data = truth, size = 0.8) +
  labs(x = "Taxon", y = "Effect", col = "Treatment") +
  facet_grid(design ~ .)

ggsave("~/Desktop/my_plot.pdf", p)

p

bind_rows(treatment_estimates, .id = "design") %>%
  group_by(design, feature, treatment) %>%
  summarise(sd = sd(value)) %>%
  group_by(design) %>%
  summarise(mean(sd))




