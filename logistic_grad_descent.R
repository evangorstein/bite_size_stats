# This is logistic regression gradient descent for 3 two dimensional data points
# Points are at:
# (-1, -1): Negative label
# (-1, 1): Negative label
# (1, 0): Positive label
# Models is fit without an intercept

##TODO: Add another point that either
#- keeps things linearly separable but makes the solution less symmetric
#- makes things no longer linearly separable
library(ggplot2)

grad_log <- function(w) {
  w1 <- w[1]
  w2 <- w[2]
  term1 <- -exp(-w1) / (1 + exp(-w1))
  term2 <- exp(w2 - w1)/(1 + exp(w2 - w1))
  term3 <- exp(-w2 - w1)/(1 + exp(-w2 - w1))
  lw1 <- term1 - term2 - term3
  lw2 <- term2 - term3
  return( c(lw1, lw2) )
}

grad_hinge <- function(w) {
  w1 <- w[1]
  w2 <- w[2]
  term1 <- -1*(w1 < 1)
  term2 <- 1*(1 + w2 - w1 > 0)
  term3 <- 1*(1 - w2 - w1 > 0)
  lw1 <- term1 - term2 - term3
  lw2 <- term2 - term3
  return( c(lw1, lw2) )
}


grad_desc <- function(start, grad, max_steps = 10000,
                      ss = .1, tol = 0.0001) {
  w = start
  step = 1
  storage = matrix(NA, nrow = max_steps, ncol = length(w))
  storage[step,] = w
  while(step < max_steps) {
    step = step + 1
    prev = w
    gr = grad(w)
    w = w - ss*gr
    storage[step,] = w
    if ( all( abs(w - prev) < tol) ) break
  }
  list(
    "solution" = w,
    "steps" = storage,
    "num_steps" = step
    )
}

run_and_plot <- function(start = c(-10, -20), grad, ss = 1) {
  results <- grad_desc(start, grad, ss = ss)
  colnames(results$steps) = c("w1", "w2")
  steps_df <- data.frame(na.omit(results$steps))
  ggplot(steps_df) +
    geom_segment(aes(x = 0,
                     y = 0,
                     xend = w1,
                     yend = w2,
                     col = 1:nrow(steps_df)),
                 arrow = arrow()) +
    scale_color_continuous(name = paste0("1:", nrow(steps_df)))+
    labs(x = "w1", y = "w2")
}

run_and_plot(grad = grad_log, ss = .1)
run_and_plot(grad = grad_hinge, ss = .1)


# least squares solution
X = rbind( c(-1, -1),
           c(-1, 1),
           c(1, 0)
           )
y = c(-1, -1, 1)
coef( lm(y ~ 0+X )  )







