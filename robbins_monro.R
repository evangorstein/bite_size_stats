
#Code from stack exchange 
f <- function(x) {
  1/(1 + exp(-x/2)) - 1/2 + rnorm(length(x), sd=0.1)
}
#
# Specify the study parameters.
#
set.seed(17)
iterations <- seq_len(1e4)    # Specify the number of iterations
burn.period <- 1e2            # Skip these initial results when scaling the plots
replicates <- 20              # Specify how many starting values to use
bs <- c(0.25, 0.6, 1, 2)      # Specify which `b` values to explore
as <- sort(rt(replicates, 2)) # Draw the starting values randomly
#
# Perform the study.
#
par(mfrow=c(2,2)) 
colors <- hsv(seq_along(as)/length(as), .8, .9, .5)
for (b in bs) {
  #
  # Find the solutions for each starting value `a`.
  #
  lambda <- iterations^(-b)
  solutions <- sapply(as, function(a) {
    Reduce(function(y, lambda) y - lambda * f(y), lambda, a, accumulate=TRUE)
  })
  #
  # Prepare to plot all the trajectories.
  #
  r <- range(solutions[-seq_len(burn.period), ])
  plot(range(iterations), r,
       log="x", type="n", xlab="Iteration", ylab=expression(theta), las=1,
       main=paste("Solution walks for b =", b))
  abline(h=0, lwd=2)
  #
  # Plot the trajectories.
  #
  sapply(seq_len(ncol(solutions)), function(j) {
    theta <- solutions[-1, j]
    lines(iterations, theta, cex=3/4, col=colors[j])
  })
}
par(mfrow=c(1,1))



# Study the algorithm for finding square root of 2 without any noise in the function
library(tidyverse)
# f is the function whose root we are trying to find
# a is the starting value
# p is the exponent dictating the decay rate of the step size (p = 0 means constant step size)
# iters is the number of iterations
solution <- function( f, a, p, iters) {
  
  step_size <- (2:iters)^p
  solution = Reduce(function(y, lambda) y - lambda * f(y), step_size, a, accumulate=TRUE)
  
} 

f <- function(x) {
  x^2-2 + rnorm(length(x), sd=0)
}
a=2
ps = seq(-3, 0, by=.2)
set.seed(100)
solutions = sapply(X = ps, FUN = solution, f=f, a=a, iters=1000)
colnames(solutions) = ps
df_solutions <- as_tibble(solutions) %>%
  mutate(iterate = row_number()) %>%
  pivot_longer(cols = -c(iterate), names_to = "p", names_transform = as.numeric) %>%
  arrange(p) 

df_solutions %>%
  filter(iterate < 100) %>%
  ggplot(aes(x = iterate, y=value)) +
  geom_line() +
  facet_wrap(~p,labeller = "label_both") +
  geom_hline(yintercept = sqrt(2), col = 'blue', linetype = "dashed")
  





