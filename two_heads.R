# Simulation study for the time it takes to reach two heads in a row when flipping a biased coin

draw <- function(p) {

  # initiate coin flips with first two flips
  sides <- c("H", "T")
  flips <- sample(sides, 2, replace = TRUE, prob = c(p, 1-p))
  l = 2

  while (T) {
    if (flips[l-1] == "H" && flips[l] == "H") return(l)
    flip <- sample(sides, 1, prob = c(p, 1-p))
    flips <- c(flips, flip)
    l = l + 1
  }

}

ps = seq(0.05, .95, .05)
R = 1000
grid_draws = matrix(nrow = length(ps), ncol = R)

for (i in seq_along(ps)) {
  grid_draws[i,] <- replicate(R, draw(p[i]))
}

# How expected number of flips before two Heads depends on the probability of Heads
means <- rowMeans(grid_draws)
plot(ps, means, log = "y")


