#########################
## Discriminating coins
## (Problem 2 in Section 16.5 from MFML by Rob Nowak)
#########################

## Suppose we have m coins, m-1 of which are fair but one of which has probability of 0.5 + epsilon of landing heads.
## We perform an experiment where we flip each coin n times and we hope that the unfair coin wins in the sense
## that it lands heads most frequently. If it ties for the win, that's also OK.
## We can get an upper bound of (m-1)exp(-n*epsilon^2) on the probability that this desired outcome does not occur
## from Hoeffding's inequality. What's the actual probability?


run_experiment <- function(m = 2, n = 100, eps = 0.1) {
  unfair_count <- rbinom(1, n, 0.5 + eps)
  fair_counts <- rbinom(m-1, n, 0.5)
  lose <- as.integer(any(fair_counts > unfair_count))
  return( c("lose"=lose,
            "unfair_count"=unfair_count,
            "fair_count"=fair_counts ) )
}

grid_eps = 0.1
grid_m = c(2, 100)
grid_n = c(10, 100, 500, 1000)
N = 10000
ary_true_prob = ary_prob_bound = array(dim = c(length(grid_eps),
                                               length(grid_m),
                                               length(grid_n)),
                                       dimnames = list(grid_eps, grid_m, grid_n))
for (eps in grid_eps) {
  for (m in grid_m) {
    for (n in grid_n) {
      ary_prob_bound[as.character(eps), as.character(m), as.character(n)] = (m-1) * exp(-n*eps^2)
      results <- replicate(N, run_experiment(m, n, eps))
      prob_loss = mean(results["lose",])
      ary_true_prob[as.character(eps), as.character(m), as.character(n)] = prob_loss
    }
  }
}

cat("True probabilities with eps = ", grid_eps[1], ":\n", sep = "")
print(ary_true_prob[1,,], digits = 3)
cat("Bounds from Hoeffding with eps = ", grid_eps[1], ":\n", sep = "")
print(ary_prob_bound[1,,], digits = 3)
