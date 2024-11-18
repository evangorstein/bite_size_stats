gen_data_and_est <- function(n, rate, search_int = c(0, 10)) {

  draws <- rexp(n, rate)
  y <- max(draws)
  f <- function(theta) {
    (n-1)*y*exp(-theta*y)/(1-exp(-theta*y)) + 1/theta - y
  }
  f2 <- function(theta) {
    y*(1-n*exp(-theta*y))/(1-exp(-theta*y)) - (1/theta)
  }
  f3 = function(theta) {
    (1-exp(-theta*y))/(y*(1-n*exp(-theta*y))) - theta
  }
  root_results <- uniroot(f, search_int)
  root_results2 <- uniroot(f2, search_int)
  exact_sol <- root_results$root
  exact_sol2 <- root_results2$root
  # f3 has the same root as f and f2, but it also has a vertical asymptote very close to this root
  # Specifically, it has the vertical asymptote at log(n)/y, which is the approximate solution!
  # For this reason, if we want to recover the solutions using f3, we need to restrict the search interval to avoid including the vertical asymptote
  root_results3 <- uniroot(f3, c(exact_sol - 1/n, exact_sol + 1/n))
  exact_sol3 <- root_results3$root
  approx_sol <- log(n)/y
  c("exact"=exact_sol,
    "exact2"=exact_sol2,
    "exact3" = exact_sol3,
    "approx"=approx_sol)
}
set.seed(11)
B = 10000
n = 100
rate = 5
results <- replicate(B, gen_data_and_est(n, rate))
hist(results["exact",])
abline(v = rate, lwd = 2, col = "green")
abline(v = mean(results["exact",]), lwd = 2, col = "orange")
abline(v = mean(results["exact",]) + 2*sd(results["exact",]), lwd = 2, col = "red")
abline(v = mean(results["exact",]) - 2*sd(results["exact",]), lwd=2, col="red")
hist(results["exact2",])
abline(v = rate, lwd = 2, col = "green")
abline(v = mean(results["exact2",]), lwd = 2, col = "orange")
abline(v = mean(results["exact2",]) + 2*sd(results["exact2",]), lwd = 2, col = "red")
abline(v = mean(results["exact2",]) - 2*sd(results["exact2",]), lwd=2, col="red")
hist(results["exact3",])
abline(v = rate, lwd = 2, col = "green")
abline(v = mean(results["exact3",]), lwd = 2, col = "orange")
abline(v = mean(results["exact3",]) + 2*sd(results["exact3",]), lwd = 2, col = "red")
abline(v = mean(results["exact3",]) - 2*sd(results["exact3",]), lwd=2, col="red")


hist(results["approx",])
abline(v = rate, lwd = 2, col = "green")
abline(v = mean(results["approx",]), lwd = 2, col = "orange")
abline(v = mean(results["approx",]) + 2*sd(results["exact",]), lwd = 2, col = "red")
abline(v = mean(results["approx",]) - 2*sd(results["exact",]), lwd=2, col="red")

cat("True parameter:",
    rate,
    "\nAverage exact estimate:",
    round(mean(results["exact",]), 5),
    "\nAverage exact estimate 2:",
    round(mean(results["exact2",]), 5),
    "\nAverage exact estimate 3:",
    round(mean(results["exact3",]), 5),
    "\nAverage approx estimate:",
    round(mean(results["approx",]), 5)
    )
pairs(t(results))

draws <- rexp(n, rate)
y <- max(draws)
f <- function(theta) {
  (n-1)*y*exp(-theta*y)/(1-exp(-theta*y)) + 1/theta - y
}
f3 = function(theta) {
  (1-exp(-theta*y))/(y*(1-n*exp(-theta*y))) - theta
}
root_results <- uniroot(f, c(0, 10))
exact_sol <- root_results$root
curve(f3, exact_sol - .01, exact_sol + .01)
abline(h = 0)
abline(v = exact_sol)
curve(f3, log(n)/y - .0001, log(n)/y + .0001)
abline(v = log(n)/y)

