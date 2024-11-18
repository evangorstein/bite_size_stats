alpha_solve <- function(beta, sigma=1){
  alpha = sigma*sqrt(gamma(1/beta)/gamma(3/beta))
}

my_dg <- function(x, beta = 2, sigma = 1) {
  dgnorm(x, beta = beta, alpha = alpha_solve(beta, sigma))
}

my_rg <- function(n, beta = 2, sigma = 1) {
  rgnorm(n, beta = beta, alpha = alpha_solve(beta, sigma))
}

cols = rainbow(4)
betas = c(1,2,3,8)
curve(my_dg(x, beta = betas[1]), -4,4, col = cols[1], ylab = "density")
for (i in seq_along(betas[-1])) {
  curve(my_dg(x, beta = betas[i+1]), add = T, col = cols[i+1])
}

