library(rethinking)

bf_sim <- function(n=100, bXY=1, bUX=1, bUZ=1, bVZ=1, bVY=1) {
  
  U <- rnorm(n) 
  X <- rnorm(n, bUX*U)
  V <- rnorm(n)
  Y <- rnorm(n, bXY*X + bVY*V)
  Z <- rnorm(n, bUZ*U + bVZ*V)
  
  est_marg <- coef( lm(Y ~ X ) )["X"]
  est_cont <- coef( lm(Y ~ X + Z) )["X"]
  
  return( c(est_marg, est_cont) )
}

estimates <- mcreplicate(1e4, bf_sim(), mc.cores = 8)
dens( estimates[1,], lwd = 2, xlab = "posterior mean")
dens(estimates[2,], lwd =2, add=TRUE, col=2)

