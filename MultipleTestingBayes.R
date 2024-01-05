options(digits=4)
library(dplyr)
library(purrr)
spidey <- function(sigma, tau, N) {
  cat("sigma = ", sigma, ", tau = ", tau, ", N = ", N, "\n", sep="")
  
  theta <- rnorm(N, 0, tau)
  y <- rnorm(N, theta, sigma)
  
  #Classical
  classical_cover <- pmap_lgl(.l = list(theta, y-2*sigma, y+2*sigma), .f = between)
  classical_signif <- abs(y) > 2*sigma
  cat(100*mean(classical_cover), "% of the 95% classical intervals include the true parameter\n", sep="")
  cat(100*mean(classical_signif), "% of the 95% classical intervals exclude 0\n", sep="")
  
  classical_signif_theta = theta[classical_signif]
  classical_signif_est = y[classical_signif]
  classical_signif_cover <- pmap_lgl(list(classical_signif_theta, 
                                          classical_signif_est - 2*sigma, 
                                          classical_signif_est + 2*sigma), 
                                     between)
  cat(100*mean(classical_signif_cover), "% of these \"significant\" intervals contain the true parameter\n", sep="")
  
  
  #Bayes
  theta_hat_bayes <- y * (1/sigma^2) / (1/sigma^2 + 1/tau^2)
  theta_se_bayes <- sqrt(1 / (1/sigma^2 + 1/tau^2))
  
  bayes_signif <- abs(theta_hat_bayes) > 2*theta_se_bayes
  bayes_cover <- pmap_lgl(list(theta, 
                               theta_hat_bayes - 2*theta_se_bayes, 
                               theta_hat_bayes + 2*theta_se_bayes),
                          between)
  cat(100*mean(bayes_cover), "% of the 95% credible intervals exclude the true parameter\n", sep="")
  cat(100*mean(bayes_signif), "% of the 95% credible intervals exclude 0\n", sep="")
  
  bayes_signif_theta = theta[bayes_signif]
  bayes_signif_est = theta_hat_bayes[bayes_signif]
  bayes_signif_cover <- pmap_lgl(list(bayes_signif_theta, 
                                      bayes_signif_est - 2*theta_se_bayes, 
                                      bayes_signif_est + 2*theta_se_bayes),
                                 between)
  cat(100*mean(bayes_signif_cover), "% of these \"significant\" intervals contain the true parameter", sep="")
}


sigma <- 1
tau <- .5
N <- 1e6
spidey(sigma, tau, N)



spidey_wrong_prior <- function(sigma, tau, multiplier, N) {
  cat("sigma = ", sigma, ", tau = ", tau, ", N = ", N, "\n", sep="")
  cat("Prior is corrupted with mutlipler", multiplier, "\n")
  
  theta <- ifelse(runif(N) > .001,  rnorm(N, 0, tau), rnorm(N, 0, multiplier*tau))
  y <- rnorm(N, theta, sigma)
  
  #Classical
  classical_cover <- pmap_lgl(.l = list(theta, y-2*sigma, y+2*sigma), .f = between)
  classical_signif <- abs(y) > 2*sigma
  cat(100*mean(classical_cover), "% of the 95% classical intervals include the true parameter\n", sep="")
  cat(100*mean(classical_signif), "% of the 95% classical intervals exclude 0\n", sep="")
  
  classical_signif_theta = theta[classical_signif]
  classical_signif_est = y[classical_signif]
  classical_signif_cover <- pmap_lgl(list(classical_signif_theta, 
                                          classical_signif_est - 2*sigma, 
                                          classical_signif_est + 2*sigma), 
                                     between)
  cat(100*mean(classical_signif_cover), "% of these \"significant\" intervals contain the true parameter\n", sep="")
  
  
  #Bayes
  theta_hat_bayes <- y * (1/sigma^2) / (1/sigma^2 + 1/tau^2)
  theta_se_bayes <- sqrt(1 / (1/sigma^2 + 1/tau^2))
  
  bayes_signif <- abs(theta_hat_bayes) > 2*theta_se_bayes
  bayes_cover <- pmap_lgl(list(theta, 
                               theta_hat_bayes - 2*theta_se_bayes, 
                               theta_hat_bayes + 2*theta_se_bayes),
                          between)
  cat(100*mean(bayes_cover), "% of the 95% credible intervals exclude the true parameter\n", sep="")
  cat(100*mean(bayes_signif), "% of the 95% credible intervals exclude 0\n", sep="")
  
  bayes_signif_theta = theta[bayes_signif]
  bayes_signif_est = theta_hat_bayes[bayes_signif]
  bayes_signif_cover <- pmap_lgl(list(bayes_signif_theta, 
                                      bayes_signif_est - 2*theta_se_bayes, 
                                      bayes_signif_est + 2*theta_se_bayes),
                                 between)
  cat(100*mean(bayes_signif_cover), "% of these \"significant\" intervals contain the true parameter", sep="")
}

multiplier=6


