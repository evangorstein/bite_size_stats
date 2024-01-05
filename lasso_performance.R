library(glmnet)
library(fields)
# Define constants
p <- 1000
sigma <- 4
stn_ratio = 10

alphas <- seq(0.1, 1, by=0.1)
rhos <- seq(0.05, 1, by=0.05)


# Generate data and return error from a lasso fit
gen_and_get_err <- function(N, k) {
  
  ### Takes in design, response, lambda, and true beta
  # fits lasso and calculates the l2 loss
  get_err <- function(X, y, lambda, true_beta) {
    
    mod <- glmnet(X, y, lambda=lambda, standardize = FALSE, intercept = FALSE)
    l2_err <- sqrt(sum((as.vector(mod$beta) - true_beta)^2))
    return(l2_err)
    
  }
  
  X <- matrix(rnorm(N*p), ncol= p)  
  S = sample(1:p, k)
  true_beta <- rep(0, p)
  true_beta[S] = rnorm(k)
  w = rnorm(N, sd=sigma)
  signal = as.vector(X %*% true_beta) 
  
  #Normalize signal to achieve desired signal to noise ratio stn_ratio
  var_sig = var(signal)
  signal = sigma*sqrt(stn_ratio/var_sig)*signal
  print(var(signal)/sigma^2) #Should be approximately 10
  
  y = signal + w
  
  lambda = 2*sigma*sqrt( 3*log(exp(1)*p/k)/N)
  
  return(get_err(X, y, lambda, true_beta))
}

med_err <- matrix(nrow=length(rhos), ncol=length(alphas))
for (j in 1:length(alphas)) {
  
  for (i in 1:length(rhos)) {
    
    N = p*alphas[j]
    k = N*rhos[i]
    med_err[i,j] = median(replicate(10, gen_and_get_err(N, k)))
    print(c(rhos[i],alphas[j]))
  }
  
}

image.plot(x = alphas,
           y = rhos, 
           z = t(med_err), 
           col = hcl.colors(12, "YlOrRd", rev = TRUE))




