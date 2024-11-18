# Stolen from Simon Wood's GAMs textbook section 2.4.4

library(nlme)

llm <- function(theta, X, Z, y) {
  # untransform parameters
  sigma.b <- exp(theta[1])
  sigma <- exp(theta[2])
  # extract dimensions
  n <- length(y); pr <- ncol(Z); pf <- ncol(X)
  # obtain betahat and bhat
  X1 <- cbind(X, Z)
  ipsi <- c(rep(0, pf), rep(1/sigma.b^2, pr))
  b1 <- solve(crossprod(X1) / sigma^2 + diag(ipsi),
              t(X1) %*% y / sigma^2)
  # compute log|Z'Z/sigma^2 + I/sigma.b^2|
  chol_fact <- chol( crossprod(Z) / sigma^2 + diag(ipsi[-(1:pf)]) )
  ldet <- 2*sum(log(diag(chol_fact)))
  # compute the log profile likelihood
  bhat = b1[-(1:pf)]
  l = -( sum((y - X1 %*% b1)^2)/sigma^2 + sum(bhat^2)/sigma.b^2 +
           n*log(sigma^2) + pr*log(sigma.b^2) + ldet + n*log(2*pi) )/2
  attr(l,"b") <- as.numeric(b1) ## return \hat beta and \hat b
  -l
}

data(Rail)
Rail$Rail = factor(Rail$Rail, ordered = F)
Z <- model.matrix( ~ Rail$Rail - 1)
X <- matrix(1, nrow = nrow(Rail), ncol = 1)
rail.mod <- optim(c(0,0),llm,hessian=TRUE,
                  X=X,Z=Z,y=Rail$travel)
#minimum negative likelihood achieved
rail.mod$value
# max-likelihood estimates of variance components on standard deviation scale
exp(rail.mod$par)
# approximate (asymptotic) covariance matrix for theta
(ass_var = solve(rail.mod$hessian))
# Fixed and random effect estimates/predictions
attr(llm(rail.mod$par,X,Z,Rail$travel),"b")


