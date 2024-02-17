library(rethinking)
sppnames <- c( "afarensis","africanus","habilis","boisei",
               "rudolfensis","ergaster","sapiens")
brainvolcc <- c( 438 , 452 , 612, 521, 752, 871, 1350 )
masskg <- c( 37.0 , 35.5 , 34.5 , 41.5 , 55.5 , 61.0 , 53.5 )
d <- data.frame( species=sppnames , brain=brainvolcc , mass=masskg )
d$mass_std <- (d$mass - mean(d$mass))/sd(d$mass)
d$brain_std <- d$brain / max(d$brain)
m7.1 <- quap(
  alist(
    brain_std ~ dnorm( mu , exp(log_sigma) ),
    mu <- a + b*mass_std,
    a ~ dnorm( 0.5 , 1 ),
    b ~ dnorm( 0 , 10 ),
    log_sigma ~ dnorm( 0 , 1 )
  ), data=d )
set.seed(12)
post <- extract.samples(m7.1)
dens(post$b)
dens(post$a)


m7.1_OLS <- lm( brain_std ~ mass_std , data=d )
post_ols <- extract.samples( m7.1_OLS )
dens(post_ols$mass_std)
dens(post_ols$Intercept)

set.seed(12)
s <- link( m7.1 )
r <- apply(s,2,mean) - d$brain_std
resid_var <- var2(r)
outcome_var <- var2( d$brain_std )
1 - resid_var/outcome_var

R2_is_bad <- function( quap_fit ) {
  s <- sim( quap_fit , refresh=0 )
  r <- apply(s,2,mean) - d$brain_std
  1 - var2(r)/var2(d$brain_std)
}

myR2 <- function( quap_fit ) {
  s <- link(quap_fit)
  r <- apply(s,2,mean) - d$brain_std
  1 - var2(r)/var2(d$brain_std)
}

m7.2 <- quap(
  alist(
    brain_std ~ dnorm( mu , exp(log_sigma) ),
    mu <- a + b[1]*mass_std + b[2]*mass_std^2,
    a ~ dnorm( 0.5 , 1 ),
    b ~ dnorm( 0 , 10 ),
    log_sigma ~ dnorm( 0 , 1 )
  ), data=d , start=list(b=rep(0,2)) )

m7.2_OLS <-  lm( brain_std ~ mass_std + I(mass_std^2), data=d )

m7.3 <- quap(
  alist(
    brain_std ~ dnorm( mu , exp(log_sigma) ),
    mu <- a + b[1]*mass_std + b[2]*mass_std^2 +
      b[3]*mass_std^3,
    a ~ dnorm( 0.5 , 1 ),
    b ~ dnorm( 0 , 10 ),
    log_sigma ~ dnorm( 0 , 1 )
  ), data=d , start=list(b=rep(0,3)) )
m7.3_OLS <-  lm( brain_std ~ mass_std + I(mass_std^2) + I(mass_std^3), data=d )

m7.4 <- quap(
  alist(
    brain_std ~ dnorm( mu , exp(log_sigma) ),
    mu <- a + b[1]*mass_std + b[2]*mass_std^2 +
      b[3]*mass_std^3 + b[4]*mass_std^4,
    a ~ dnorm( 0.5 , 1 ),
    b ~ dnorm( 0 , 10 ),
    log_sigma ~ dnorm( 0 , 1 )
  ), data=d , start=list(b=rep(0,4)) )
m7.4_OLS <-  lm( brain_std ~ mass_std + I(mass_std^2) + I(mass_std^3) 
                 + I(mass_std^4), data=d )

m7.5 <- quap(
  alist(
    brain_std ~ dnorm( mu , exp(log_sigma) ),
    mu <- a + b[1]*mass_std + b[2]*mass_std^2 +
      b[3]*mass_std^3 + b[4]*mass_std^4 +
      b[5]*mass_std^5,
    a ~ dnorm( 0.5 , 1 ),
    b ~ dnorm( 0 , 10 ),
    log_sigma ~ dnorm( 0 , 1 )
  ), data=d , start=list(b=rep(0,5)) )
m7.5_OLS <-  lm( brain_std ~ mass_std + I(mass_std^2) + I(mass_std^3) 
                 + I(mass_std^4) + I(mass_std^5), data=d )


m7.6 <- quap(
  alist(
    brain_std ~ dnorm( mu , 0.001),
    mu <- a + b[1]*mass_std + b[2]*mass_std^2 +
      b[3]*mass_std^3 + b[4]*mass_std^4 +
      b[5]*mass_std^5 + b[6]*mass_std^6,
    a ~ dnorm( 0.5 , 1 ),
    b ~ dnorm( 0 , 10 )
  ), data=d , start=list(b=rep(0,6)) )
m7.6_OLS <-  lm( brain_std ~ mass_std + I(mass_std^2) + I(mass_std^3) 
                 + I(mass_std^4) + I(mass_std^5) + I(mass_std^6), data=d )

set.seed(12)
R2_is_bad(m7.1)
myR2(m7.1)
summary(m7.1_OLS)$r.squared

R2_is_bad(m7.2)
myR2(m7.2)
summary(m7.2_OLS)$r.squared


R2_is_bad(m7.3)
myR2(m7.3)
summary(m7.3_OLS)$r.squared

R2_is_bad(m7.4)
myR2(m7.4)
summary(m7.4_OLS)$r.squared

R2_is_bad(m7.5)
myR2(m7.5)
summary(m7.5_OLS)$r.squared

R2_is_bad(m7.6)
myR2(m7.6)
summary(m7.6_OLS)$r.squared

plot_fit <- function(quap, title) {
  mass_seq <- seq( from=min(d$mass_std) , to=max(d$mass_std) , length.out=100 )
  l <- link( quap , data=list( mass_std=mass_seq ) )
  mu <- apply( l , 2 , mean )
  ci <- apply( l , 2 , PI )
  s <- sim( quap , data=list( mass_std=mass_seq ))
  pi <- apply( s , 2 , PI )
  plot( brain_std ~ mass_std , data=d, main = title )
  lines( mass_seq , mu )
  shade( ci , mass_seq )
  shade(pi, mass_seq, col = col.alpha('red', 0.2))
}
pdf('plots.pdf')
par(mfrow = c(2,3))
plot_fit(m7.1, "degree 1")
plot_fit(m7.2, "degree 2")
plot_fit(m7.3, "degree 3")
plot_fit(m7.4, "degree 4")
plot_fit(m7.5, "degree 5")
plot_fit(m7.6, "degree 6")
dev.off()


