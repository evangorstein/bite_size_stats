library(rethinking)
library(ggplot2)
library(dplyr)
data("Howell1")
d <- Howell1
d <- d %>%
  mutate(age_bracket = cut(age, 
                           breaks=c(-Inf, 20, 40, 60, Inf), 
                           labels=c("Child", "Young", "Mid-age", "Old")))

ggplot(d) +
  geom_point(aes(height, weight, col = age_bracket)) +
  coord_equal(xlim = c(0, 180), ylim = c(0, 65))


d2 <- d %>%
  filter(age >= 18)


m4.3 <- quap(
  alist(
    weight ~ dnorm( mu , sigma ) ,
    mu <- a + b*height,
    a ~ dnorm( 0 , 10 ) ,
    b ~ dunif( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) , data=d2 )


precis(m4.3)

post <- extract.samples(m4.3) 
plot( d2$height, d2$weight, col=2, lwd=3, xlab = "height (cm)", ylab = "width (kg)")
for (j in 1:20) {
  abline( a = post$a[j], b = post$b[j], lwd=1)
}

height_seq = seq(130, 190, length.out = 20)
W_postpred = sim( m4.3, data = list(height = height_seq))
W_PI <- apply(W_postpred, 2, rethinking::PI)
lines(height_seq, W_PI[1,],lty=2, lwd=2)
lines(height_seq, W_PI[2,], lty=2, lwd=2)







