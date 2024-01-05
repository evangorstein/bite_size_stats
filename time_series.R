r = 10
w = seq(-pi, pi, length.out = r)
a = seq(-r/2+.5, r/2-.5)

real_ts = 100
u = matrix(runif(r*real_ts, -pi, pi), nrow = r)

len_ts = 100
t = 1:len_ts
ts = matrix(nrow = len_ts, ncol = real_ts)

for (i in 1:len_ts) {
  ts[i,] = colSums(a*cos(t[i]*w + u))
}

plot(ts[,2])
lines(ts[,2])


plot(ts[,25])
lines(ts[,25])


