# idea comes for dynamic programming comes from https://www.youtube.com/watch?v=BAiuFOwhAWw

# X_t = Alice's score minus Bob's score at time t
# C_t = Coin flip at time t
# VP(X) = 1 if X > 0, 0 if X = 0, -1 if X < 0
# I want to write a program that calculates E[VP(X_100)], which is Alice's advantage (i.e. her probability of winning minus Bob's probability of winning)
# I define v_t(x,c) = E[VP(X_100) | X_t = x, C_t = c]
# Then E[VP(X_100)] = 0.5*v_1(0, H) + 0.5*v_1(0, T)
# So I just need to calculate v_1(0, H) and v_1(0, T)
# I can calculate for all x and c  v_100(x, c) = v_100(x, c) = VP(x)
# and given knowledge of v_t(x,c) for all x and c, I can calculate v_(t-1)(x, c) for all x and c


solve <- function(n_flips) {

  max_x = n_flips - 1
  min_x = -(floor(n_flips/2))
  range_x = max_x - min_x + 1

  # initialize empty array
  ary = array(data = NA, dim = c(n_flips, range_x, 2) )
  dimnames(ary) = list(
    time = 1:n_flips,
    x = min_x:max_x,
    flip = c("H", "T")
  )
  # initial conditions
  ary[n_flips,as.character(min_x:-1),] = -1
  ary[n_flips, "0",] = 0
  ary[n_flips,as.character(1:max_x),] = 1

  # Dynamically fill array
  later = ary[n_flips,,]
  for (t in 1:(n_flips-1)) {
    bottom_tails_idx = ceiling(t/2)
    bottom_heads_idx = bottom_tails_idx + 1
    top_idx = range_x - t
    tails_idx = bottom_tails_idx:top_idx
    heads_idx = bottom_heads_idx:top_idx

    # update rule for tails
    ary[n_flips -t , tails_idx, "T"] = 0.5*later[tails_idx, "H"] + 0.5*later[tails_idx, "T"]

    # update rule for heads
    ary[n_flips - t, heads_idx, "H"] = 0.5*later[heads_idx+1, "H"] + 0.5*later[heads_idx-1,"T"]

    later = ary[n_flips - t,,]
  }

  return(ary)

}


ary = solve(100)
cat("Bob's advantage is", -sum(50*ary[1,"0",]), "percent")

flips = seq(3,100,2)
bobs_advs = sapply(flips, \(x) -sum(50*solve(x)[1,"0",]))


plot(flips, bobs_advs,
     ylab = "Bob's advantage (percent)",
     pch = 2,
     ylim = c(0, 50))







