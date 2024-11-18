


get_cov <- function(index_dif, smooth = TRUE) {

  if (index_dif == 0) {
    return(1)
  } else if (index_dif == 1) {
    return(.9)
  } else if (index_dif == 2) {
    return(if (smooth) .7 else .8)
  }

}

sigma = matrix(nrow = 3, ncol = 3)
for (i in 1:nrow(sigma)) {
  for (j in 1:ncol(sigma)) {

    sigma[i,j] = get_cov(abs(i-j), smooth = TRUE)

  }
}


obs = c(1,2)
sigma12 = sigma[1:2, 1:2]
cond_mu3 = sum(sigma[3,1:2] * solve(sigma12, obs))
cond_mu3





