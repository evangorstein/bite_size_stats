#Automatic differentiation

library(plotly)
library(autodiffr)
ad_setup()

sigmoid = function(x) exp(x)/(1+exp(x))

f <- function(a, b) {

  inner_activation <- sigmoid(a)
  z <- b*inner_activation
  outter_activation <- sigmoid(z)
  return(outter_activation)

}

f2 <- function(x) {
  f(x[1], x[2])
}


a = seq(-1, 1, .1)
b = seq(-1, 1, .1)
z = outer(a, b, f)

# Take transpose because we want the first argument of function, a, to be the column index so that it gets mapped tox-axis
# and second argument, b, to be the row index so that it gets mapped to the y axis
# By default, outer has the reverse: first argument of function is row index and second argument of function is column index

plot_ly(z=t(z), x=a, y=b, type = "surface") %>%
  layout(scene = list(xaxis = list(title = "a"),
                      yaxis = list(title = "b")))


# Automatic differentiation
# The gradient with respect to second argument, b, is always positive
# In contrast, we see that the sign of the gradient with respect to the first argument depends on the second argument

ad_grad(f2, c(-1, -1))
ad_grad(f2, c(-1, 0)) ## deriv(f, c(2, 3))
ad_grad(f2, c(-1, 1))
ad_grad(f2, c(0, -1))
ad_grad(f2, c(0, 0))
ad_grad(f2, c(0, 1))
ad_grad(f2, c(1, -1))
ad_grad(f2, c(1, 0))
ad_grad(f2, c(1, 1))
