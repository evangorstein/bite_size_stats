library(ggeffects)
set.seed(1234)
x <- rnorm(200)

# Note that all the lines in the following plots are fitted values
# as the covariate x changes, where the other predictor in the model (z) is fixed at
# its mean


## Case 1
# quadratic relationship between x and z
z <- x^2 + rnorm(200)
# linear relationship between x and y
y <- 2 * x + 4 * z  + rnorm(200)

d <- data.frame(x, y, z)
m <- lm(y ~ x + z, data = d)

pr <- predict_response(m, "x [all]")
# Compare
plot(x, y)
plot(pr, show_data = TRUE) # y-coordinate of data points are actual values of y
plot(pr, show_residuals = TRUE) # y-coordinate of points are the data point's partial residual


# We fit a linear regression of y on predictors x and z
# The graph of counterfactual predictions where you are keeping z fixed
# and just varying x (this is what plot(pr) gives you) has to be a line
# because the regression model is linear in x.
# OTOH, the scatter plot of y versus x is quadratic in x because y is linear in z and z is quadratic in x
# This discrepancy does not mean your model is wrong--on the contrary, model is correct:
# conditional on z, y is really linear in x!
# But if you plot(pr, show_data = TRUE), data points show marginal relationship
# (which is quadratic), and you might be led to believe there is a problem in letting x enter into your model
# linearly



## Case 2
# no relationship between x and z
z <- 4 + rnorm(200)
# quadratic relationship between x and y
y <- 2 * x + + x^2 + 4 * z  + rnorm(200)

d <- data.frame(x, y, z)
m <- lm(y ~ x + z, data = d)

pr <- predict_response(m, "x [all]")
# Compare
plot(x, y)
plot(pr, show_data = TRUE)
plot(pr, show_residuals = TRUE)

# In this case, plotting the counterfactual predictions alongside the actual data makes you think
# model is adequate but plotting partial residuals show you the poor fit


# Moral: To determine if a given covariate x_1 enters into the regression with
# the right functional form, simple scatter plot of y versus x_1 doesn't work
# You want to look at partial residual versus x_1










