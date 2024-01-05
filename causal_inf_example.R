# Toy example of the issue in choosing right hand side of regression.
# Knowledge (perhaps more rightly called intelligence) and how much you study together
# determine your grade. Knowledge affects how much you study ("If I think I'm smart, I'll study less.")

know = rnorm(1000, 2.5, 1)
study = 5 - know + rnorm(1000, 0, 1)
grade = know + study + rnorm(1000, 0, 1)
mod = lm(grade~know+study)
summary(mod)
mod = lm(grade~know)
summary(mod)
#Which regression do you prefer? 