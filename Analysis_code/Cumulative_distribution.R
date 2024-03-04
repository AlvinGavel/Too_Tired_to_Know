source('Preprocessing.R')

arit_control <- brm(
  formula = rating3 ~ 1 + performance,
  data = arit_control,
  family = cumulative("probit")
)

arit_test <- brm(
  formula = rating3 ~ 1 + performance,
  data = arit_test,
  family = cumulative("probit")
)

summary(arit_control)
summary(arit_test)