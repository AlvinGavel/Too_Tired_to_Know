source('Preprocessing.R')

arit_control <- brm(
  formula = rating3 ~ 1 + performance,
  data = arit_merged[arit_merged$sd == 'Control',],
  family = cumulative("probit")
)

arit_test <- brm(
  formula = rating3 ~ 1 + performance,
  data = arit_merged[arit_merged$sd == 'Sleep Deprivation',],
  family = cumulative("probit")
)

summary(arit_control)
summary(arit_test)