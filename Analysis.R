library(brms)
library(dplyr)

# Loading and pre-processing of data
kss = read.csv(file = 'Data/kss_data.csv')
arit = kss[kss$test_type == 'M',]
arit$is_sd = as.integer(arit$sd == 'Sleep Deprivation')

# Here we will do the fitting of the cumulative model, following Burkner and Vuorre

fit_ad <- brm(
  formula = rating3 ~ 1 + is_sd,
  data = arit,
  family = cumulative("probit")
)

summary(fit_ad)