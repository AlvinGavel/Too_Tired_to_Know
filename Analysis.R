library(brms)
library(dplyr)

# Loading and pre-processing of data
kss = read.csv(file = 'Data/kss_data.csv')

arit = read.csv(file = 'Data/arithmetic_data.csv')
arit_processed = arit %>% group_by(ID, order_of_test) %>% summarize(performance = mean(correct))

arit_merged <- merge(arit_processed,
                     kss[kss$test_type == 'M',],
                     by.x=c("ID", "order_of_test"),
                     by.y=c("id", "order_t"),
                     all.x=FALSE,
                     all.y=FALSE)

# Here we will do the fitting of the cumulative model, following Bürkner and Vuorre

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


# TODO:
# Main analysis defines performance as proportion correct, but make an additional
# analysis (to go in an appendix) looking at answers per unit time