library(brms)
library(dplyr)

# Loading and pre-processing of data
kss = read.csv(file = '../Data/kss_data.csv')

arit = read.csv(file = '../Data/arithmetic_data.csv')

arit_processed = arit %>% group_by(ID, order_of_test) %>% summarize(performance = mean(correct))

arit_merged <- merge(arit_processed,
                     kss[kss$test_type == 'M',],
                     by.x=c("ID", "order_of_test"),
                     by.y=c("id", "order_t"),
                     all.x=FALSE,
                     all.y=FALSE)

arit_cont <- arit_merged[arit_merged$sd == 'Control',]
arit_test <- arit_merged[arit_merged$sd == 'Sleep Deprivation',]

n_cont <- nrow(arit_cont)
n_test <- nrow(arit_test)

median_performance_cont <- median(arit_cont$performance)
median_performance_test <- median(arit_test$performance)

median_rating_cont <- median(arit_cont$rating3)
median_rating_test <- median(arit_test$rating3)