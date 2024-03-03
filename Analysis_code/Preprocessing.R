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