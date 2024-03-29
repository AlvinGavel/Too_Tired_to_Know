library(brms)
library(dplyr)
library(pracma)

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

arit_cont <- list()
arit_test <- list()
median_performance_cont <- list()
median_performance_test <- list()
median_rating_cont <- list()
median_rating_test <- list()

for (time in 1:3) {
  arit_cont[[time]] <- arit_merged[arit_merged$sd == 'Control' & arit_merged$time == time,]
  arit_test[[time]] <- arit_merged[arit_merged$sd == 'Sleep Deprivation' & arit_merged$time == time,]
  
  median_performance_cont[[time]] <- median(arit_cont[[time]]$performance)
  median_performance_test[[time]] <- median(arit_test[[time]]$performance)
  
  median_rating_cont[[time]] <- median(arit_cont[[time]]$rating3)
  median_rating_test[[time]] <- median(arit_test[[time]]$rating3)
}

