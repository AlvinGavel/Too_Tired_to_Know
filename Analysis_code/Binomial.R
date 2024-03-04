source('Preprocessing.R')

rating_above_median_cont <- arit_cont$rating3 > median_rating_cont
performance_above_median_cont <- arit_cont$performance > median_performance_cont
rating_above_median_test <- arit_test$rating3 > median_rating_test 
performance_above_median_test <- arit_test$performance > median_performance_test

above_cont <- rating_above_median_cont & performance_above_median_cont
above_test <- rating_above_median_test & performance_above_median_test

frac_above_cont <- sum(above_cont) / (n_cont / 2)
frac_above_test <- sum(above_test) / (n_test / 2)

# [TODO: Put in actual binomial regression here]