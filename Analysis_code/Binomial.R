source('Preprocessing.R')

clinical_significance = 0.1

rating_above_median_cont <- arit_cont$rating3 >= median_rating_cont
rating_below_median_cont <- arit_cont$rating3 < median_rating_cont
performance_above_median_cont <- arit_cont$performance >= median_performance_cont
rating_above_median_test <- arit_test$rating3 >= median_rating_test 
rating_below_median_test <- arit_test$rating3 < median_rating_test 
performance_above_median_test <- arit_test$performance >= median_performance_test

# Of the ones who scored above median, how many also rated themselves above/below median?
above_cont <- rating_above_median_cont & performance_above_median_cont
below_cont <- rating_below_median_cont & performance_above_median_cont
above_test <- rating_above_median_test & performance_above_median_test
below_test <- rating_below_median_test & performance_above_median_test

# What fraction does that constitute?
frac_above_cont <- sum(above_cont) / (n_cont / 2)
frac_above_test <- sum(above_test) / (n_test / 2)

logB <- function(alpha, beta) {
       return(lgamma(alpha) + lgamma(beta) - lgamma(alpha + beta))
}


n_steps <- 1000
P_vector <- linspace(0., 1., n_steps)

L <- function(n_above, n_below) {
  log_L = n_above * log(P_vector) +
  n_below * log(1 - P_vector) -
  logB(sum(above_cont) + 1, sum(below_cont) + 1)
  return(exp(log_L))
}

L_cont <- L(sum(above_cont), sum(below_cont))
L_test <- L(sum(above_test), sum(below_test))

plot(P_vector, L_cont)
plot(P_vector, L_test)


delta_steps = 2 * n_steps - 1
delta_vector = linspace(-1., 1., delta_steps)

delta = convolve(L_cont, L_test, type = 'open')



plot(delta_vector, delta, type="l", lty = "solid", xlim=c(-1,1), ylim =c(0, max(delta)* 1.1), xaxs="i", yaxs="i")
abline(v=clinical_significance, col="black", lty = "dashed")
abline(v=-clinical_significance, col="black", lty = "dashed")