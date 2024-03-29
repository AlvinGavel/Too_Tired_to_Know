source('Preprocessing.R')

clinical_significance = 0.1

n_above_cont <- 0
n_below_cont <- 0
n_above_test <- 0
n_below_test <- 0
for (time in 1:3) {
  rating_above_median_cont <- arit_cont[[time]]$rating3 >= median_rating_cont[[time]]
  rating_below_median_cont <- arit_cont[[time]]$rating3 < median_rating_cont[[time]]
  performance_above_median_cont <- arit_cont[[time]]$performance >= median_performance_cont[[time]]
  rating_above_median_test <- arit_test[[time]]$rating3 >= median_rating_test[[time]]
  rating_below_median_test <- arit_test[[time]]$rating3 < median_rating_test[[time]]
  performance_above_median_test <- arit_test[[time]]$performance >= median_performance_test[[time]]
  
  # Of the ones who scored above median, how many also rated themselves above/below median?
  n_above_cont <- n_above_cont + sum(rating_above_median_cont & performance_above_median_cont)[[1]]
  n_below_cont <- n_below_cont + sum(rating_below_median_cont & performance_above_median_cont)[[1]]
  n_above_test <- n_above_test + sum(rating_above_median_test & performance_above_median_test)[[1]]
  n_below_test <- n_below_test + sum(rating_below_median_test & performance_above_median_test)[[1]]
}

# What fraction does that constitute?
frac_above_cont <- n_above_cont / (n_above_cont + n_below_cont)
frac_above_test <- n_above_test / (n_above_test + n_below_test)

logB <- function(alpha, beta) {
  return(lgamma(alpha) + lgamma(beta) - lgamma(alpha + beta))
}

n_steps <- 1000
P_vector <- linspace(0., 1., n_steps)

L <- function(n_above, n_below) {
  log_L = n_above * log(P_vector) +
    n_below * log(1 - P_vector) -
    logB(n_above + 1, n_below + 1)
  return(exp(log_L))
}

L_cont <- L(n_above_cont, n_below_cont)
L_test <- L(n_above_test, n_below_test)

plot(P_vector, L_cont)
plot(P_vector, L_test)


delta_steps <- 2 * n_steps - 1
delta_vector <- linspace(-1., 1., delta_steps)

delta <- convolve(L_cont, L_test, type = 'open')

probability_mass <- integrate(approxfun(delta_vector, y = delta, method = "linear"), -1, 1)$value
delta <- delta / probability_mass
probability_insignificant <- integrate(approxfun(delta_vector, y = delta, method = "linear"), -clinical_significance, clinical_significance)$value

print(paste0('The probability that the difference is NOT clinically significant is ', format(round(probability_insignificant * 100, 2), nsmall = 2), '%'))

plot(delta_vector, delta, type="l", lty = "solid", xlim=c(-1,1), ylim =c(0, max(delta)* 1.1), xaxs="i", yaxs="i")
abline(v=clinical_significance, col="black", lty = "dashed")
abline(v=-clinical_significance, col="black", lty = "dashed")
