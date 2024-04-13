source('Analysis_code/Preprocessing.R')

# Function definitions
logB <- function(alpha, beta) {
  return(lgamma(alpha) + lgamma(beta) - lgamma(alpha + beta))
}

L <- function(n_above, n_below, P_vector) {
  log_L = n_above * log(P_vector) +
    n_below * log(1 - P_vector) -
    logB(n_above + 1, n_below + 1)
  return(exp(log_L))
}

# Magic numbers in statistics
clinical_significance <- 0.1

n_steps <- 1000
P_vector <- linspace(0., 1., n_steps)
delta_steps <- 2 * n_steps - 1
delta <- linspace(-1., 1., delta_steps)

# Parameters in plotting
control_colour <- "#0000FF"
test_colour <- "#FF0033"

performance_bounds <- list('arithmetic' = c(0.0,1.0),
                           'episodic memory' = c(0.0,1.0),
                           'working memory' = c(0.0,1.0),
                           'stroop' = c(0.0,1.0),
                           'simple attention' = c(100,1000))

rating_bounds <- c(0,10)
scatterplot_scatter <- 0.005


probability_insignificant <- c()
for (i in 1:length(datasets)) {
  dataset = datasets[i]
  n_above_cont <- 0
  n_below_cont <- 0
  n_above_test <- 0
  n_below_test <- 0
  for (time in 1:3) {
    rating_above_median_cont <- data_cont[[dataset]][[time]]$rating3 >= median_rating_cont[[dataset]][[time]]
    rating_below_median_cont <- data_cont[[dataset]][[time]]$rating3 < median_rating_cont[[dataset]][[time]]
    performance_above_median_cont <- data_cont[[dataset]][[time]]$performance >= median_performance_cont[[dataset]][[time]]
    rating_above_median_test <- data_test[[dataset]][[time]]$rating3 >= median_rating_test[[dataset]][[time]]
    rating_below_median_test <- data_test[[dataset]][[time]]$rating3 < median_rating_test[[dataset]][[time]]
    performance_above_median_test <- data_test[[dataset]][[time]]$performance >= median_performance_test[[dataset]][[time]]
    
    # Of the ones who scored above median, how many also rated themselves above/below median?
    n_above_cont <- n_above_cont + sum(rating_above_median_cont & performance_above_median_cont)[[1]]
    n_below_cont <- n_below_cont + sum(rating_below_median_cont & performance_above_median_cont)[[1]]
    n_above_test <- n_above_test + sum(rating_above_median_test & performance_above_median_test)[[1]]
    n_below_test <- n_below_test + sum(rating_below_median_test & performance_above_median_test)[[1]]
  }
  
  # What fraction does that constitute?
  frac_above_cont <- n_above_cont / (n_above_cont + n_below_cont)
  frac_above_test <- n_above_test / (n_above_test + n_below_test)
  
  png(filename=paste0("Plots/", dataset, "/Performance.png"))
  plot(c(),
       c(),
       main="Performance",
       xlab="Actual",
       ylab="Self-rated",
       xlim=performance_bounds[[dataset]],
       ylim=rating_bounds,
       cex=10)
  for (time in 1:3) {
    x_max <- performance_bounds[[dataset]][2]
    x_min <- performance_bounds[[dataset]][1]
    x_range <- x_max - x_min
    
    y_max <- rating_bounds[2]
    y_min <- rating_bounds[1]
    y_range <- y_max - y_min
    
    cont_performance <- data_cont[[dataset]][[time]]$performance
    cont_performance <- cont_performance + rnorm(length(cont_performance),
                                                 mean=0,
                                                 sd=x_range * scatterplot_scatter)
    
    cont_rating <- data_cont[[dataset]][[time]]$rating3
    cont_rating <- cont_rating + rnorm(length(cont_rating),
                                       mean=0,
                                       sd=y_range * scatterplot_scatter)
    
    test_performance <- data_test[[dataset]][[time]]$performance
    test_performance <- test_performance + rnorm(length(test_performance),
                                                 mean=0,
                                                 sd=x_range * scatterplot_scatter)
    
    test_rating <- data_test[[dataset]][[time]]$rating3
    test_rating <- test_rating + rnorm(length(test_rating),
                                       mean=0,
                                       sd=y_range * scatterplot_scatter)
    
    points(cont_performance,
           cont_rating,
           cex=0.1,
           pch=1,
           col=c(control_colour))
    points(test_performance,
           test_rating,
           cex=0.1,
           pch=1,
           col=c(test_colour))
    legend(performance_bounds[[dataset]][1],
           y_min + 0.1 * y_range,
           legend=c("Control", "Test"),
           cex=0.8,
           pch=1,
           col=c(control_colour, test_colour))
  }
  dev.off()
  
  L_cont <- L(n_above_cont, n_below_cont, P_vector)
  L_test <- L(n_above_test, n_below_test, P_vector)
  
  p_delta <- convolve(L_cont, L_test, type = 'open')
  
  probability_mass <- integrate(approxfun(delta, y = p_delta, method = "linear"), -1, 1)$value
  p_delta <- p_delta / probability_mass
  probability_insignificant[[dataset]] <- integrate(approxfun(delta, y = p_delta, method = "linear"), -clinical_significance, clinical_significance)$value
  
  print(paste0('For ', dataset, ' the probability that the difference is NOT clinically significant is ', format(round(probability_insignificant[[dataset]] * 100, 2), nsmall = 2), '%'))
  
  png(filename=paste0("Plots/", dataset, "/Difference.png"))
  plot(delta,
       p_delta,
       type="l",
       lty = "solid",
       xlim=c(-1,1),
       ylim =c(0, max(p_delta) * 1.1),
       xaxs="i",
       yaxs="i")
  abline(v=clinical_significance, col="black", lty = "dashed")
  abline(v=-clinical_significance, col="black", lty = "dashed")
  dev.off()
}