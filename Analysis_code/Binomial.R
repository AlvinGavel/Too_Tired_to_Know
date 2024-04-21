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
sleepiness_bounds <- c(0, 9)
scatterplot_scatter <- 0.005


P_insignificant <- c()
for (i in 1:length(datasets)) {
  print(paste0('For ', dataset, ':'))
  
  dataset = datasets[i]
  n_correct <- 0
  n_below_cont <- 0
  n_above_test <- 0
  n_below_test <- 0
  for (time in 1:3) {
    print(paste0('   For time ', time))
    print(paste0('      The median self-rated performance in the control group was ', median_rating_cont[[dataset]][[time]]))
    print(paste0('      The median self-rated performance in the test group was ', median_rating_test[[dataset]][[time]]))
    print(paste0('      The median actual performance in the control group was ', format(median_performance_cont[[dataset]][[time]]), nsmall = 2))
    print(paste0('      The median actual performance in the test group was ', format(median_performance_test[[dataset]][[time]]), nsmall = 2))
    print(paste0('      The median reported sleepiness in the control group was ', median_sleepiness_cont[[dataset]][[time]]))
    print(paste0('      The median reported sleepiness in the test group was ', median_sleepiness_test[[dataset]][[time]]))
    rating_above_median_cont <- data_cont[[dataset]][[time]]$rating3 >= median_rating_cont[[dataset]][[time]]
    rating_below_median_cont <- data_cont[[dataset]][[time]]$rating3 < median_rating_cont[[dataset]][[time]]
    performance_above_median_cont <- data_cont[[dataset]][[time]]$performance >= median_performance_cont[[dataset]][[time]]
    performance_below_median_cont <- data_cont[[dataset]][[time]]$performance < median_performance_cont[[dataset]][[time]]
    sleepiness_above_median_cont <- data_cont[[dataset]][[time]]$rating1 >= median_sleepiness_cont[[dataset]][[time]]
    sleepiness_below_median_cont <- data_cont[[dataset]][[time]]$rating1 < median_sleepiness_cont[[dataset]][[time]]
    
    rating_above_median_test <- data_test[[dataset]][[time]]$rating3 >= median_rating_test[[dataset]][[time]]
    rating_below_median_test <- data_test[[dataset]][[time]]$rating3 < median_rating_test[[dataset]][[time]]
    performance_above_median_test <- data_test[[dataset]][[time]]$performance >= median_performance_test[[dataset]][[time]]
    performance_below_median_test <- data_test[[dataset]][[time]]$performance < median_performance_test[[dataset]][[time]]
    sleepiness_above_median_test <- data_test[[dataset]][[time]]$rating1 >= median_sleepiness_test[[dataset]][[time]]
    sleepiness_below_median_test <- data_test[[dataset]][[time]]$rating1 < median_sleepiness_test[[dataset]][[time]]
    
    # How prone were people to rate themselves accurately w.r.t. the median
    n_acc_cont <- n_correct + sum(rating_above_median_cont & performance_above_median_cont)[[1]] + sum(rating_below_median_cont & performance_below_median_cont)[[1]]
    n_inacc_cont <- n_below_cont + sum(rating_below_median_cont & performance_above_median_cont)[[1]] + sum(rating_above_median_cont & performance_below_median_cont)[[1]]
    n_acc_test <- n_above_test + sum(rating_above_median_test & performance_above_median_test)[[1]] + sum(rating_below_median_test & performance_below_median_test)[[1]]
    n_inacc_test <- n_below_test + sum(rating_below_median_test & performance_above_median_test)[[1]] + sum(rating_above_median_test & performance_below_median_test)[[1]]
  }
  
  ylab = c('Self-rated performance', 'Sleepiness')
  ybounds = c(list(rating_bounds), list(sleepiness_bounds))
  ydata = c('rating3', 'rating1')
  for (y in 1:2) {
    png(filename=paste0("Plots/", dataset, paste0("/Actual_performance_", ylab[y], ".png")))
    plot(c(),
         c(),
         xlab="Actual performance",
         ylab=ylab[y],
         xlim=performance_bounds[[dataset]],
         ylim=ybounds[[y]],
         cex=10)
    for (time in 1:3) {
      x_range <- performance_bounds[[dataset]][2] - performance_bounds[[dataset]][1]
      y_min <- ybounds[[y]][1]
      y_range <- ybounds[[y]][2] - y_min
      
      cont_performance <- data_cont[[dataset]][[time]]$performance
      cont_performance <- cont_performance + rnorm(length(cont_performance),
                                                   mean=0,
                                                   sd=x_range * scatterplot_scatter)
      
      cont_rating <- data_cont[[dataset]][[time]][[ydata[y]]]
      cont_rating <- cont_rating + rnorm(length(cont_rating),
                                         mean=0,
                                         sd=y_range * scatterplot_scatter)
      
      test_performance <- data_test[[dataset]][[time]]$performance
      test_performance <- test_performance + rnorm(length(test_performance),
                                                   mean=0,
                                                   sd=x_range * scatterplot_scatter)
      
      test_rating <- data_test[[dataset]][[time]][[ydata[y]]]
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
    }
    legend(performance_bounds[[dataset]][1],
           y_min + 0.1 * y_range,
           legend=c("Control", "Test"),
           cex=0.8,
           pch=1,
           col=c(control_colour, test_colour))
    dev.off()}
  
  
  # What fraction does that constitute?
  frac_acc_cont <- n_acc_cont / (n_acc_cont + n_inacc_cont)
  frac_acc_test <- n_acc_test / (n_acc_test + n_inacc_test)
  print(paste0('In the control group ', format(round(frac_acc_cont * 100, 2), nsmall = 2), '% rated themselves accurately'))
  print(paste0('In the test group ', format(round(frac_acc_test * 100, 2), nsmall = 2), '% rated themselves accurately'))
  
  L_cont <- L(n_acc_cont, n_inacc_cont, P_vector)
  L_test <- L(n_acc_test, n_inacc_test, P_vector)
  max_L = max(c(max(L_cont), max(L_test)))
  
  png(filename=paste0("Plots/", dataset, "/Metacognitive_performance.png"))
  plot(c(),
       c(),
       main=dataset,
       xlab="P",
       ylab="p_(a,a)^x",
       xlim=c(0,1),
       ylim=c(0, max_L),
       cex=10)
  lines(P_vector,
        L_cont,
        type="l",
        lty = "solid",
        xaxs="i",
        yaxs="i",
        col=control_colour)
  lines(P_vector,
        L_test,
        type="l",
        lty = "solid",
        xaxs="i",
        yaxs="i",
        col=test_colour)
  legend(0,
         max_L,
         legend=c("Control", "Test"),
         cex=0.8,
         pch=1,
         col=c(control_colour, test_colour))
  dev.off()
  
  p_delta <- convolve(L_cont, L_test, type = 'open')
  
  probability_mass <- integrate(approxfun(delta, y = p_delta, method = "linear"), -1, 1)$value
  p_delta <- p_delta / probability_mass
  P_insignificant[[dataset]] <- integrate(approxfun(delta, y = p_delta, method = "linear"), -clinical_significance, clinical_significance)$value
  
  print(paste0('The probability that the difference is NOT clinically significant is ', format(round(P_insignificant[[dataset]] * 100, 2), nsmall = 2), '%'))
  
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

