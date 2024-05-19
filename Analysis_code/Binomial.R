source('Analysis_code/Preprocessing.R')

# Function definitions
logB <- function(alpha, beta) {
  return(lgamma(alpha) + lgamma(beta) - lgamma(alpha + beta))
}

L <- function(n_above, n_below, P_vector) {
  log_L <- n_above * log(P_vector) +
    n_below * log(1 - P_vector) -
    logB(n_above + 1, n_below + 1)
  return(exp(log_L))
}

outputFile <- file.path('Text_output', "Binomial.txt")
file.create(outputFile)

# Magic numbers in statistics
practical_significance <- 0.1

n_steps <- 1000
P_vector <- linspace(0., 1., n_steps)
delta_steps <- 2 * n_steps - 1
delta <- linspace(-1., 1., delta_steps)

# Parameters in plotting
colours <- c('test' = "#FF0033",
             'control' = "#0000FF")

performance_bounds <- list('arithmetic' = c(0.0, max_performance[['arithmetic']]),
                           'episodic memory' = c(0.0, 1.0),
                           'working memory' = c(0.0, 1.0),
                           'stroop' = c(100, 1000),
                           'simple attention' = c(100, 1000))

rating_bounds <- c(0, 10)
sleepiness_bounds <- c(0, 9)
scatterplot_scatter <- 0.005
for (k in 1:2) {
  split_type <- split_types[k]
  printOutput(paste0('Splitting by ', split_type), outputFile)
  if (split_type == 'pre-set groups') {
    colour_legend <- c('Control', 'Test')
  } else if (split_type == 'reported sleepiness') {
    colour_legend <- c('Less sleepy', 'More sleepy')
  }
  
  n_acc <- c('test' = 0, 'control' = 0)
  n_inacc <- c('test' = 0, 'control' = 0)
  for (i in 1:length(datasets)) {
    dataset <- datasets[i]
    
    # Session zero was a preparation test given the day before.
    for (time in 1:3) {
      for (i in 1:2) {
        group <- groups[i]
        rating_above_median <- data[[dataset]][[split_type]][[group]][[time]]$rating3 >= median_rating[[dataset]][[split_type]][[group]][[time]]
        rating_below_median <- data[[dataset]][[split_type]][[group]][[time]]$rating3 < median_rating[[dataset]][[split_type]][[group]][[time]]
        performance_above_median <- data[[dataset]][[split_type]][[group]][[time]]$performance >= median_performance[[dataset]][[split_type]][[group]][[time]]
        performance_below_median <- data[[dataset]][[split_type]][[group]][[time]]$performance < median_performance[[dataset]][[split_type]][[group]][[time]]
        # How prone were people to rate themselves accurately w.r.t. the median
        
        n_acc[[group]] <- n_acc[[group]] + sum(rating_above_median & performance_above_median)[[1]] + sum(rating_below_median & performance_below_median)[[1]]
        n_inacc[[group]] <- n_inacc[[group]] + sum(rating_below_median & performance_above_median)[[1]] + sum(rating_above_median & performance_below_median)[[1]]
      }
    }
    
    
    # Scatterplots across real performance and either self-rated performance or sleepiness
    xlab <- c('Self-rated performance', 'Sleepiness')
    xbounds <- c(list(rating_bounds), list(sleepiness_bounds))
    xdata <- c('rating3', 'rating1')
    for (x in 1:2) {
      png(filename=file.path("Plots", split_type, "Individual_tests", dataset, paste0("Actual_performance_", xlab[x], ".png")))
      plot(c(),
           c(),
           ylab="Actual performance",
           xlab=xlab[x],
           ylim=performance_bounds[[dataset]],
           xlim=xbounds[[x]],
           cex=10)
      for (time in 1:3) {
        y_range <- performance_bounds[[dataset]][2] - performance_bounds[[dataset]][1]
        x_min <- xbounds[[x]][1]
        x_range <- xbounds[[x]][2] - x_min
        
        for (i in 1:2) {
          group <- groups[i]
          performance <- data[[dataset]][[split_type]][[group]][[time]]$performance
          performance <- performance + rnorm(length(performance),
                                             mean=0,
                                             sd=y_range * scatterplot_scatter)
          
          rating <- data[[dataset]][[split_type]][[group]][[time]][[xdata[x]]]
          rating <- rating + rnorm(length(rating),
                                   mean=0,
                                   sd=x_range * scatterplot_scatter)
          
          points(rating,
                 performance,
                 cex=0.1,
                 pch=1,
                 col=c(colours[[group]]))
        }
      }
      y_min <- performance_bounds[[dataset]][1]
      y_range <- performance_bounds[[dataset]][2] - y_min
      legend(xbounds[[x]][1],
             y_min + 0.1 * y_range,
             legend=colour_legend,
             cex=0.8,
             pch=1,
             col=c(colours[['control']], colours[['test']]))
      dev.off()
    }
  }
  
  for (i in 1:2) {
    group <- groups[i]
    frac_acc <- n_acc[[group]] / (n_acc[[group]] + n_inacc[[group]])
    printOutput(paste0('In the ', group, ' group ', format(round(frac_acc * 100, 2), nsmall = 2), '% ratings were accurate'), outputFile)
  }
  
  L_cont <- L(n_acc[['control']], n_inacc[['control']], P_vector)
  L_test <- L(n_acc[['test']], n_inacc[['test']], P_vector)
  max_L <- max(c(max(L_cont), max(L_test)))
  
  # Probability distributions over P
  png(filename=file.path("Plots", split_type, "Aggregate", "Metacognitive_performance.png"))
  plot(c(),
       c(),
       xlab="P^x",
       ylab="p(P^x)",
       xlim=c(0, 1),
       ylim=c(0, max_L),
       cex=10)
  lines(P_vector,
        L_cont,
        type="l",
        lty="solid",
        xaxs="i",
        yaxs="i",
        col=colours[['control']])
  lines(P_vector,
        L_test,
        type="l",
        lty="solid",
        xaxs="i",
        yaxs="i",
        col=colours[['test']])
  
  legend(0,
         max_L,
         legend=colour_legend,
         cex=0.8,
         pch=1,
         col=c(colours[['control']], colours[['test']]))
  dev.off()
  
  # Test - control
  p_delta <- convolve(L_test, L_cont, type = 'open')
  
  probability_mass <- integrate(approxfun(delta, y = p_delta, method = "linear"), -1, 1)$value
  p_delta <- p_delta / probability_mass
  P_significant_negative <- integrate(approxfun(delta, y = p_delta, method = "linear"), delta[1], -practical_significance)$value
  P_significant_positive <- integrate(approxfun(delta, y = p_delta, method = "linear"), practical_significance, tail(delta, n=1))$value
  P_significant_double <- P_significant_negative + P_significant_positive
  
  printOutput(paste0('The probability that the difference is...'), outputFile)
  printOutput(paste0('   ...practically significant in the negative direction is ', format(round(P_significant_negative * 100, 2), nsmall = 2), '%'), outputFile)
  printOutput(paste0('   ...NOT practically significant in the negative direction is ', format(round((1 - P_significant_negative) * 100, 2), nsmall = 2), '%'), outputFile)
  printOutput(paste0('   ...practically significant in the positive direction is ', format(round(P_significant_positive * 100, 2), nsmall = 2), '%'), outputFile)
  printOutput(paste0('   ...NOT practically significant in the positive direction is ', format(round((1 - P_significant_positive) * 100, 2), nsmall = 2), '%'), outputFile)
  printOutput(paste0('   ...practically significant in either direction is ', format(round(P_significant_double * 100, 2), nsmall = 2), '%'), outputFile)
  printOutput(paste0('   ...NOT practically significant in either direction is ', format(round((1 - P_significant_double) * 100, 2), nsmall = 2), '%'), outputFile)
  
  # Plot over probability distribution over D
  png(filename=file.path("Plots", split_type, "Aggregate", "Difference.png"))
  plot(delta,
       p_delta,
       xlab="D",
       ylab="p(D)",
       type="l",
       lty = "solid",
       xlim=c(-1, 1),
       ylim =c(0, max(p_delta) * 1.1),
       xaxs="i",
       yaxs="i")
  abline(v=practical_significance, col="black", lty = "dashed")
  abline(v=-practical_significance, col="black", lty = "dashed")
  dev.off()
}
printOutput('', outputFile)

