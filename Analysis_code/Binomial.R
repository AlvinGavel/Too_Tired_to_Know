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
colours <- c('test' = "#FF0033",
             'control' = "#0000FF")

performance_bounds <- list('arithmetic' = c(0.0,1.0),
                           'episodic memory' = c(0.0,1.0),
                           'working memory' = c(0.0,1.0),
                           'stroop' = c(0.0,1.0),
                           'simple attention' = c(100,1000))

rating_bounds <- c(0,10)
sleepiness_bounds <- c(0, 9)
scatterplot_scatter <- 0.005

P_significant_negative <- c()
P_significant_positive <- c()
P_significant_double <- c()
for (i in 1:length(datasets)) {
  print(paste0('For ', dataset, ':'))
  dataset <- datasets[i]
  
  for (k in 1:2) {
    split_type <- split_types[k]
    print(paste0('   Splitting by ', split_type))
    if (split_type == 'pre-set groups') {
      colour_legend = c('Control', 'Test')
    } else if (split_type == 'reported sleepiness') {
      colour_legend = c('Less sleepy', 'More sleepy')
    }
    
    n_acc <- c('test' = 0, 'control' = 0)
    n_inacc <- c('test' = 0, 'control' = 0)
    for (time in 1:3) {
      for (i in 1:2) {
        group = groups[i]
        rating_above_median <- data[[dataset]][[split_type]][[group]][[time]]$rating3 >= median_rating[[dataset]][[split_type]][[group]][[time]]
        rating_below_median <- data[[dataset]][[split_type]][[group]][[time]]$rating3 < median_rating[[dataset]][[split_type]][[group]][[time]]
        performance_above_median <- data[[dataset]][[split_type]][[group]][[time]]$performance >= median_performance[[dataset]][[split_type]][[group]][[time]]
        performance_below_median <- data[[dataset]][[split_type]][[group]][[time]]$performance < median_performance[[dataset]][[split_type]][[group]][[time]]
        # How prone were people to rate themselves accurately w.r.t. the median
        n_acc[[group]] <- n_acc[[group]] + sum(rating_above_median & performance_above_median)[[1]] + sum(rating_below_median & performance_below_median)[[1]]
        n_inacc[[group]] <- n_inacc[[group]] + sum(rating_below_median & performance_above_median)[[1]] + sum(rating_above_median & performance_below_median)[[1]]
      }
    }
    
    ylab = c('Self-rated performance', 'Sleepiness')
    ybounds = c(list(rating_bounds), list(sleepiness_bounds))
    ydata = c('rating3', 'rating1')
    for (y in 1:2) {
      png(filename=file.path("Plots", dataset, split_type,  paste0("Actual_performance_", ylab[y], ".png")))
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
        
        for (i in 1:2) {
          group = groups[i]
          performance <- data[[dataset]][[split_type]][[group]][[time]]$performance
          performance <- performance + rnorm(length(performance),
                                             mean=0,
                                             sd=x_range * scatterplot_scatter)
          
          rating <- data[[dataset]][[split_type]][[group]][[time]][[ydata[y]]]
          rating <- rating + rnorm(length(rating),
                                   mean=0,
                                   sd=y_range * scatterplot_scatter)
          
          points(performance,
                 rating,
                 cex=0.1,
                 pch=1,
                 col=c(colours[[group]]))
        }
      }
      legend(performance_bounds[[dataset]][1],
             y_min + 0.1 * y_range,
             legend=colour_legend,
             cex=0.8,
             pch=1,
             col=c(colours[['control']], colours[['test']]))
      dev.off()
    }
    
    for (i in 1:2) {
      group = groups[i]
      frac_acc <- n_acc[[group]] / (n_acc[[group]] + n_inacc[[group]])
      print(paste0('      In the ', group, ' group ', format(round(frac_acc * 100, 2), nsmall = 2), '% rated themselves accurately'))
    }
    
    L_cont <- L(n_acc[['control']], n_inacc[['control']], P_vector)
    L_test <- L(n_acc[['test']], n_inacc[['test']], P_vector)
    max_L = max(c(max(L_cont), max(L_test)))
    
    png(filename=file.path("Plots", dataset, split_type, "Metacognitive_performance.png"))
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
          col=colours[['control']])
    lines(P_vector,
          L_test,
          type="l",
          lty = "solid",
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
    P_significant_negative[[dataset]] <- integrate(approxfun(delta, y = p_delta, method = "linear"), delta[1], -clinical_significance)$value
    P_significant_positive[[dataset]] <- integrate(approxfun(delta, y = p_delta, method = "linear"), clinical_significance, tail(delta, n=1))$value
    P_significant_double[[dataset]] <- P_significant_negative[[dataset]] + P_significant_positive[[dataset]]
    
    print(paste0('      The probability that the difference is clinically significant in the negative direction is ', format(round(P_significant_negative[[dataset]] * 100, 2), nsmall = 2), '%'))
    print(paste0('      The probability that the difference is NOT clinically significant in the negative direction is ', format(round((1 - P_significant_negative[[dataset]]) * 100, 2), nsmall = 2), '%'))
    print(paste0('      The probability that the difference is clinically significant in the positive direction is ', format(round(P_significant_positive[[dataset]] * 100, 2), nsmall = 2), '%'))
    print(paste0('      The probability that the difference is NOT clinically significant in the positive direction is ', format(round((1 - P_significant_positive[[dataset]]) * 100, 2), nsmall = 2), '%'))
    print(paste0('      The probability that the difference is clinically significant in either direction is ', format(round(P_significant_double[[dataset]] * 100, 2), nsmall = 2), '%'))
    print(paste0('      The probability that the difference is NOT clinically significant in either direction is ', format(round((1 - P_significant_double[[dataset]]) * 100, 2), nsmall = 2), '%'))


    png(filename=file.path("Plots", dataset, split_type, "Difference.png"))
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
}


