source('Analysis_code/Preprocessing.R')



# Magic numbers in statistics
main_practical_significance <- 0.1
practical_significances <- c(0.01, 0.05, 0.1, 0.2, 0.5)

n_steps <- 1000
P_vector <- linspace(0., 1., n_steps)
delta_steps <- 2 * n_steps - 1
delta <- linspace(-1., 1., delta_steps)


plot_bounds <- list(
  'narrow' = list(
    'performance' = list(
      'arithmetic' = c(0.0, max_performance[['arithmetic']]),
      'episodic memory' = c(0.0, 1.0),
      'working memory' = c(0.0, 1.0),
      'stroop' = c(100, 1000),
      'simple attention' = c(100, 1000)
    ),
    'rating' = c(0, 10),
    'sleepiness' = c(0, 9)
  ),
  'wide' = list(
    'performance' = list(
      'arithmetic' = c(0.0, max_performance[['arithmetic']]),
      'episodic memory' = c(0.0, 1.0),
      'working memory' = c(0.0, 1.0),
      'stroop' = c(100, max_performance[['stroop']]),
      'simple attention' = c(100, max_performance[['simple attention']])
    ),
    'rating' = c(0, 10),
    'sleepiness' = c(0, 9)
  )
)

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

practical_significance_string <- function(practical_significance) {
  if (practical_significance == main_practical_significance) {
    path_string <- ""
  } else {
    path_string <- paste0('Varying_practical_significance/Practical_significance_',
                          format(round(practical_significance * 100, 2), nsmall = 0))
  }
  return(path_string)
}

fullAnalysis <- function(n_acc, n_inacc, outputFile, plotFolder) {
  for (i in 1:2) {
    group <- groups[i]
    frac_acc <- n_acc[[group]] / (n_acc[[group]] + n_inacc[[group]])
    printOutput(paste0(
      'In the ',
      group,
      ' group ',
      format(round(frac_acc * 100, 2), nsmall = 2),
      '% ratings were accurate'
    ),
    outputFile)
  }
  
  L_cont <- L(n_acc[['control']], n_inacc[['control']], P_vector)
  L_test <- L(n_acc[['test']], n_inacc[['test']], P_vector)
  max_L <- max(c(max(L_cont), max(L_test)))
  
  # Probability distributions over P
  png(filename = file.path(plotFolder, "Metacognitive_performance.png"))
  plot(
    c(),
    c(),
    xlab = "P^x",
    ylab = "p(P^x)",
    xlim = c(0, 1),
    ylim = c(0, max_L),
    cex = 10
  )
  lines(
    P_vector,
    L_cont,
    type = "l",
    lty = "solid",
    xaxs = "i",
    yaxs = "i",
    col = colours[['control']]
  )
  lines(
    P_vector,
    L_test,
    type = "l",
    lty = "solid",
    xaxs = "i",
    yaxs = "i",
    col = colours[['test']]
  )
  legend(
    0,
    max_L,
    legend = colour_legend,
    cex = 0.8,
    pch = 1,
    col = c(colours[['control']], colours[['test']])
  )
  dev.off()
  
  # Test - control
  p_delta <- convolve(L_test, L_cont, type = 'open')
  
  probability_mass <- integrate(approxfun(delta, y = p_delta, method = "linear"), -1, 1)$value
  p_delta <- p_delta / probability_mass
  P_significant_negative <- integrate(approxfun(delta, y = p_delta, method = "linear"),
                                      delta[1],-practical_significance)$value
  P_significant_positive <- integrate(
    approxfun(delta, y = p_delta, method = "linear"),
    practical_significance,
    tail(delta, n = 1)
  )$value
  P_significant_double <- P_significant_negative + P_significant_positive
  
  printOutput(paste0('The probability that the difference is...'),
              outputFile)
  printOutput(
    paste0(
      '   ...practically significant in the negative direction is ',
      format(round(P_significant_negative * 100, 2), nsmall = 2),
      '%'
    ),
    outputFile
  )
  printOutput(
    paste0(
      '   ...NOT practically significant in the negative direction is ',
      format(round((
        1 - P_significant_negative
      ) * 100, 2), nsmall = 2),
      '%'
    ),
    outputFile
  )
  printOutput(
    paste0(
      '   ...practically significant in the positive direction is ',
      format(round(P_significant_positive * 100, 2), nsmall = 2),
      '%'
    ),
    outputFile
  )
  printOutput(
    paste0(
      '   ...NOT practically significant in the positive direction is ',
      format(round((
        1 - P_significant_positive
      ) * 100, 2), nsmall = 2),
      '%'
    ),
    outputFile
  )
  printOutput(
    paste0(
      '   ...practically significant in either direction is ',
      format(round(P_significant_double * 100, 2), nsmall = 2),
      '%'
    ),
    outputFile
  )
  printOutput(
    paste0(
      '   ...NOT practically significant in either direction is ',
      format(round((
        1 - P_significant_double
      ) * 100, 2), nsmall = 2),
      '%'
    ),
    outputFile
  )
  
  # Plot over probability distribution over D
  png(filename = file.path(plotFolder, "Difference.png"))
  plot(
    delta,
    p_delta,
    xlab = "D",
    ylab = "p(D)",
    type = "l",
    lty = "solid",
    xlim = c(-1, 1),
    ylim = c(0, max(p_delta) * 1.1),
    xaxs = "i",
    yaxs = "i"
  )
  abline(v = practical_significance,
         col = "black",
         lty = "dashed")
  abline(
    v = -practical_significance,
    col = "black",
    lty = "dashed"
  )
  dev.off()
  printOutput('', outputFile)
}


scatterplot_scatter <- 0.005
for (n in 1:length(practical_significances)) {
  practical_significance <- practical_significances[n]
  
  dir.create(file.path(
    'Plots',
    practical_significance_string(practical_significance)
  ),
  showWarnings = FALSE)
  dir.create(file.path(
    'Text_output',
    practical_significance_string(practical_significance)
  ),
  showWarnings = FALSE)
  
  
  ggplot(sleepiness_tasks_combined, aes(x=sleepiness, fill=fill)) +
    geom_histogram(bins=10, colour="black", position="dodge") +
    scale_fill_identity() +
    labs(x = 'Sleepiness')
  
  ggsave(file.path(
    "Plots",
    "Sleepiness.png")
  )
  
  
  for (j in 1:length(split_types)) {
    split_type <- split_types[j]
    
    dir.create(file.path(
      'Plots',
      practical_significance_string(practical_significance),
      split_type
    ),
    showWarnings = FALSE)
    dir.create(file.path(
      'Text_output',
      practical_significance_string(practical_significance),
      split_type
    ),
    showWarnings = FALSE)
    
    if (split_type == 'pre-set groups') {
      colour_legend <- c('Control', 'Test')
    } else if (split_type == 'reported sleepiness') {
      colour_legend <- c('Less sleepy', 'More sleepy')
    }
    
    for (k in 1:length(median_types)) {
      median_type <- median_types[k]
      
      dir.create(file.path(
        'Text_output',
        practical_significance_string(practical_significance),
        split_type,
        median_type
      ),
      showWarnings = FALSE)
      
      dir.create(file.path(
        'Text_output',
        practical_significance_string(practical_significance),
        split_type,
        median_type,
        'Individual_tests'
      ),
      showWarnings = FALSE)
      
      dir.create(file.path(
        'Plots',
        practical_significance_string(practical_significance),
        split_type,
        median_type
      ),
      showWarnings = FALSE)
      dir.create(
        file.path(
          'Plots',
          practical_significance_string(practical_significance),
          split_type,
          median_type,
          'Aggregate'
        ),
        showWarnings = FALSE
      )
      dir.create(
        file.path(
          'Plots',
          practical_significance_string(practical_significance),
          split_type,
          median_type,
          'Individual_tests'
        ),
        showWarnings = FALSE
      )
      
      n_acc <- c('test' = 0, 'control' = 0)
      n_inacc <- c('test' = 0, 'control' = 0)
      
      n_acc_by_test <- list()
      n_inacc_by_test <- list()
      for (l in 1:length(datasets)) {
        dataset <- datasets[l]
        
        n_acc_by_test[[dataset]] <-c('test' = 0, 'control' = 0)
        n_inacc_by_test[[dataset]] <-c('test' = 0, 'control' = 0)
        
        dir.create(
          file.path(
            'Plots',
            practical_significance_string(practical_significance),
            split_type,
            median_type,
            'Individual_tests',
            dataset
          ),
          showWarnings = FALSE
        )
        
        # Session zero was a preparation test given the day before.
        for (time in 1:3) {
          for (i in 1:length(groups)) {
            group <- groups[i]
            if (median_type == 'within groups') {
              comparison <- group
            } else if (median_type == 'across groups') {
              comparison <- 'across'
            } else {
              print('Something has gone wrong')
            }
            current_median_rating <- median_rating[[dataset]][[split_type]][[median_type]][[time]][[comparison]]
            current_median_performance <- median_performance[[dataset]][[split_type]][[median_type]][[time]][[comparison]]
            
            rating_above_median <- data[[dataset]][[split_type]][[median_type]][[time]][[group]]$rating3 >= current_median_rating
            rating_below_median <- data[[dataset]][[split_type]][[median_type]][[time]][[group]]$rating3 < current_median_rating
            performance_above_median <- data[[dataset]][[split_type]][[median_type]][[time]][[group]]$performance >= current_median_performance
            performance_below_median <- data[[dataset]][[split_type]][[median_type]][[time]][[group]]$performance < current_median_performance
            # How prone were people to rate themselves accurately w.r.t. the median
            n_acc_by_test[[dataset]][[group]] <- sum(rating_above_median &
                                                       performance_above_median)[[1]] + sum(rating_below_median &
                                                                                              performance_below_median)[[1]]
            n_acc[[group]] <- n_acc[[group]] + n_acc_by_test[[dataset]][[group]]
            
            n_inacc_by_test[[dataset]][[group]] <- sum(rating_below_median &
                                                         performance_above_median)[[1]] + sum(rating_above_median &
                                                                                                performance_below_median)[[1]]
            n_inacc[[group]] <- n_inacc[[group]] + n_inacc_by_test[[dataset]][[group]]
          }
      
        }
        outputFile <- file.path('Text_output',
                                practical_significance_string(practical_significance),
                                split_type,
                                median_type,
                                'Individual_tests',
                                paste0(dataset, '.txt'))
        file.create(file.path(outputFile))
        plotFolder <- file.path("Plots",
                                practical_significance_string(practical_significance),
                                split_type,
                                median_type,
                                'Individual_tests',
                                dataset)
        fullAnalysis(n_acc_by_test[[dataset]],
                     n_inacc_by_test[[dataset]],
                     outputFile,
                     plotFolder)
        
        # Histograms across real performance, self-rated performance and sleepiness
        hist_targets = c('performance', 'rating', 'sleepiness')
        hist_names = list(
          'performance' = 'Actual_performance',
          'rating' = 'Self-rated performance',
          'sleepiness' = 'Sleepiness'
        )
        hist_test = list(
          'performance' = data_sessions_combined[[dataset]][[split_type]][[median_type]][['test']]$performance,
          'rating' = data_sessions_combined[[dataset]][[split_type]][[median_type]][['test']]$rating3,
          'sleepiness' = data_sessions_combined[[dataset]][[split_type]][[median_type]][['test']]$rating1
        )
        hist_control = list(
          'performance' = data_sessions_combined[[dataset]][[split_type]][[median_type]][['control']]$performance,
          'rating' = data_sessions_combined[[dataset]][[split_type]][[median_type]][['control']]$rating3,
          'sleepiness' = data_sessions_combined[[dataset]][[split_type]][[median_type]][['control']]$rating1
        )
        hist_bounds = list(
          'performance' = plot_bounds[['wide']][['performance']][[dataset]],
          'rating' = plot_bounds[['wide']][['rating']],
          'sleepiness' = plot_bounds[['wide']][['sleepiness']]
          
        )
        for (t in 1:3) {
          target = hist_targets[t]
          
          hist_df <- rbind(data.frame(fill=colours[['test']],
                                      obs=hist_test[[target]]),
                           data.frame(fill=colours[['control']],
                                      obs=hist_control[[target]]))
          ggplot(hist_df, aes(x=obs, fill=fill)) +
            geom_histogram(bins=10, colour="black", position="dodge") +
            scale_fill_identity() +
            labs(x = hist_names[[target]])
          
          ggsave(file.path(plotFolder, paste0(hist_names[[target]], ".png"))
          )
        }
        
        # Scatterplots across real performance and either self-rated performance or sleepiness
        xlab <- c('Self-rated performance', 'Sleepiness')
        xbounds <- c(list(plot_bounds[['narrow']][['rating']]), list(plot_bounds[['narrow']][['sleepiness']]))
        xdata <- c('rating3', 'rating1')
        for (x in 1:2) {
          png(filename = file.path(plotFolder,paste0("Actual_performance_", xlab[x], ".png")))
          plot(
            c(),
            c(),
            main = str_to_title(dataset),
            ylab = "Actual performance",
            xlab = xlab[x],
            ylim = plot_bounds[['narrow']][['performance']][[dataset]],
            xlim = xbounds[[x]],
            cex = 10
          )
          for (time in 1:3) {
            y_range <- plot_bounds[['narrow']][['performance']][[dataset]][2] - plot_bounds[['narrow']][['performance']][[dataset]][1]
            x_min <- xbounds[[x]][1]
            x_range <- xbounds[[x]][2] - x_min
            
            for (i in 1:2) {
              group <- groups[i]
              performance <- data[[dataset]][[split_type]][[median_type]][[time]][[group]]$performance
              performance <- performance + rnorm(
                length(performance),
                mean = 0,
                sd = y_range * scatterplot_scatter
              )
              
              rating <- data[[dataset]][[split_type]][[median_type]][[time]][[group]][[xdata[x]]]
              rating <- rating + rnorm(length(rating),
                                       mean = 0,
                                       sd = x_range * scatterplot_scatter)
              
              points(
                rating,
                performance,
                cex = 0.1,
                pch = 1,
                col = c(colours[[group]])
              )
            }
          }
          y_min <- plot_bounds[['narrow']][['performance']][[dataset]][1]
          y_range <- plot_bounds[['narrow']][['performance']][[dataset]][2] - y_min
          legend(
            xbounds[[x]][1],
            y_min + 0.1 * y_range,
            legend = colour_legend,
            cex = 0.8,
            pch = 1,
            col = c(colours[['control']], colours[['test']])
          )
          dev.off()
        }
      }
      
      outputFile <- file.path('Text_output',
                              practical_significance_string(practical_significance),
                              split_type,
                              median_type,
                              paste0('Aggregate.txt'))
      file.create(file.path(outputFile))
      plotFolder <- file.path("Plots",
                              practical_significance_string(practical_significance),
                              split_type,
                              median_type,
                              "Aggregate")
      fullAnalysis(n_acc, n_inacc, outputFile, plotFolder)
    }
  }
}