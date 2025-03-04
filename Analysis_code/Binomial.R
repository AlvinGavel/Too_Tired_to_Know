source('Analysis_code/Preprocessing.R')
source('Analysis_code/Split_violin.R')


# Magic numbers in statistics
credible_interval_low_bound <- 0.05
credible_interval_high_bound <- 0.95

main_practical_significance <- 0.1
practical_significances <- c(0.1, 0.01, 0.05, 0.2, 0.5)

rating_practical_significance <- 0.05

scatterplot_scatter <- 0.05

n_steps <- 1000
P_vector <- linspace(0., 1., n_steps)
delta_steps <- 2 * n_steps - 1
delta <- linspace(-1., 1., delta_steps)


plot_bounds <- list(
  'narrow' = list(
    'performance' = list(
      'throughput' = c(0.0, max_performance[['throughput']]),
      'episodic memory' = c(0.0, 1.0),
      'working memory' = c(0.0, 1.0),
      'executive processing' = c(100, 1000),
      'simple attention' = c(100, 1000)
    ),
    'rating' = c(1, 9),
    'sleepiness' = c(1, 9)
  ),
  'wide' = list(
    'performance' = list(
      'throughput' = c(0.0, max_performance[['throughput']]),
      'episodic memory' = c(0.0, 1.0),
      'working memory' = c(0.0, 1.0),
      'executive processing' = c(100, max_performance[['executive processing']]),
      'simple attention' = c(100, max_performance[['simple attention']])
    ),
    'rating' = c(1, 9),
    'sleepiness' = c(1, 9)
  )
)

performance_meaning = c('throughput' = 'Correct responses / minute',
                        'episodic memory' = 'Frac. correct responses',
                        'working memory' = 'Frac. correct responses',
                        'executive processing' = 'Mean reaction time [ms]',
                        'simple attention' = 'Mean reaction time [ms]')

# Function definitions
logB <- function(alpha, beta) {
  return(lgamma(alpha) + lgamma(beta) - lgamma(alpha + beta))
}

L <- function(n_acc, n_inacc, P_vector) {
  log_L <- n_acc * log(P_vector) +
    n_inacc * log(1 - P_vector) -
    logB(n_acc + 1, n_inacc + 1)
  if (n_acc == 0) {
    log_L[1] <- - logB(n_acc + 1, n_inacc + 1)
  }
  if (n_inacc == 0) {
    log_L[length(log_L)] <- - logB(n_acc + 1, n_inacc + 1)
  }
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

plotLegend <- function(plotFolder, split_type) {
  if (split_type == 'pre-set groups') {
    colour_legend <- c('Well-rested' = 'Well-rested',
                       'Sleep-deprived' = 'Sleep-deprived')
  } else if (split_type == 'reported sleepiness') {
    colour_legend <- c('Well-rested' = 'Less sleepy',
                       'Sleep-deprived' = 'More sleepy')
  }
  
  tiff(filename = file.path(plotFolder, "Legend.tiff"), width=2400, height=2400, units="px", res=300)
  plot(NULL, axes = FALSE, xlab = "", ylab = "", xlim = c(0, 8), ylim = c(0, 6))
  legend(
    0,
    6,
    legend = c(colour_legend[['Well-rested']], colour_legend[['Sleep-deprived']]),
    text.col = c(colours[['Well-rested']], colours[['Sleep-deprived']]),
    cex = 2,
    pch = NULL
  )
  dev.off()
}

fullAnalysis <- function(n_acc, n_inacc, practical_significance, outputFile, plotFolder) {
  for (i in 1:2) {
    group <- groups[i]
    total <- n_acc[[group]] + n_inacc[[group]]
    # Have a look at this?!
    if (total > 0) {
      frac_acc <- n_acc[[group]] / total
      printOutput(paste0(
        'In the ',
        group,
        ' group ',
        format(round(frac_acc * 100, 2), nsmall = 2),
        '% of ratings were accurate'
      ),
      outputFile)} else {
        printOutput(paste0(
          'The ',
          group,
          ' is empty'
        ),
        outputFile)
      }
  }
  
  pP <- list('Well-rested' = L(n_acc[['Well-rested']], n_inacc[['Well-rested']], P_vector),
             'Sleep-deprived' = L(n_acc[['Sleep-deprived']], n_inacc[['Sleep-deprived']], P_vector)
  )
  max_pP <- max(c(max(pP[['Well-rested']]), max(pP[['Sleep-deprived']])))
  
  # Find credible intervals
  for (i in 1:2) {
    group <- groups[i]
    pP_integral <- function(right_bound) {
      return(integrate(approxfun(P_vector, y = pP[[group]], method = "linear"), 0.0, right_bound)$value)
    }
    pP_root_low <- function(right_bound) {
      return(pP_integral(right_bound) - credible_interval_low_bound)
    }
    pP_root_high <- function(right_bound) {
      return(pP_integral(right_bound) - credible_interval_high_bound)
    }
    pP_percentile_low <- pracma::bisect(pP_root_low, 0.0, 1.0)
    pP_percentile_high <- pracma::bisect(pP_root_high, 0.0, 1.0)
    
    printOutput(
      paste0(
        '   ',
        format(round((credible_interval_high_bound - credible_interval_low_bound) * 100, 0), nsmall = 2),
        '% of the P probability mass for the ',
        group,
        ' group is in the interval ',
        format(round(pP_percentile_low$root, 2), nsmall = 2),
        ' to ',
        format(round(pP_percentile_high$root, 2), nsmall = 2)
      ),
      outputFile
    )
  }
  
  # Plot probability distributions over P
  tiff(filename = file.path(plotFolder, "Metacognitive_performance.tiff"), width=2400, height=2400, units="px", res=300)
  par(mar=c(5,6,1,1) + 0.1)
  plot(
    c(),
    c(),
    xlab = TeX('$P^x$'),
    ylab = TeX('$p\\left( P^x \\right)$'),
    xlim = c(0, 1),
    ylim = c(0, max_pP),
    cex.lab = 2,
    cex.main = 2,
    cex.axis = 2,
    cex.sub = 2
  )
  for (i in 1:2) {
    group <- groups[i]
    lines(
      P_vector,
      pP[[group]],
      type = "l",
      lty = "solid",
      xaxs = "i",
      yaxs = "i",
      col = colours[[group]]
    )
  }
  dev.off()
  
  # Test - control
  p_delta <- convolve(pP[['Sleep-deprived']], pP[['Well-rested']], type = 'open')
  
  probability_mass <- integrate(
    approxfun(delta, y = p_delta, method = "linear"),
    -1,
    1
  )$value
  p_delta <- p_delta / probability_mass
  P_significant_negative <- integrate(
    approxfun(delta, y = p_delta, method = "linear"),
    -1,
    -practical_significance
  )$value
  P_significant_positive <- integrate(
    approxfun(delta, y = p_delta, method = "linear"),
    practical_significance,
    1
  )$value
  P_significant_double <- P_significant_negative + P_significant_positive
  
  # Find credible interval
  delta_integral <- function(right_bound) {
    return(integrate(approxfun(delta, y = p_delta, method = "linear"), -1, right_bound)$value)
  }
  delta_root_low <- function(right_bound) {
    return(delta_integral(right_bound) - credible_interval_low_bound)
  }
  delta_root_high <- function(right_bound) {
    return(delta_integral(right_bound) - credible_interval_high_bound)
  }
  delta_percentile_low <- pracma::bisect(delta_root_low, -1.0, 1.0)
  delta_percentile_high <- pracma::bisect(delta_root_high, -1.0, 1.0)
  
  printOutput(
    paste0(
      '   ',
      format(round((credible_interval_high_bound - credible_interval_low_bound) * 100, 0), nsmall = 2),
      '% of the delta probability mass is in the interval ',
      format(round(delta_percentile_low$root, 2), nsmall = 2),
      ' to ',
      format(round(delta_percentile_high$root, 2), nsmall = 2)
    ),
    outputFile
  )
  
  # Calculate probability of practically significant difference
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
  tiff(filename = file.path(plotFolder, "Difference.tiff"), width=2400, height=2400, units="px",res=300)
  par(mar=c(5,6,1,1) + 0.1)
  plot(
    delta,
    p_delta,
    xlab = TeX("$D$"),
    ylab = TeX("$p\\left( D \\right)$"),
    type = "l",
    lty = "solid",
    xlim = c(-1, 1),
    ylim = c(0, max(p_delta) * 1.1),
    xaxs = "i",
    yaxs = "i",
    cex.lab = 2,
    cex.main = 2,
    cex.axis = 2,
    cex.sub = 2
  )
  abline(v = practical_significance,
         col = "black",
         lty = "dashed")
  abline(v = -practical_significance,
         col = "black",
         lty = "dashed"
  )
  dev.off()
  printOutput('', outputFile)
}

ratingAnalysis <- function(n_above, n_below, practical_significance, outputFile, plotFolder) {
  for (i in 1:2) {
    group <- groups[i]
    total <- n_above[[group]] + n_below[[group]]
    # Have a look at this?!
    if (total > 0) {
      frac_above <- n_above[[group]] / total
      printOutput(paste0(
        'In the ',
        group,
        ' group ',
        format(round(frac_above * 100, 2), nsmall = 2),
        '% rated themselves above the common median'
      ),
      outputFile)} else {
        printOutput(paste0(
          'The ',
          group,
          ' is empty'
        ),
        outputFile)
      }
  }
  
  pP <- list('Well-rested' = L(n_above[['Well-rested']], n_below[['Well-rested']], P_vector),
             'Sleep-deprived' = L(n_above[['Sleep-deprived']], n_below[['Sleep-deprived']], P_vector)
  )
  max_pP <- max(c(max(pP[['Well-rested']]), max(pP[['Sleep-deprived']])))
  
  # Find credible intervals
  for (i in 1:2) {
    group <- groups[i]
    pP_integral <- function(right_bound) {
      return(integrate(approxfun(P_vector, y = pP[[group]], method = "linear"), 0.0, right_bound)$value)
    }
    pP_root_low <- function(right_bound) {
      return(pP_integral(right_bound) - credible_interval_low_bound)
    }
    pP_root_high <- function(right_bound) {
      return(pP_integral(right_bound) - credible_interval_high_bound)
    }
    pP_percentile_low <- pracma::bisect(pP_root_low, 0.0, 1.0)
    pP_percentile_high <- pracma::bisect(pP_root_high, 0.0, 1.0)
    
    printOutput(
      paste0(
        '   ',
        format(round((credible_interval_high_bound - credible_interval_low_bound) * 100, 0), nsmall = 2),
        '% of the P probability mass for the ',
        group,
        ' group is in the interval ',
        format(round(pP_percentile_low$root, 2), nsmall = 2),
        ' to ',
        format(round(pP_percentile_high$root, 2), nsmall = 2)
      ),
      outputFile
    )
  }
  
  # Plot probability distributions over P
  tiff(filename = file.path(plotFolder, "Rating.tiff"), width=2400, height=2400, units="px", res=300)
  par(mar=c(5,6,1,1) + 0.1)
  plot(
    c(),
    c(),
    xlab = TeX('$P^{x}_{rat.}$'),
    ylab = TeX('$p\\left( P^x_{rat.} \\right)$'),
    xlim = c(0, 1),
    ylim = c(0, max_pP),
    cex.lab = 2,
    cex.main = 2,
    cex.axis = 2,
    cex.sub = 2
  )
  for (i in 1:2) {
    group <- groups[i]
    lines(
      P_vector,
      pP[[group]],
      type = "l",
      lty = "solid",
      xaxs = "i",
      yaxs = "i",
      col = colours[[group]]
    )
  }
  dev.off()
  
  # Test - control
  p_delta <- convolve(pP[['Sleep-deprived']], pP[['Well-rested']], type = 'open')
  
  probability_mass <- integrate(
    approxfun(delta, y = p_delta, method = "linear"),
    -1,
    1
  )$value
  p_delta <- p_delta / probability_mass
  P_significant_negative <- integrate(
    approxfun(delta, y = p_delta, method = "linear"),
    -1,
    -practical_significance
  )$value
  P_significant_positive <- integrate(
    approxfun(delta, y = p_delta, method = "linear"),
    practical_significance,
    1
  )$value
  P_significant_double <- P_significant_negative + P_significant_positive
  
  # Find credible interval
  delta_integral <- function(right_bound) {
    return(integrate(approxfun(delta, y = p_delta, method = "linear"), -1, right_bound)$value)
  }
  delta_root_low <- function(right_bound) {
    return(delta_integral(right_bound) - credible_interval_low_bound)
  }
  delta_root_high <- function(right_bound) {
    return(delta_integral(right_bound) - credible_interval_high_bound)
  }
  delta_percentile_low <- pracma::bisect(delta_root_low, -1.0, 1.0)
  delta_percentile_high <- pracma::bisect(delta_root_high, -1.0, 1.0)
  
  printOutput(
    paste0(
      '   ',
      format(round((credible_interval_high_bound - credible_interval_low_bound) * 100, 0), nsmall = 2),
      '% of the delta probability mass is in the interval ',
      format(round(delta_percentile_low$root, 2), nsmall = 2),
      ' to ',
      format(round(delta_percentile_high$root, 2), nsmall = 2)
    ),
    outputFile
  )
  
  # Calculate probability of practically significant difference
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
  tiff(filename = file.path(plotFolder, "Difference_in_rating.tiff"), width=2400, height=2400, units="px", res=300)
  par(mar=c(5,6,1,1) + 0.1)
  plot(
    delta,
    p_delta,
    xlab = TeX("$D$"),
    ylab = TeX("$p\\left( D_{rat.} \\right)$"),
    type = "l",
    lty = "solid",
    xlim = c(-1, 1),
    ylim = c(0, max(p_delta) * 1.1),
    xaxs = "i",
    yaxs = "i",
    cex.lab = 2,
    cex.main = 2,
    cex.axis = 2,
    cex.sub = 2
  )
  abline(v = practical_significance,
         col = "black",
         lty = "dashed")
  abline(v = -practical_significance,
         col = "black",
         lty = "dashed"
  )
  dev.off()
  printOutput('', outputFile)
}


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
  
  # Sleepiness over all tasks combined
  ggplot(sleepiness_tasks_combined, aes(x=sleepiness, fill=fill)) +
    geom_bar(colour="black", position=position_dodge2(preserve = "single")) +
    scale_fill_identity() +
    labs(x = 'Sleepiness', y = 'Counts') +
    scale_x_continuous(name = 'Sleepiness',
                       breaks = seq(1, 9, by=1)) +
    theme(axis.title=element_text(size=20),
          axis.text.x=element_text(size=20),
          axis.text.y=element_text(size=20))
  ggsave(file.path("Plots", "Sleepiness.tiff"), device = 'tiff', width=2400, height=2400, units="px", dpi=300)
  
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
      
      n_acc <- c('Sleep-deprived' = 0, 'Well-rested' = 0)
      n_inacc <- c('Sleep-deprived' = 0, 'Well-rested' = 0)
      
      n_acc_by_test <- list()
      n_inacc_by_test <- list()
      for (l in 1:length(datasets)) {
        dataset <- datasets[l]
        
        n_acc_by_test[[dataset]] <-c('Sleep-deprived' = 0, 'Well-rested' = 0)
        n_inacc_by_test[[dataset]] <-c('Sleep-deprived' = 0, 'Well-rested' = 0)
        
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
            current_median_rating <- median_rating[[dataset]][[split_type]][[time]][[comparison]]
            current_median_performance <- median_performance[[dataset]][[split_type]][[time]][[comparison]]
            
            rating_above_median <- data[[dataset]][[split_type]][[time]][[group]]$rating3 >= current_median_rating
            rating_below_median <- data[[dataset]][[split_type]][[time]][[group]]$rating3 < current_median_rating
            performance_above_median <- data[[dataset]][[split_type]][[time]][[group]]$performance >= current_median_performance
            performance_below_median <- data[[dataset]][[split_type]][[time]][[group]]$performance < current_median_performance
            # How prone were people to rate themselves accurately w.r.t. the median
            n_acc_this_test_this_session <- sum(rating_above_median &
                                                  performance_above_median)[[1]] + sum(rating_below_median &
                                                                                         performance_below_median)[[1]]
            
            n_inacc_this_test_this_session <- sum(rating_below_median &
                                                    performance_above_median)[[1]] + sum(rating_above_median &
                                                                                           performance_below_median)[[1]]
            
            n_acc_by_test[[dataset]][[group]] <- n_acc_by_test[[dataset]][[group]] + n_acc_this_test_this_session
            n_acc[[group]] <- n_acc[[group]] + n_acc_this_test_this_session
            
            n_inacc_by_test[[dataset]][[group]] <- n_inacc_by_test[[dataset]][[group]] + n_inacc_this_test_this_session
            n_inacc[[group]] <- n_inacc[[group]] + n_inacc_this_test_this_session
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
        plotLegend(plotFolder, split_type)
        fullAnalysis(n_acc_by_test[[dataset]],
                     n_inacc_by_test[[dataset]],
                     practical_significance,
                     outputFile,
                     plotFolder)
        
        # Bar plots across self-rated performance and sleepiness
        bar_targets = c('rating', 'sleepiness')
        
        bar_names = list(
          'rating' = 'Self-rated performance',
          'sleepiness' = 'Sleepiness'
        )
        bar_test = list(
          'rating' = data_sessions_combined[[dataset]][[split_type]][['Sleep-deprived']]$rating3,
          'sleepiness' = data_sessions_combined[[dataset]][[split_type]][['Sleep-deprived']]$rating1
        )
        bar_control = list(
          'rating' = data_sessions_combined[[dataset]][[split_type]][['Well-rested']]$rating3,
          'sleepiness' = data_sessions_combined[[dataset]][[split_type]][['Well-rested']]$rating1
        )
        bar_bounds = list(
          'rating' = plot_bounds[['wide']][['rating']],
          'sleepiness' = plot_bounds[['wide']][['sleepiness']]
        )
        for (t in 1:2) {
          target = bar_targets[t]
          
          bar_df <- rbind(data.frame(fill=colours[['Sleep-deprived']],
                                     obs=bar_test[[target]]),
                          data.frame(fill=colours[['Well-rested']],
                                     obs=bar_control[[target]]))
          ggplot(bar_df, aes(x=obs, fill=fill)) +
            geom_bar(colour="black", position=position_dodge2(preserve = 'single')) +
            scale_fill_identity() +
            theme(text = element_text(size = 25)) +
            scale_x_continuous(name = bar_names[[target]],
                               breaks = seq(1, 9, by=1)) +
            ylab('Counts') +
            ggtitle(str_to_title(dataset))
          
          ggsave(file.path(plotFolder, paste0(bar_names[[target]], '.tiff')), width=2400, height=2400, units="px", dpi=300)
        }
        
        # Histogram across real performance
        hist_target = 'performance'
        hist_test = data_sessions_combined[[dataset]][[split_type]][['Sleep-deprived']]$performance
        hist_control = data_sessions_combined[[dataset]][[split_type]][['Well-rested']]$performance
        hist_bounds = plot_bounds[['wide']][['performance']][[dataset]]
        
        hist_df <- rbind(data.frame(fill=colours[['Sleep-deprived']],
                                    obs=hist_test),
                         data.frame(fill=colours[['Well-rested']],
                                    obs=hist_control))
        ggplot(bar_df, aes(x=obs, fill=fill)) +
          geom_histogram(bins=9, colour="black", position=position_dodge2(preserve = 'single')) +
          scale_fill_identity() +
          theme(text = element_text(size = 25)) +
          ggtitle(str_to_title(dataset)) +
          scale_x_continuous(name = paste0("Actual performance\n(", performance_meaning[[dataset]], ")"),
                             breaks = seq(1, 9, by=1)) +
          ylab('Counts') +
          ggtitle(str_to_title(dataset))
        
        
        ggsave(file.path(plotFolder, paste0('Actual_performance.tiff')), width=2400, height=2400, units="px", dpi=300)
        
        
        # Scatterplots across real performance and either self-rated performance or sleepiness
        xlab <- c('Self-rated performance', 'Sleepiness')
        xbounds <- c(list(plot_bounds[['narrow']][['rating']]), list(plot_bounds[['narrow']][['sleepiness']]))
        xdata <- c('rating3', 'rating1')
        for (x in 1:2) {
          tiff(filename = file.path(plotFolder,paste0("Actual_performance_", xlab[x], ".tiff")), width=3600, height=2400, units="px", res = 300)
          par(mar=c(5,7,2,1) + 0.1)
          plot(
            c(),
            c(),
            main = str_to_title(dataset),
            ylab = paste0("Actual performance\n(", performance_meaning[[dataset]], ")"), 
            xlab = xlab[x],
            ylim = plot_bounds[['narrow']][['performance']][[dataset]],
            xlim = xbounds[[x]],
            xaxp = c(1, 9, 8),
            cex.lab = 2,
            cex.main = 2,
            cex.axis = 2,
            cex.sub = 2
          )
          for (time in 1:3) {
            y_range <- plot_bounds[['narrow']][['performance']][[dataset]][2] - plot_bounds[['narrow']][['performance']][[dataset]][1]
            x_min <- xbounds[[x]][1]
            x_range <- xbounds[[x]][2] - x_min
            
            for (i in 1:2) {
              group <- groups[i]
              performance <- data[[dataset]][[split_type]][[time]][[group]]$performance
              performance <- performance + rnorm(
                length(performance),
                mean = 0,
                sd = 0.05
              )
              
              rating <- data[[dataset]][[split_type]][[time]][[group]][[xdata[x]]]
              rating <- rating + rnorm(length(rating),
                                       mean = 0,
                                       sd =  scatterplot_scatter)
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
          
          dev.off()
        }
        
        # Violin plots across real performance and either self-rated performance or sleepiness
        for (x in 1:2) {
          
          # This is a kluge to get around the fact that I can't figure out when
          # the split violin plot puts each group to the left or the right
          for (shift in c('Rested_left', 'Rested_right')) {
            
            # The violin plot requires us to collect the performances and ratings over the 3 sessions
            combined_performance <- list()
            combined_rating <- list()
            # The scatter plot will require some overlaid scatter
            combined_rating_scattered <- list()
            for (i in 1:2) {
              group <- groups[i]
              combined_performance[[group]] <- list()
              combined_rating[[group]] <- list()
              combined_rating_scattered[[group]] <- list()
              
              for (time in 1:3) {
                combined_performance[[group]] <- c(combined_performance[[group]],
                                                   data[[dataset]][[split_type]][[time]][[group]]$performance)
                rating <- data[[dataset]][[split_type]][[time]][[group]][[xdata[x]]]
                combined_rating[[group]] <- c(combined_rating[[group]], rating)
                scatter <- abs(rnorm(length(rating),
                                     mean = 0,
                                     sd = scatterplot_scatter)) + scatterplot_scatter
                if ((group == 'Sleep-deprived' & shift == 'Rested_left') | (group == 'Well-rested' & shift == 'Rested_right')) {
                  combined_rating_scattered[[group]] <- c(combined_rating_scattered[[group]], rating + scatter)
                } else {
                  combined_rating_scattered[[group]] <- c(combined_rating_scattered[[group]], rating - scatter)
                }
              }
            }
            
            violin_data <- data.frame(
              combined_performance = unlist(c(combined_performance[['Sleep-deprived']],
                                              combined_performance[['Well-rested']])),
              combined_rating = unlist(c(combined_rating[['Sleep-deprived']],
                                         combined_rating[['Well-rested']])),
              group = unlist(c(rep('Sleep-deprived', length(combined_performance[['Sleep-deprived']])),
                               rep('Well-rested', length(combined_performance[['Well-rested']]))))
            )
            violin_colours <- unlist(c(rep(colours[['Sleep-deprived']], length(combined_performance[['Sleep-deprived']])),
                                       rep(colours[['Well-rested']], length(combined_performance[['Well-rested']]))))
            
            sleep_deprived_data <- data.frame(combined_performance = unlist(c(combined_performance[['Sleep-deprived']])),
                                              combined_rating = unlist(c(combined_rating_scattered[['Sleep-deprived']])))
            
            rested_data <- data.frame(combined_performance = unlist(c(combined_performance[['Well-rested']])),
                                      combined_rating = unlist(c(combined_rating_scattered[['Well-rested']])))
            
            ggplot(violin_data,
                   aes(x = as.factor(combined_rating),
                       y = combined_performance,
                       fill = group,
                       color = group)) +
              geom_split_violin(scale = 'width') +
              scale_fill_manual(values=c('white', 'white'),
                                guide="none") +
              scale_color_manual(values=colours,
                                 guide="none") +
              geom_point(data = rested_data,
                         aes(x = combined_rating,
                             y = combined_performance),
                         size = 1.0,
                         stroke = 0,
                         color = colours[['Well-rested']]) +
              geom_point(data = sleep_deprived_data,
                         aes(x = combined_rating,
                             y = combined_performance),
                         size = 1.0,
                         stroke = 0,
                         color = colours[['Sleep-deprived']]) +
              xlab(xlab[x]) +
              ylab(paste0("Actual performance\n(", performance_meaning[[dataset]], ")")) +
              ggtitle(str_to_title(dataset)) +
              guides(fill="none") +
              ylim(plot_bounds[['narrow']][['performance']][[dataset]][[1]],
                   plot_bounds[['narrow']][['performance']][[dataset]][[2]]) +
              theme(text = element_text(size = 25)) 
            
            ggsave(file.path(file.path(plotFolder,paste0("Actual_performance_", xlab[x], "_violin_", shift, ".tiff"))), width=2400, height=2400, units="px", dpi=300)
          }
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
      fullAnalysis(n_acc, n_inacc, practical_significance, outputFile, plotFolder)
    }
    
    # Analysis of ratings
    n_rating_above <- c('Sleep-deprived' = 0, 'Well-rested' = 0)
    n_rating_below <- c('Sleep-deprived' = 0, 'Well-rested' = 0)
    
    n_rating_above_by_test <- list()
    n_rating_below_by_test <- list()
    
    for (l in 1:length(datasets)) {
      dataset <- datasets[l]
      
      n_rating_above_by_test[[dataset]] <- c('Sleep-deprived' = 0, 'Well-rested' = 0)
      n_rating_below_by_test[[dataset]] <- c('Sleep-deprived' = 0, 'Well-rested' = 0)
      for (i in 1:2) {
        group <- groups[i]
    
        for (time in 1:3) {
          current_median_rating <- median_rating[[dataset]][[split_type]][[time]][['across']]
          
          n_rating_above_by_test[[dataset]][[group]] <- n_rating_above_by_test[[dataset]][[group]] +
            sum(data[[dataset]][[split_type]][[time]][[group]]$rating3 >= current_median_rating)
          n_rating_below_by_test[[dataset]][[group]] <- n_rating_below_by_test[[dataset]][[group]] +
            sum(data[[dataset]][[split_type]][[time]][[group]]$rating3 < current_median_rating)
        }
        
        n_rating_above[[group]] <- n_rating_above[[group]] + n_rating_above_by_test[[dataset]][[group]]
        n_rating_below[[group]] <- n_rating_below[[group]] + n_rating_below_by_test[[dataset]][[group]]
      }
      
      outputFile <- file.path('Text_output',
                              practical_significance_string(practical_significance),
                              split_type,
                              'across groups',
                              'Individual_tests',
                              paste0(dataset, '.txt'))
      plotFolder <- file.path("Plots",
                              practical_significance_string(practical_significance),
                              split_type,
                              'across groups',
                              'Individual_tests',
                              dataset)
      ratingAnalysis(n_rating_above_by_test[[dataset]],
                     n_rating_below_by_test[[dataset]],
                     practical_significance,
                     outputFile,
                     plotFolder)
    }
    
    outputFile <- file.path('Text_output',
                            practical_significance_string(practical_significance),
                            split_type,
                            'across groups',
                            'Aggregate.txt')
    plotFolder <- file.path("Plots",
                            practical_significance_string(practical_significance),
                            split_type,
                            'across groups',
                            "Aggregate")
    ratingAnalysis(n_rating_above,
                   n_rating_below,
                   practical_significance,
                   outputFile,
                   plotFolder)
  }
  
  # Here we run some final sanity checks.
  dir.create(file.path(
    'Plots',
    practical_significance_string(practical_significance),
    'Sanity_checks'
  ),
  showWarnings = FALSE)
  dir.create(file.path(
    'Text_output',
    practical_significance_string(practical_significance),
    'Sanity_checks'
  ),
  showWarnings = FALSE)
  
  # What if just have no data?
  dir.create(file.path(
    'Plots',
    practical_significance_string(practical_significance),
    'Sanity_checks',
    'No_data'
  ),
  showWarnings = FALSE)
  
  outputFile <- file.path('Text_output',
                          practical_significance_string(practical_significance),
                          'Sanity_checks',
                          'No_data.txt')
  file.create(file.path(outputFile))
  plotFolder <- file.path('Plots',
                          practical_significance_string(practical_significance),
                          'Sanity_checks',
                          'No_data')
  
  fullAnalysis(c('Sleep-deprived' = 0, 'Well-rested' = 0),
               c('Sleep-deprived' = 0, 'Well-rested' = 0),
               practical_significance,
               outputFile,
               plotFolder)
  
  # What if everybody got it right?
  dir.create(file.path(
    'Plots',
    practical_significance_string(practical_significance),
    'Sanity_checks',
    'All_correct'
  ),
  showWarnings = FALSE)
  
  outputFile <- file.path('Text_output',
                          practical_significance_string(practical_significance),
                          'Sanity_checks',
                          'All_correct.txt')
  
  file.create(file.path(outputFile))
  
  plotFolder <- file.path('Plots',
                          practical_significance_string(practical_significance),
                          'Sanity_checks',
                          'All_correct')
  fullAnalysis(c('Sleep-deprived' = 0, 'Well-rested' = 91 * length(datasets)),
               c('Sleep-deprived' = 91 * length(datasets), 'Well-rested' = 0),
               practical_significance,
               outputFile,
               plotFolder)
}


