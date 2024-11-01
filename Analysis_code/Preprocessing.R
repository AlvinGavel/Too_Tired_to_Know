library(brms)
library(dplyr)
library(pracma)
library(stringr)
library(scales)
library(ggplot2)
library(latex2exp)

printOutput <- function(string, filePath) {
  print(string)
  write(string, filePath, append=TRUE)
}

colours <- c('Sleep-deprived' = "#FFC20A", 'Well-rested' = "#0C7BDC")

dir.create('Plots',
           showWarnings = FALSE)
dir.create(file.path('Plots',
                     'Varying_practical_significance'),
           showWarnings = FALSE)
dir.create('Text_output',
           showWarnings = FALSE)
dir.create(file.path('Text_output',
                     'Varying_practical_significance'),
           showWarnings = FALSE)

outputFile <- file.path('Text_output', "Preprocessing.txt")
file.create(outputFile)

kss <- read.csv(file = 'Data/kss_data.csv')

datasets <- c('throughput',
              'episodic memory',
              'working memory',
              'executive processing',
              'simple attention'
)

filenames <- c('throughput' = 'arithmetic_data.csv',
               'episodic memory' = 'episodic_memory_data.csv',
               'working memory' = 'working_memory_data.csv',
               'executive processing' = 'Stroop_data.csv',
               'simple attention' = 'simple_attention_data.csv')

kss_abbrev <- c('throughput' = 'M',
                'episodic memory' = 'ST',
                'working memory' = 'W',
                'executive processing' = 'stroop',
                'simple attention' = 'reactionTime')

split_types <- c('pre-set groups', 'reported sleepiness')

median_types <- c('within groups', 'across groups')

groups <- c('Sleep-deprived', 'Well-rested')
group_sd <- c('Sleep-deprived' = 'Sleep Deprivation',
              'Well-rested' = 'Control')

data <- list()
data_sessions_combined <- list()
median_performance <-  list()
median_rating <-  list()
median_sleepiness <-  list()
min_performance <- list()
max_performance <- list()
shortest_session <- list()
longest_session <- list()

for (i in 1:length(datasets)) {
  dataset <- datasets[i]
  printOutput(paste0('Preprocessing ', dataset, ' data'), outputFile)
  
  file_data <- read.csv(file = file.path('Data', filenames[[dataset]]))
  
  if (dataset == 'working memory') {
    count_twelves = sum(file_data$order_in_test == 12)
    count_tens = sum(file_data$order_in_test == 10)
    printOutput(paste('In the working memory test', count_twelves, 'rounds went to 12 rounds'), outputFile)
    printOutput(paste('While', count_tens- count_twelves, 'rounds only went to 10 rounds'), outputFile)
  }
  
  if (dataset == 'throughput') {
    data_processed <- file_data %>% group_by(ID, time) %>% summarize(performance = 60 * 1000 * sum(correct) / (max(time_start_battery) - min(time_start_battery)),
                                                                     session_length = max(time_start_battery) - min(time_start_battery))
    # We might want to adjust this one
    data_processed <- data_processed[data_processed$session_length >= 100000 & data_processed$session_length <= 130000,]
  } else if (dataset == 'simple attention') {
    data_processed <- file_data %>% group_by(ID, time) %>% summarize(performance = mean(reaction_time),
                                                                     session_length = max(time_start_battery) - min(time_start_battery))
  } else if (dataset == 'executive processing') {
    response_given <- file_data[file_data$response != '',]
    data_processed <- response_given %>% group_by(ID, time) %>% summarize(performance = mean(reaction_time),
                                                                          session_length = max(time_start_battery) - min(time_start_battery))
  } else {
    data_processed <- file_data %>% group_by(ID, time) %>% summarize(performance = mean(correct),
                                                                     session_length = max(time_start_battery) - min(time_start_battery))
  }
  
  min_performance[dataset] <- min(data_processed$performance)
  max_performance[dataset] <- max(data_processed$performance)
  session_happened <- data_processed[data_processed$session_length > 0,]
  shortest_session[dataset] <- min(session_happened$session_length )
  longest_session[dataset] <- max(session_happened$session_length)
  printOutput(paste0('Shortest session was ', shortest_session[dataset], ' milliseconds'), outputFile)
  printOutput(paste0('Longest session was ', longest_session[dataset], ' milliseconds'), outputFile)
  
  data_merged <- merge(data_processed,
                       kss[kss$test_type == kss_abbrev[dataset],],
                       by.x=c("ID", "time"),
                       by.y=c("id", "time"),
                       all.x=FALSE,
                       all.y=FALSE)
  
  data[[dataset]] <- list()
  data_sessions_combined[[dataset]] <- list()
  median_performance[[dataset]] <- list()
  median_rating[[dataset]] <- list()
  median_sleepiness[[dataset]] <- list()
  
  sleepiness_tasks_combined <- data.frame(sleepiness=data_merged$rating1,
                                          group=data_merged$sd)
  sleepiness_tasks_combined <- transform(sleepiness_tasks_combined, fill=ifelse(group=='Control',
                                                                                colours[['Well-rested']],
                                                                                colours[['Sleep-deprived']]))
  
  median_sleepiness_across_groups <- median(sleepiness_tasks_combined$sleepiness)
  printOutput(paste0('Reported median sleepiness across the board is ', median_sleepiness_across_groups), outputFile)
  
  for (j in 1:length(split_types)) {
    split_type <- split_types[j]
    printOutput(paste0('Splitting by ', split_type), outputFile)
    
    data[[dataset]][[split_type]] <- list()
    data_sessions_combined[[dataset]][[split_type]] <- list()
    median_performance[[dataset]][[split_type]] <- list()
    median_rating[[dataset]][[split_type]] <- list()
    median_sleepiness[[dataset]][[split_type]] <- list()
    
    for (k in 1:length(median_types)) {
      median_type <- median_types[k]
      printOutput(paste0('   Calculating median ', median_type), outputFile)
      
      data[[dataset]][[split_type]][[median_type]] <- list()
      data_sessions_combined[[dataset]][[split_type]][[median_type]] <- list()
      median_performance[[dataset]][[split_type]][[median_type]] <- list()
      median_rating[[dataset]][[split_type]][[median_type]] <- list()
      median_sleepiness[[dataset]][[split_type]][[median_type]] <- list()
      
      for (l in 1:length(groups)) {
        group <- groups[l]
        data_sessions_combined[[dataset]][[split_type]][[median_type]][[group]] <- list()
      }
      
      for (time in 1:3) {
        data[[dataset]][[split_type]][[median_type]][[time]] <- list()
        median_performance[[dataset]][[split_type]][[median_type]][[time]] <- list()
        median_rating[[dataset]][[split_type]][[median_type]][[time]] <- list()
        median_sleepiness[[dataset]][[split_type]][[median_type]][[time]] <- list()
        for (l in 1:length(groups)) {
          group <- groups[l]
          data[[dataset]][[split_type]][[median_type]][[time]][[group]] <- list()
          median_performance[[dataset]][[split_type]][[median_type]][[time]][[group]] <- list()
          median_rating[[dataset]][[split_type]][[median_type]][[time]][[group]] <- list()
          median_sleepiness[[dataset]][[split_type]][[median_type]][[time]][[group]] <- list()
          
          median_performance[[dataset]][[split_type]][[median_type]][[time]][['across']] <- list()
          median_rating[[dataset]][[split_type]][[median_type]][[time]][['across']] <- list()
          median_sleepiness[[dataset]][[split_type]][[median_type]][[time]][['across']] <- list()
          
          if (split_type == 'pre-set groups') {
            group_data <- data_merged[data_merged$sd == group_sd[group] & data_merged$time == time,]
          } else if (split_type == 'reported sleepiness') {
            if (group == 'Sleep-deprived') {
              group_data <- data_merged[data_merged$rating1 >= median_sleepiness_across_groups & data_merged$time == time,]
            } else if (group == 'Well-rested') {
              group_data <- data_merged[data_merged$rating1 < median_sleepiness_across_groups & data_merged$time == time,]
            }
          }
          
          data[[dataset]][[split_type]][[median_type]][[time]][[group]] <- group_data
          if (time == 1) {
            data_sessions_combined[[dataset]][[split_type]][[median_type]][[group]] <- group_data
          } else {
            data_sessions_combined[[dataset]][[split_type]][[median_type]][[group]] <- rbind(data_sessions_combined[[dataset]][[split_type]][[median_type]][[group]],
                                                                                             group_data)
          }
          median_rating[[dataset]][[split_type]][[median_type]][[time]][[group]] <- median(data[[dataset]][[split_type]][[median_type]][[time]][[group]]$rating3)
          median_performance[[dataset]][[split_type]][[median_type]][[time]][[group]] <- median(data[[dataset]][[split_type]][[median_type]][[time]][[group]]$performance)
          median_sleepiness[[dataset]][[split_type]][[median_type]][[time]][[group]] <- median(data[[dataset]][[split_type]][[median_type]][[time]][[group]]$rating1)
        }
        
        median_rating[[dataset]][[split_type]][[median_type]][[time]][['across']] <- median(c(data[[dataset]][[split_type]][[median_type]][[time]][['Well-rested']]$rating3,
                                                                                                  data[[dataset]][[split_type]][[median_type]][[time]][['Sleep-deprived']]$rating3))
        median_performance[[dataset]][[split_type]][[median_type]][[time]][['across']] <- median(c(data[[dataset]][[split_type]][[median_type]][[time]][['Well-rested']]$performance,
                                                                                                       data[[dataset]][[split_type]][[median_type]][[time]][['Sleep-deprived']]$performance))
        median_sleepiness[[dataset]][[split_type]][[median_type]][[time]][['across']] <- median(c(data[[dataset]][[split_type]][[median_type]][[time]][['Well-rested']]$rating1,
                                                                                                      data[[dataset]][[split_type]][[median_type]][[time]][['Sleep-deprived']]$rating1))
        
        printOutput(paste0('      For time ', time), outputFile)
        printOutput(paste0('         The median self-rated performance in the well-rested group was ', median_rating[[dataset]][[split_type]][[median_type]][[time]][['Well-rested']]), outputFile)
        printOutput(paste0('         The median self-rated performance in the sleep-deprived group was ', median_rating[[dataset]][[split_type]][[median_type]][[time]][['Sleep-deprived']]), outputFile)
        printOutput(paste0('         The median self-rated performance across groups was ', median_rating[[dataset]][[split_type]][[median_type]][[time]][['across']]), outputFile)
        printOutput(paste0('         The median actual performance in the well-rested group was ', format(median_performance[[dataset]][[split_type]][[median_type]][[time]][['Well-rested']]), nsmall = 2), outputFile)
        printOutput(paste0('         The median actual performance in the sleep-deprived group was ', format(median_performance[[dataset]][[split_type]][[median_type]][[time]][['Sleep-deprived']]), nsmall = 2), outputFile)
        printOutput(paste0('         The median actual performance across groups was ', format(median_performance[[dataset]][[split_type]][[median_type]][[time]][['across']]), nsmall = 2), outputFile)
        printOutput(paste0('         The median reported sleepiness in the well-rested group was ', median_sleepiness[[dataset]][[split_type]][[median_type]][[time]][['Well-rested']]), outputFile)
        printOutput(paste0('         The median reported sleepiness in the sleep-deprived group was ', median_sleepiness[[dataset]][[split_type]][[median_type]][[time]][['Sleep-deprived']]), outputFile)
        printOutput(paste0('         The median reported sleepiness across groups was ', median_sleepiness[[dataset]][[split_type]][[median_type]][[time]][['across']]), outputFile)
      }
    }
  }
}
printOutput('', outputFile)


