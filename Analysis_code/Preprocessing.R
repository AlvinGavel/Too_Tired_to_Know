library(brms)
library(dplyr)
library(pracma)
library(stringr)
library(scales)
library(ggplot2)

printOutput <- function(string, filePath) {
  print(string)
  write(string, filePath, append=TRUE)
}

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

datasets <- c('arithmetic',
              'episodic memory',
              'working memory',
              'stroop',
              'simple attention'
)

filenames <- c('arithmetic' = 'arithmetic_data.csv',
               'episodic memory' = 'episodic_memory_data.csv',
               'working memory' = 'working_memory_data.csv',
               'stroop' = 'Stroop_data.csv',
               'simple attention' = 'simple_attention_data.csv')

kss_abbrev <- c('arithmetic' = 'M',
                'episodic memory' = 'ST',
                'working memory' = 'W',
                'stroop' = 'stroop',
                'simple attention' = 'reactionTime')

split_types <- c('pre-set groups', 'reported sleepiness')

median_types <- c('within groups', 'across groups')

groups <- c('test', 'control')
group_sd <- c('test' = 'Sleep Deprivation',
              'control' = 'Control')

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
  
  if (dataset == 'arithmetic') {
    data_processed <- file_data %>% group_by(ID, time) %>% summarize(performance = 120 * 1000 * sum(correct) / (max(time_start_battery) - min(time_start_battery)),
                                                                     session_length = max(time_start_battery) - min(time_start_battery))
    # We might want to adjust this one
    data_processed <- data_processed[data_processed$session_length >= 100000 & data_processed$session_length <= 130000,]
  } else if (dataset == 'simple attention') {
    data_processed <- file_data %>% group_by(ID, time) %>% summarize(performance = mean(reaction_time),
                                                                     session_length = max(time_start_battery) - min(time_start_battery))
  } else if (dataset == 'stroop') {
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
  
  median_sleepiness_across_groups <- median(data_merged$rating1)
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
            if (group == 'test') {
              group_data <- data_merged[data_merged$rating1 >= median_sleepiness_across_groups & data_merged$time == time,]
            } else if (group == 'control') {
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
        
        median_rating[[dataset]][[split_type]][[median_type]][[time]][['across']] <- median(c(data[[dataset]][[split_type]][[median_type]][[time]][['control']]$rating3,
                                                                                                  data[[dataset]][[split_type]][[median_type]][[time]][['test']]$rating3))
        median_performance[[dataset]][[split_type]][[median_type]][[time]][['across']] <- median(c(data[[dataset]][[split_type]][[median_type]][[time]][['control']]$performance,
                                                                                                       data[[dataset]][[split_type]][[median_type]][[time]][['test']]$performance))
        median_sleepiness[[dataset]][[split_type]][[median_type]][[time]][['across']] <- median(c(data[[dataset]][[split_type]][[median_type]][[time]][['control']]$rating1,
                                                                                                      data[[dataset]][[split_type]][[median_type]][[time]][['test']]$rating1))
        
        printOutput(paste0('      For time ', time), outputFile)
        printOutput(paste0('         The median self-rated performance in the control group was ', median_rating[[dataset]][[split_type]][[median_type]][[time]][['control']]), outputFile)
        printOutput(paste0('         The median self-rated performance in the test group was ', median_rating[[dataset]][[split_type]][[median_type]][[time]][['test']]), outputFile)
        printOutput(paste0('         The median self-rated performance across groups was ', median_rating[[dataset]][[split_type]][[median_type]][[time]][['across']]), outputFile)
        printOutput(paste0('         The median actual performance in the control group was ', format(median_performance[[dataset]][[split_type]][[median_type]][[time]][['control']]), nsmall = 2), outputFile)
        printOutput(paste0('         The median actual performance in the test group was ', format(median_performance[[dataset]][[split_type]][[median_type]][[time]][['test']]), nsmall = 2), outputFile)
        printOutput(paste0('         The median actual performance across groups was ', format(median_performance[[dataset]][[split_type]][[median_type]][[time]][['across']]), nsmall = 2), outputFile)
        printOutput(paste0('         The median reported sleepiness in the control group was ', median_sleepiness[[dataset]][[split_type]][[median_type]][[time]][['control']]), outputFile)
        printOutput(paste0('         The median reported sleepiness in the test group was ', median_sleepiness[[dataset]][[split_type]][[median_type]][[time]][['test']]), outputFile)
        printOutput(paste0('         The median reported sleepiness across groups was ', median_sleepiness[[dataset]][[split_type]][[median_type]][[time]][['across']]), outputFile)
      }
    }
  }
}
printOutput('', outputFile)


