library(brms)
library(dplyr)
library(pracma)

dir.create('Text_output', showWarnings = FALSE)
outputFile <- file.path('Text_output', "Preprocessing.txt")
file.create(outputFile)

printOutput <- function(string, filePath) {
  print(string)
  write(string, filePath, append=TRUE)
}

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

groups <- c('test', 'control')
group_sd <- c('test' = 'Sleep Deprivation',
              'control' = 'Control')

data <- list()
median_performance <-  list()
median_rating <-  list()
median_sleepiness <-  list()
min_performance <- list()
max_performance <- list()
shortest_session <- list()
longest_session <- list()

dir.create('Plots', showWarnings = FALSE)
for (i in 1:length(datasets)) {
  dataset <- datasets[i]
  printOutput(paste0('Preprocessing ', dataset, ' data'), outputFile)
  dir.create(file.path('Plots', dataset), showWarnings = FALSE)
  
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
  median_performance[[dataset]] <- list()
  median_rating[[dataset]] <- list()
  median_sleepiness[[dataset]] <- list()
  
  median_sleepiness_across_groups <- median(data_merged$rating1)
  printOutput(paste0('Reported median sleepiness across the board is ', median_sleepiness_across_groups), outputFile)
  
  for (k in 1:2) {
    split_type <- split_types[k]
    printOutput(paste0('Splitting by ', split_type), outputFile)
    dir.create(file.path('Plots', dataset, split_type), showWarnings = FALSE)
    
    data[[dataset]][[split_type]] <- list()
    median_performance[[dataset]][[split_type]] <- list()
    median_rating[[dataset]][[split_type]] <- list()
    median_sleepiness[[dataset]][[split_type]] <- list()
    
    for (i in 1:2) {
      group <- groups[i]
      data[[dataset]][[split_type]][[group]] <- list()
      median_performance[[dataset]][[split_type]][[group]] <- list()
      median_rating[[dataset]][[split_type]][[group]] <- list()
      median_sleepiness[[dataset]][[split_type]][[group]] <- list()
      for (time in 1:3) {
        if (split_type == 'pre-set groups') {
          group_data <- data_merged[data_merged$sd == group_sd[group] & data_merged$time == time,]
        } else if (split_type == 'reported sleepiness') {
          if (group == 'test') {
            group_data <- data_merged[data_merged$rating1 >= median_sleepiness_across_groups & data_merged$time == time,]
          } else if (group == 'control') {
            group_data <- data_merged[data_merged$rating1 < median_sleepiness_across_groups & data_merged$time == time,]
          }
        }
        
        data[[dataset]][[split_type]][[group]][[time]] <- group_data
        median_rating[[dataset]][[split_type]][[group]][[time]] <- median(data[[dataset]][[split_type]][[group]][[time]]$rating3)
        median_performance[[dataset]][[split_type]][[group]][[time]] <- median(data[[dataset]][[split_type]][[group]][[time]]$performance)
        median_sleepiness[[dataset]][[split_type]][[group]][[time]] <- median(data[[dataset]][[split_type]][[group]][[time]]$rating1)
      }
    }
    for (i in 1:2) {
      group <- groups[i]
      printOutput(paste0('   For the ', group, ' group'), outputFile)
      for (time in 1:3) {
        printOutput(paste0('      For time ', time), outputFile)
        printOutput(paste0('         The median self-rated performance in the control group was ', median_rating[[dataset]][[split_type]][['control']][[time]]), outputFile)
        printOutput(paste0('         The median self-rated performance in the test group was ', median_rating[[dataset]][[split_type]][['test']][[time]]), outputFile)
        printOutput(paste0('         The median actual performance in the control group was ', format(median_performance[[dataset]][[split_type]][['control']][[time]]), nsmall = 2), outputFile)
        printOutput(paste0('         The median actual performance in the test group was ', format(median_performance[[dataset]][[split_type]][['test']][[time]]), nsmall = 2), outputFile)
        printOutput(paste0('         The median reported sleepiness in the control group was ', median_sleepiness[[dataset]][[split_type]][['control']][[time]]), outputFile)
        printOutput(paste0('         The median reported sleepiness in the test group was ', median_sleepiness[[dataset]][[split_type]][['test']][[time]]), outputFile)
      }
    }
  }
  printOutput('', outputFile)
}


