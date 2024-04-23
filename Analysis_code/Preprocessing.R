library(brms)
library(dplyr)
library(pracma)

# Loading and pre-processing of data
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

performance_indicators <- c('arithmetic' = 'correct',
                            'episodic memory' = 'correct',
                            'working memory' = 'correct',
                            'stroop' = 'reaction_time',
                            'simple attention' = 'reaction_time')

split_types <- c('pre-set groups', 'reported sleepiness')

groups <- c('test', 'control')
group_sd <- c('test' = 'Sleep Deprivation',
              'control' = 'Control')

data <- list()
median_performance <-  list()
median_rating <-  list()
median_sleepiness <-  list()

dir.create('Plots', showWarnings = FALSE)
for (i in 1:length(datasets)) {
  dataset <- datasets[i]
  print(paste0('Preprocessing ', dataset, ' data'))
  dir.create(file.path('Plots', dataset), showWarnings = FALSE)
  
  performance_indicator <- as.character(performance_indicators[[dataset]])
  
  file_data <- read.csv(file = paste0('Data/', filenames[[dataset]]))
  data_processed <- file_data %>% group_by(ID, time) %>% summarize(performance = mean(.data[[performance_indicator]]))
  
  data_merged <- merge(data_processed,
                       kss[kss$test_type == 'M',],
                       by.x=c("ID", "time"),
                       by.y=c("id", "time"),
                       all.x=FALSE,
                       all.y=FALSE)
  
  data[[dataset]] <- list()
  median_performance[[dataset]] <- list()
  median_rating[[dataset]] <- list()
  median_sleepiness[[dataset]] <- list()
  
  median_sleepiness_across_groups <- median(data_merged$rating1)
  print(paste0('Reported median sleepiness across the board is ', median_sleepiness_across_groups))
  
  for (k in 1:2) {
    split_type <- split_types[k]
    print(paste0('Splitting by ', split_type))
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
      group = groups[i]
      print(paste0('   For the ', group, ' group'))
      for (time in 1:3) {
        print(paste0('      For time ', time))
        print(paste0('         The median self-rated performance in the control group was ', median_rating[[dataset]][[split_type]][['control']][[time]]))
        print(paste0('         The median self-rated performance in the test group was ', median_rating[[dataset]][[split_type]][['test']][[time]]))
        print(paste0('         The median actual performance in the control group was ', format(median_performance[[dataset]][[split_type]][['control']][[time]]), nsmall = 2))
        print(paste0('         The median actual performance in the test group was ', format(median_performance[[dataset]][[split_type]][['test']][[time]]), nsmall = 2))
        print(paste0('         The median reported sleepiness in the control group was ', median_sleepiness[[dataset]][[split_type]][['control']][[time]]))
        print(paste0('         The median reported sleepiness in the test group was ', median_sleepiness[[dataset]][[split_type]][['test']][[time]]))
      }
    }
  }
}
