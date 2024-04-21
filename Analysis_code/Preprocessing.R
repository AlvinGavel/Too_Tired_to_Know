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
                            'stroop' = 'correct', # It is possible that we want to use reaction_time here
                            'simple attention' = 'reaction_time')


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
  for (i in 1:2) {
    group = groups[i]
    data[[dataset]][[group]] <- list()
    median_performance[[dataset]][[group]] <- list()
    median_rating[[dataset]][[group]] <- list()
    median_sleepiness[[dataset]][[group]] <- list()
    for (time in 1:3) {
      data[[dataset]][[group]][[time]] <- data_merged[data_merged$sd == group_sd[group] & data_merged$time == time,]
      
      median_performance[[dataset]][[group]][[time]] <- median(data[[dataset]][[group]][[time]]$performance)
      # Lower is better for reaction time
      if (performance_indicator == 'reaction_time') {
        median_performance[[dataset]][[group]][[time]] <- - median_performance[[dataset]][[group]][[time]]
      }
      
      median_rating[[dataset]][[group]][[time]] <- median(data[[dataset]][[group]][[time]]$rating3)
      median_sleepiness[[dataset]][[group]][[time]] <- median(data[[dataset]][[group]][[time]]$rating1)
      
    }
  }
  for (i in 1:2) {
    group = groups[i]
    print(paste0('For the ', group, ' group'))
    for (time in 1:3) {
      print(paste0('   For time ', time))
      print(paste0('      The median self-rated performance in the control group was ', median_rating[[dataset]][['control']][[time]]))
      print(paste0('      The median self-rated performance in the test group was ', median_rating[[dataset]][['test']][[time]]))
      print(paste0('      The median actual performance in the control group was ', format(median_performance[[dataset]][['control']][[time]]), nsmall = 2))
      print(paste0('      The median actual performance in the test group was ', format(median_performance[[dataset]][['test']][[time]]), nsmall = 2))
      print(paste0('      The median reported sleepiness in the control group was ', median_sleepiness[[dataset]][['control']][[time]]))
      print(paste0('      The median reported sleepiness in the test group was ', median_sleepiness[[dataset]][['test']][[time]]))
    }
  }
}
