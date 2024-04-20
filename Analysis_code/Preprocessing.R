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

data_cont <- c()
data_test <- c()
median_performance_cont <- c()
median_performance_test <- c()
median_rating_cont <- c()
median_rating_test <- c()
median_sleepiness_cont <- c()
median_sleepiness_test <- c()

for (i in 1:length(datasets)) {
  dataset <- datasets[i]
  print(paste0('Preprocessing ', dataset, ' data'))
  dir.create(file.path('Plots', dataset), showWarnings = FALSE)
  
  performance_indicator <- as.character(performance_indicators[[dataset]])

  data <- read.csv(file = paste0('Data/', filenames[[dataset]]))
  data_processed <- data %>% group_by(ID, time) %>% summarize(performance = mean(.data[[performance_indicator]]))

  data_merged <- merge(data_processed,
                      kss[kss$test_type == 'M',],
                      by.x=c("ID", "time"),
                      by.y=c("id", "time"),
                      all.x=FALSE,
                      all.y=FALSE)


  data_cont[[dataset]] <- list()
  data_test[[dataset]] <- list()
  median_performance_cont[[dataset]] <- list()
  median_performance_test[[dataset]] <- list()
  median_rating_cont[[dataset]] <- list()
  median_rating_test[[dataset]] <- list()
  median_sleepiness_cont[[dataset]] <- list()
  median_sleepiness_test[[dataset]] <- list()
  for (time in 1:3) {
    data_cont[[dataset]][[time]] <- data_merged[data_merged$sd == 'Control' & data_merged$time == time,]
    data_test[[dataset]][[time]] <- data_merged[data_merged$sd == 'Sleep Deprivation' & data_merged$time == time,]

    median_performance_cont[[dataset]][[time]] <- median(data_cont[[dataset]][[time]]$performance)
    median_performance_test[[dataset]][[time]] <- median(data_test[[dataset]][[time]]$performance)

    median_rating_cont[[dataset]][[time]] <- median(data_cont[[dataset]][[time]]$rating3)
    median_rating_test[[dataset]][[time]] <- median(data_test[[dataset]][[time]]$rating3)
    
    median_sleepiness_cont[[dataset]][[time]] <- median(data_cont[[dataset]][[time]]$rating1)
    median_sleepiness_test[[dataset]][[time]] <- median(data_test[[dataset]][[time]]$rating1)
  }
}
