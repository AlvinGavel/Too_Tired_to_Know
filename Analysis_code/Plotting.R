source('Preprocessing.R')

plot_folder_path = '../Plots'

dir.create(plot_folder_path, showWarnings = FALSE)

png(filename= paste(plot_folder_path, 'Control_group.png', sep = '/'))
plot(arit_merged[arit_merged$sd == 'Control',]$performance,
     arit_merged[arit_merged$sd == 'Control',]$rating3,
     main="Control group",
     xlab="Actual performance",
     ylab="Self-rated performance",
     xlim=c(0.0,1.0),
     ylim=c(0,10),
     cex=0.1,
     pch=1)
dev.off()


png(filename= paste(plot_folder_path, 'Test_group.png', sep = '/'))
plot(arit_merged[arit_merged$sd == 'Sleep Deprivation',]$performance,
     arit_merged[arit_merged$sd == 'Sleep Deprivation',]$rating3,
     main="Test group",
     xlab="Actual performance",
     ylab="Self-rated performance",
     xlim=c(0.0,1.0),
     ylim=c(0,10),
     cex=0.1,
     pch=1)
dev.off()
