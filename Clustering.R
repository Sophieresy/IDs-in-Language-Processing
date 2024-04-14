library('tidyLPA')
library('dplyr')
library(tidyverse)  
library(cluster)    
library(factoextra)
# read data

data <- read.table(here::here("data", "data_L2_scalars.csv"), header=T, sep= ",")

cluster_data <- subset(data, Condition =="T1")
#remove coulums i dont need
cluster_data <- cluster_data %>% dplyr::select(Subject, word4.ACC, word4.RT, Age, TotalSocialSkill, TotalAttentionToDetail,TotalAttentionSwitching, TotalCommunication, Totalimagination, TotalSystemizing,  TotalExtraversion, TotalAgreeableness, TotalConscientiousness, TotalNeuroticism, TotalOpeness, Proficiency)

lpa <- estimate_profiles(cluster_data[,2:15], n_profiles = 1:9, models = c(1,6))
# das hat nicht so gut funktioniert
 
kmeans_data <- scale(cluster_data[,2:15])
distance <- get_dist(kmeans_data)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
fviz_nbclust(kmeans_data, kmeans, method = "wss")
k2 <- kmeans(kmeans_data, centers = 5, nstart = 25)
fviz_cluster(k2, data = kmeans_data)
str(k2)
k2
experiment <- cluster_data %>% dplyr::select(TotalSocialSkill, TotalAttentionToDetail,TotalAttentionSwitching, TotalCommunication, Totalimagination, TotalSystemizing,  TotalExtraversion, TotalAgreeableness, TotalConscientiousness, TotalNeuroticism, TotalOpeness)
experiment <- scale(experiment)
distance2 <- get_dist(experiment)
fviz_dist(distance2, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
fviz_nbclust(experiment, kmeans, method = "wss")
k2 <- kmeans(experiment, centers = 6, nstart = 25)
fviz_cluster(k2, data = kmeans_data)



data_cluster$cluster <- k2$cluster # adding the cluster to the data
cluster_summary <- aggregate(. ~ cluster, data = data_cluster, FUN = function(x) c(mean = mean(x)))

# Assuming you have cluster_summary as described xvsdfasgs<dgfsdf
# Extract the means for each cluster
cluster_1_means <- cluster_summary[cluster_summary$cluster == 1, -1]
cluster_2_means <- cluster_summary[cluster_summary$cluster == 2, -1]


# Define the names of the variables (assumed to be the same for both clusters)
variable_names <- names(cluster_1_means)

# Combine means for both clusters
barplot_data <- rbind(cluster_1_means, cluster_2_means)
barplot_data <- t(barplot_data)
barplot_data <- t(barplot_data)
# Create bar plot
barplot(barplot_data, beside = TRUE, 
        col = c("red", "blue"), # Colors for clusters
        # ylim = c(0, max(cluster_summary[, -1]) * 1.1), # Adjust ylim based on your preference
        xlab = "Variables", ylab = "Mean Values",
        
        legend.text = c("Cluster 1", "Cluster 2"),
        args.legend = list(x = "topright"))