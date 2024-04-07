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
