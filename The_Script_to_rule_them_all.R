### data Reanalysis script

#"what second-language speakers can tell us about pragmatic processing"
options(scipen=999)
#load packages
library('Hmisc')
library('corrplot')
library('ggplot2')
library('dplyr')
library('factoextra')
library('ggbiplot')
#library('tidyverse')  
library('cluster')    


### read in data directly copied from the given Script

data <- read.table(here::here("data", "data_L2_scalars.csv"), header=T, sep= ",")



### look at accuracy by participant excluding critical condition
### check for accuracy in fillers
data_acc <- subset(data, Condition!="T1")

accuracy <- aggregate(accuracy ~ Subject, data_acc, mean)

#accuracy
accuracy$score <- accuracy$accuracy
accuracy$accuracy <- NULL
data <- left_join(data, accuracy, by=c("Subject"= "Subject")) # changed in the Reanalysis due to syntax errors

### remove participants with less than 70% accuracy 

data <- subset(data,score>0.7)

##### check nr of participants 

length(unique(data$Subject))

# plot accuracy 

#acc.summary <- summarySEwithin(data, measurevar = "accuracy", withinvars = c( "Condition","Proficiency"))
#acc.summary

data$Proficiency <- as.factor(data$Proficiency)

data$Proficiency<- relevel(data$Proficiency, "high")
# until here we have the same outlier exclusion as the Paper


####################################################################
####################################################################
##########################
##########################
####################################################################
########################
######### EXPLORATORY ANALYSIS looking at individual differences andv their effect on implicature derivation
########
#####

####focus on T1 responses only, just like paper

data_sub <- subset(data, Condition=="T1")

data_sub <- droplevels(data_sub)
contrasts(data_sub$Proficiency) <- contr.sum(2)/2

###PCA
## since we have 11 personality measures, its likely that some of them are correlated
data_pca <- data_sub %>% distinct(Subject, .keep_all = TRUE) #remove duplicate subjects
data_pca <- data_pca %>% dplyr::select(TotalSocialSkill, TotalAttentionToDetail,TotalAttentionSwitching, TotalCommunication, Totalimagination, TotalSystemizing,  TotalExtraversion, TotalAgreeableness, TotalConscientiousness, TotalNeuroticism, TotalOpeness)
# we have 11 individual difference measures
# Big Five Inventory B5: Extraversion, Agreeableness, conscientiousness, Neuroticism,Openness
# Autism Quotient : Social Skill, Attention To Detail, Communication, Imagination, Attention Switching
#Systemizing Quotient-Revised (SQ-R): Sytemizing

##correlation Matrix
correlation <- rcorr(as.matrix(data_pca))
corrplot(correlation$r, type="lower", order="hclust",diag=0,insig = "blank",addCoef.col = 'black')

## Actual PCA
#standardize the data
data_pca <- scale(data_pca)
##first run of PCA to determin how mamy Components to keep
pca1 <- psych::principal(data_pca, nfactors = 11, rotate = "none")
summary(pca1)
round(pca1$values,2) # we have 6 values around 1 or bigger, so we keep 6, ~Kaiser criterion
eigenvalues <- pca1$values
# Create a scree plot PCA1
plot(eigenvalues, type = "b", main = "Scree Plot", xlab = "Component Number", ylab = "Eigenvalue", pch = 19)
abline(h = 1, col = "red", lty = 2)  # Adds a horizontal line at eigenvalue = 1

##second round of pca
pca2 <- psych::principal(data_pca, nfactors = 6, rotate = "varimax", scores = TRUE) 
print(pca2)

# openesss loads into RC1 & RC3 almoast the same

#RC1  Negative Attention switching, Positive extraversion (openess)
#RC2 Social Skill, Communication
#RC3 Imagination
#RC4 Attention to Detail, Systemizing
#RC5 Neuroticsm
#RC6 Imagination, Agreeableness, Conscientiousness (openess)
#plot
#colnames(pca2$loadings) <- c("Agreeable & Concious (open)", "-Attention switching & + extraversion (open)", "Social Skill & Communication", "Attention to Detail & Systemizing", "Imagination", "Neuroticsm")
print(pca2)
pca2_prcomp <- prcomp(pca2$scores)
fviz_pca_biplot(pca2_prcomp,label="var", repel = TRUE)
#############################################################
### K-means Cluster Analysis
data_cluster <- data_sub %>% dplyr::select(Subject, word4.ACC, word4.RT, Age, TotalSocialSkill, TotalAttentionToDetail,TotalAttentionSwitching, TotalCommunication, Totalimagination, TotalSystemizing,  TotalExtraversion, TotalAgreeableness, TotalConscientiousness, TotalNeuroticism, TotalOpeness, Proficiency)
data_kmeans <- scale(data_cluster[,2:15])
distance <- get_dist(data_kmeans)
#fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
fviz_nbclust(data_kmeans, kmeans, method = "wss")
fviz_nbclust(data_kmeans, kmeans, method = "silhouette")
gap_stat <- clusGap(data_kmeans, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)
# after testing those recomended ones i chose the outcome of silhouette as these clusters did not overlap
k2 <- kmeans(data_kmeans, centers = 2, nstart = 25)
fviz_cluster(k2, data = data_kmeans)
str(k2)

data_cluster$cluster <- k2$cluster # adding the cluster to the data
cluster_summary <- aggregate(. ~ cluster, data = data_cluster, FUN = function(x) c(mean = mean(x), var = var(x)))

