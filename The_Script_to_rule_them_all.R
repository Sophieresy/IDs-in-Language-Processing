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
library(Rmisc)
library(lme4)

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
### split T1 into logical and pragmatic responses
data$Condition <- as.character(data$Condition)
data$condition_new <- ifelse(data$Condition=="T1" & data$word4.RESP=="TRUE","T1_logic",
                             ifelse(data$Condition=="T1" & data$word4.RESP=="FALSE","T1_pragmatic", data$Condition))

data$condition_new <- as.factor(as.character(data$condition_new))

# how many subjects are only logical or only pragmatic?

data$accuracy_numeric <- as.numeric(as.character(data$accuracy))


acc.summary_sub <- summarySEwithin(data, measurevar = "accuracy_numeric", withinvars = c( "condition_new","Subject","Proficiency"))
acc.summary_sub

nans <- subset(acc.summary_sub, N != 1)
# how many subjects are proficient?

acc.summary_subject <- summarySEwithin(data, measurevar = "accuracy_numeric", withinvars = c("Subject","Proficiency", "Condition"))
acc.summary_subject


# how many items per subject ?

acc.summary_item <- summarySEwithin(data, measurevar = "accuracy_numeric", withinvars = c("item","Category","criticalWord"))
acc.summary_item

#### change values of correct to include only controls
data$correct_new <- ifelse(data$Condition=="T1", 1, data$word4.ACC)


#### fit  a logistic regression model to accuracy 

##### change baseline to pragmatic condition and high proficiency 
data$condition_new <- as.factor(data$condition_new)
data$Proficiency <- as.factor(data$Proficiency)

data$condition_new<- relevel(data$condition_new, "T1_pragmatic")
data$Proficiency<- relevel(data$Proficiency, "high")


###convert relevant columns to categorical
data$subj <- as.factor(data$Subject)
data$Condition <- as.factor(as.character(data$Condition))
###code DV as categorical

data$accuracy <- as.factor(data$accuracy)

### sum contrast for proficiency 

contrasts(data$Proficiency) <- contr.sum(2)/2
contrasts(data$Proficiency)
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
#
print(pca2)
pca2_prcomp <- prcomp(pca2$scores)
fviz_pca_biplot(pca2_prcomp,label="var", repel = TRUE)
data_sub_pca <- cbind(data_sub[,],pca2$scores)

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
### K-means Cluster Analysis with PCA
data_cluster_pca<- data_sub_pca %>% dplyr::select( word4.ACC, word4.RT, Age,RC1,RC2,RC3,RC4,RC5,RC6)
data_kmeans_pca <- scale(data_cluster_pca)
distance <- get_dist(data_kmeans_pca)

fviz_nbclust(data_kmeans, kmeans, method = "wss")
fviz_nbclust(data_kmeans, kmeans, method = "silhouette")
gap_stat <- clusGap(data_kmeans, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

cluster_pca <- kmeans(data_kmeans_pca, centers = 2, nstart = 25)
fviz_cluster(cluster_pca, data = data_kmeans_pca)
str(cluster_pca)

### Imagination and social skill 
###simple model with no random slopes from paper
m.acc <- glm(accuracy ~ TotalSocialSkill+TotalAttentionToDetail+TotalAttentionSwitching+TotalCommunication+Totalimagination+
                 TotalSystemizing+TotalExtraversion+TotalAgreeableness+TotalConscientiousness+TotalNeuroticism+TotalOpeness,
               data = data_sub, family = binomial)
summary(m.acc)
coefplot(m.acc)
m.acc_pca <- glm(accuracy ~ RC1+RC2+RC3+RC4+RC5+RC6,
             data = data_sub_pca, family = binomial)
summary(m.acc_pca)
coefplot(m.acc)

m.acc <- glmer(accuracy ~ RC1+RC2+RC3+RC4+RC5+RC6+
                 
                 (1|item),
               data = data_sub_pca, family = binomial)
summary(m.acc)

#### now only significant predictors,
m.acc1 <- glm(accuracy ~ TotalSocialSkill+TotalAttentionToDetail+Totalimagination+  #problem: attention to detail & systemizing are correlated
                  TotalSystemizing,
                data = data_sub, family = binomial)
summary(m.acc1)
coefplot(m.acc1)
#### now only significant predictors, but attention to detail & systemizing are correlated - Replace by their PC

m.acc1_pca <- glm(accuracy ~ TotalSocialSkill+Totalimagination+RC4,
                data = data_sub_pca, family = binomial)
summary(m.acc1_pca)
####combined Model 
m.acc_combined <- glm(accuracy ~ Proficiency*Totalimagination + Proficiency*TotalSocialSkill,
                        data = data_sub, family = binomial)
summary(m.acc_combined)

# combined using their approach:
m.acc_combined <- glmer(accuracy ~ Proficiency*Totalimagination + Proficiency*TotalSocialSkill +
                          
                          (1+Totalimagination|item) + (1+Totalimagination|subj)+
                          (1+TotalSocialSkill|item) + (1+TotalSocialSkill|subj),
                        data = data_sub, family = binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list( maxfun = 50000)))
summary(m.acc_combined)
#i think the random slopes on items are negligible as they should not induce much variation by the nature of the experiment
m.acc_combined <- glmer(accuracy ~ Proficiency*Totalimagination + Proficiency*TotalSocialSkill +
(1+Totalimagination|subj) + (1+TotalSocialSkill|subj),
                        data = data_sub, family = binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list( maxfun = 50000)))
summary(m.acc_combined)

#coefplot(m.acc_combined, title ="Coeficient Plot Accuray combined",sort="alphabetical")
#has singularity issues, now try again with pca values

m.acc <- glmer(accuracy ~ RC1+RC2+RC3+RC4+RC5+RC6+
                 
                 (1|item),
               data = data_sub_pca, family = binomial)
summary(m.acc)
coefplot(m.acc)