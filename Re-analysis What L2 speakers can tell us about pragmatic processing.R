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
library('cluster')    
library('Rmisc')
library('lme4')

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
#acc.summary_sub

# how many subjects are proficient?

acc.summary_subject <- summarySEwithin(data, measurevar = "accuracy_numeric", withinvars = c("Subject","Proficiency", "Condition"))
#acc.summary_subject


# how many items per subject ?

acc.summary_item <- summarySEwithin(data, measurevar = "accuracy_numeric", withinvars = c("item","Category","criticalWord"))
#acc.summary_item

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
# End of Copy from the paper
##############################################################
########################
######### REANALYSIS looking at individual differences and their effect on implicature derivation
########
#####
####focus on T1 responses only

data_sub <- subset(data, Condition=="T1")

data_sub <- droplevels(data_sub)
contrasts(data_sub$Proficiency) <- contr.sum(2)/2
#############################################################
###PCA
## since we have 11 personality measures, its likely that some of them are correlated
data_pca <- data_sub %>% distinct(Subject, .keep_all = TRUE) #remove duplicate subjects
data_pca <- data_pca %>% dplyr::select(TotalSocialSkill, TotalAttentionToDetail,TotalAttentionSwitching, TotalCommunication, Totalimagination, TotalSystemizing,  TotalExtraversion, TotalAgreeableness, TotalConscientiousness, TotalNeuroticism, TotalOpeness)
# we have 11 individual difference measures
# Big Five Inventory B5: Extraversion, Agreeableness, conscientiousness, Neuroticism,Openness
# Autism Quotient : Social Skill, Attention To Detail, Communication, Imagination, Attention Switching
#Systemizing Quotient-Revised (SQ-R): Systemizing

##Correlation Matrix
correlation <- rcorr(as.matrix(data_pca))
corrplot(correlation$r, type="upper", order="hclust",diag=0,insig = "blank",addCoef.col = 'black',tl.srt=45,tl.col="black")

## Actual PCA
#Standardize the data
data_pca <- scale(data_pca)
##First run of PCA to determine how many components to keep
pca1 <- psych::principal(data_pca, nfactors = 11, rotate = "none")
summary(pca1)
round(pca1$values,2) # we have 6 values around 1 or bigger, so we keep 6, ~Kaiser criterion
eigenvalues <- pca1$values
# Create a scree plot PCA1
plot(eigenvalues, type = "b", main = "Scree Plot", xlab = "Component Number", ylab = "Eigenvalue", pch = 19)
abline(h = 1, col = "red", lty = 2)  # Adds a horizontal line at eigenvalue = 1

##Second round of PCA
pca2 <- psych::principal(data_pca, nfactors = 6, rotate = "varimax", scores = TRUE) 
print(pca2)

#Openesss loads into RC1 & RC3 almost the same

#RC1 Negative Attention switching, Positive extraversion (openess)
#RC2 Social Skill, Communication
#RC3 Imagination
#RC4 Attention to Detail, Systemizing
#RC5 Neuroticsm
#RC6 Imagination, Agreeableness, Conscientiousness (openess)
#plot

print(pca2)
pca2_prcomp <- prcomp(pca2$scores)
fviz_pca_biplot(pca2_prcomp,label="var", repel = TRUE)
data_sub_pca <- cbind(data_sub[,],pca2$scores)

#############################################################
### K-means Cluster Analysis
data_cluster <- data_sub %>% dplyr::select( TotalSocialSkill, TotalAttentionToDetail,TotalAttentionSwitching, TotalCommunication, Totalimagination, TotalSystemizing,  TotalExtraversion, TotalAgreeableness, TotalConscientiousness, TotalNeuroticism, TotalOpeness, word4.ACC, Proficiency)
data_kmeans <- scale(data_cluster[,1:12]) #
#data_cluster <- data_sub %>% dplyr::select( word4.ACC, TotalSocialSkill, Totalimagination) # experimenting with only significant predictors
#data_kmeans <- scale(data_cluster[,1:3])
distance <- get_dist(data_kmeans)

fviz_nbclust(data_kmeans, kmeans, method = "wss")
fviz_nbclust(data_kmeans, kmeans, method = "silhouette")
gap_stat <- clusGap(data_kmeans, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

# After testing those recomended ones I chose the outcome of silhouette as these clusters did not overlap
k2 <- kmeans(data_kmeans, centers = 2, nstart = 25)
fviz_cluster(k2, data = data_kmeans,geom = "point",main = "K-Means Cluster",ggtheme= theme_minimal())
str(k2)

data_cluster$cluster <- k2$cluster # adding the cluster to the data
cluster_summary <- aggregate(. ~ cluster, data = data_cluster, FUN = function(x) c(mean = mean(x)))
cluster_summary

### K-means Cluster Analysis with PCA
data_cluster_pca<- data_sub_pca %>% dplyr::select( word4.ACC,RC1,RC2,RC3,RC4,RC5,RC6)
data_kmeans_pca <- scale(data_cluster_pca)
distance <- get_dist(data_kmeans_pca)

fviz_nbclust(data_kmeans_pca, kmeans, method = "wss")
fviz_nbclust(data_kmeans_pca, kmeans, method = "silhouette")
gap_stat <- clusGap(data_kmeans_pca, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

cluster_pca <- kmeans(data_kmeans_pca, centers = 2, nstart = 25)
fviz_cluster(cluster_pca, data = data_kmeans_pca,geom = "point",main = "K-Means Cluster with pca",ggtheme= theme_minimal())
str(cluster_pca)
#############################################################
### Imagination and Social skill 
###Simple model with no random slopes from paper
m.acc <- glm(accuracy ~ TotalSocialSkill+TotalAttentionToDetail+TotalAttentionSwitching+TotalCommunication+Totalimagination+
                 TotalSystemizing+TotalExtraversion+TotalAgreeableness+TotalConscientiousness+TotalNeuroticism+TotalOpeness,
               data = data_sub, family = binomial)
summary(m.acc)

#### ...now only significant predictors,
m.acc1 <- glm(accuracy ~ TotalSocialSkill+TotalAttentionToDetail+Totalimagination+  #problem: Attention to Detail & Systemizing are correlated
                  TotalSystemizing,
                data = data_sub, family = binomial)
summary(m.acc1)

#### ...now only significant predictors, but Attention to Detail & Systemizing are correlated -> replace by their PC

m.acc1_pca <- glm(accuracy ~ TotalSocialSkill+Totalimagination+RC4,
                data = data_sub_pca, family = binomial)
summary(m.acc1_pca)
####Combined model 
m.acc_combined <- glm(accuracy ~ Proficiency*Totalimagination + Proficiency*TotalSocialSkill +Proficiency*TotalAttentionToDetail + Proficiency*TotalSystemizing,
                        data = data_sub, family = binomial)
summary(m.acc_combined)

m.acc_combined_pca <- glm(accuracy ~ Proficiency*Totalimagination + Proficiency*TotalSocialSkill +Proficiency*RC4,
                      data = data_sub_pca, family = binomial)
summary(m.acc_combined_pca)

# Combined using their approach:
m.acc_combined <- glmer(accuracy ~ Proficiency*Totalimagination + Proficiency*TotalSocialSkill +
                          
                          (1+Totalimagination|item) + (1+Totalimagination|subj)+
                          (1+TotalSocialSkill|item) + (1+TotalSocialSkill|subj),
                        data = data_sub, family = binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list( maxfun = 50000)))
summary(m.acc_combined)
#I think the random slopes on items are negligible as they should not induce much variation by the nature of the experiment
m.acc_combined1 <- glmer(accuracy ~ Proficiency*Totalimagination + Proficiency*TotalSocialSkill +
                          (1+Totalimagination|subj) + (1+TotalSocialSkill|subj),
                        data = data_sub, family = binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list( maxfun = 50000)))
summary(m.acc_combined1)


# Significance level (alpha)
alpha <- 0.05 /9

# Calculate z-values
z_values <- summary(m.acc_combined)$coefficients[, "z value"]

# Find the critical z-value
critical_z <- qnorm(alpha/2,lower.tail = FALSE)  # For a two-tailed test

# Compare z-values to critical z-value
significant_coefficients <- abs(z_values) > critical_z

# Print significant coefficients
significant_coefficients



# Dividing data on the median of each variable
data_sub$social <- ifelse(data_sub$TotalSocialSkill<5, "social","not social")
data_sub$imagination <- ifelse(data_sub$Totalimagination>4, "good imagination","bad imagination")
data_sub$detail <- ifelse(data_sub$TotalAttentionToDetail>5, "good detail attention","bad detail attention")
data_sub$systemizing <- ifelse(data_sub$TotalSystemizing>74, "systemizing","not systemizing")
data_sub$accuracy <- as.numeric(data_sub$accuracy)


# Plot Social skills
acc.summary_social <- summarySEwithin(data_sub, measurevar = "accuracy", withinvars = c( "Proficiency","social"))
acc.summary_social$accuracy <- acc.summary_social$accuracy-1 # Subtract 1 to align with the original accuracy scale

pd <- position_dodge(0.4)
ggplot(acc.summary_social, aes(x=Proficiency, y=accuracy, fill=social)) +
  geom_bar(stat = "identity", position="dodge", colour="black", width = 0.4) +
  scale_fill_manual(name="Social Skills", 
                    breaks=c("social", "not social"),
                    labels=c( "More social","Less social"),values=c("slateblue3", "indianred2", "slateblue3", "indianred2") )+ 
  ggtitle("Average Implicature derivation rate: Social Skills") +
  geom_errorbar(aes(ymin=accuracy-ci, ymax=accuracy+ci), width=.15, position = pd) +
  coord_cartesian(ylim=c(0,1))+
  scale_y_continuous(breaks=c(seq(0,1,0.1)))+
  xlab("Proficiency") +
  ylab("0 = Pragmatic                       1 = Logical") + 
  theme(axis.title=element_text(face="bold", size="12"),
        axis.text=element_text(face="bold", size="12"))
# Plot imagination
acc.summary_imagination <- summarySEwithin(data_sub, measurevar = "accuracy", withinvars = c( "Proficiency","imagination"))
acc.summary_imagination$accuracy <- acc.summary_imagination$accuracy-1
pd <- position_dodge(0.4)
ggplot(acc.summary_imagination, aes(x=Proficiency, y=accuracy, fill=imagination)) +
  geom_bar(stat = "identity", position="dodge", colour="black", width = 0.4) +
  scale_fill_manual(name="Imagination", 
                    breaks=c("good imagination","bad imagination"),
                    labels=c("High", "Low"),values=c("slateblue3", "indianred2", "slateblue3", "indianred2") )+ 
  ggtitle("Average Implicature derivation rate: Imagination") +
  geom_errorbar(aes(ymin=accuracy-ci, ymax=accuracy+ci), width=.15, position = pd) +
  coord_cartesian(ylim=c(0,1))+
  scale_y_continuous(breaks=c(seq(0,1,0.1)))+
  xlab("Proficiency") +
  ylab("0 = Pragmatic                       1 = Logical") + 
  theme(axis.title=element_text(face="bold", size="12"),
        axis.text=element_text(face="bold", size="12"))
# Plot Detail
acc.summary_detail <- summarySEwithin(data_sub, measurevar = "accuracy", withinvars = c( "Proficiency","detail"))
acc.summary_detail$accuracy <- acc.summary_detail$accuracy-1

pd <- position_dodge(0.4)
ggplot(acc.summary_detail, aes(x=Proficiency, y=accuracy, fill=detail)) +
  geom_bar(stat = "identity", position="dodge", colour="black", width = 0.4) +
  scale_fill_manual(name="Attention to detail", 
                    breaks=c("good detail attention","bad detail attention"),
                    labels=c("High", "Low"),values=c("slateblue3", "indianred2", "slateblue3", "indianred2") )+ 
  ggtitle("Average Implicature derivation rate: Attention to Detail") +
  geom_errorbar(aes(ymin=accuracy-ci, ymax=accuracy+ci), width=.15, position = pd) +
  coord_cartesian(ylim=c(0,1))+
  scale_y_continuous(breaks=c(seq(0,1,0.1)))+
  xlab("Proficiency") +
  ylab("0 = Pragmatic                       1 = Logical") + 
  theme(axis.title=element_text(face="bold", size="12"),
        axis.text=element_text(face="bold", size="12"))

# Plot Systemizing
acc.summary_systemizing <- summarySEwithin(data_sub, measurevar = "accuracy", withinvars = c( "Proficiency","systemizing"))
acc.summary_systemizing$accuracy <- acc.summary_systemizing$accuracy-1

pd <- position_dodge(0.4)
ggplot(acc.summary_systemizing, aes(x=Proficiency, y=accuracy, fill=systemizing)) +
  geom_bar(stat = "identity", position="dodge", colour="black", width = 0.4) +
  scale_fill_manual(name="Systemizing", 
                    breaks=c("systemizing","not systemizing"),
                    labels=c("High", "Low"),values=c("slateblue3", "indianred2", "slateblue3", "indianred2") )+ 
  ggtitle("Average Implicature derivation rate: Systemizing") +
  geom_errorbar(aes(ymin=accuracy-ci, ymax=accuracy+ci), width=.15, position = pd) +
  coord_cartesian(ylim=c(0,1))+
  scale_y_continuous(breaks=c(seq(0,1,0.1)))+
  xlab("Proficiency") +
  ylab("0 = Pragmatic                       1 = Logical") + 
  theme(axis.title=element_text(face="bold", size="12"),
        axis.text=element_text(face="bold", size="12"))


#Has singularity issues, now try again with PCA values

m.acc <- glmer(accuracy ~ RC1+RC2+RC3+RC4+RC5+RC6+
                 
                 (1|item),
               data = data_sub_pca, family = binomial)
summary(m.acc)
coefplot(m.acc)