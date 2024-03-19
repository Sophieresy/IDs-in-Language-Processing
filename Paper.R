### data analysis script

#"what second-language speakers can tell us about pragmatic processing"



options(scipen=999)

## load packages
library(ggplot2)
library(lme4)
library(here)
library(plyr)
library(haven)
library(dplyr)
library(tidyr)
library(labelled)
library(MASS)
library(Rmisc)


### read in data 

data <- read.table(here::here("data", "data_L2_scalars.csv"), header=T, sep= ",")



### look at accuracy by participant excluding critical condition
### check for accuracy in fillers
data_acc <- subset(data, Condition!="T1")

accuracy <- aggregate(accuracy ~ Subject, data_acc, mean)

accuracy
accuracy$score <- accuracy$accuracy
accuracy$accuracy <- NULL
data <- left_join(data, accuracy, by.x=c("subj"))

### remove participants with less than 70% accuracy 

data <- subset(data,score>0.7)

##### check nr of participants 

length(unique(data$Subject))

# plot accuracy 

acc.summary <- summarySEwithin(data, measurevar = "accuracy", withinvars = c( "Condition","Proficiency"))
acc.summary
# plot

pd <- position_dodge(0.4)
ggplot(acc.summary, aes(x=Condition, y=accuracy, fill=Proficiency)) +
  geom_bar(stat = "identity", position="dodge", colour="black", width = 0.4) +
  scale_fill_manual(name="Proficiency", 
                    breaks=c("low", "high"),
                    labels=c("Low", "High"),values=c("slateblue3", "indianred2", "slateblue3", "indianred2") )+ 
  ggtitle("Response accuracy for forced-choice task (with 95% CI bars)") +
  geom_errorbar(aes(ymin=accuracy-ci, ymax=accuracy+ci), width=.15, position = pd) +
  coord_cartesian(ylim=c(0,1))+
  scale_y_continuous(breaks=c(seq(0,1,0.1)))+
  xlab("Condition") +
  ylab("Percentage of correct responses") + 
  theme(axis.title=element_text(face="bold", size="12"),
        axis.text=element_text(face="bold", size="12"))




### split T1 into logical and pragmatic responses
data$Condition <- as.character(data$Condition)
data$condition_new <- ifelse(data$Condition=="T1" & data$word4.RESP=="TRUE","T1_logic",
                             ifelse(data$Condition=="T1" & data$word4.RESP=="FALSE","T1_pragmatic", data$Condition))

data$condition_new <- as.factor(as.character(data$condition_new))

# how many subjects are only logical or only pragmatic?

data$accuracy_numeric <- as.numeric(as.character(data$accuracy))


acc.summary_sub <- summarySEwithin(data, measurevar = "accuracy_numeric", withinvars = c( "condition_new","Subject","Proficiency"))
acc.summary_sub

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

#model 

m.acc1 <- glmer(accuracy ~ Proficiency*Condition +
                  (1|subj)+
                  (1+Proficiency|item),
               data = data, family = binomial, control = glmerControl(calc.derivs = FALSE))
summary(m.acc1)


#### shows main effect of proficiency, z-score =-5.873
### significant difference between T1 and all other conditions (all z-scores > 10)


#### follow up model looking only at T1

data_t1 <- subset(data, Condition=="T1")
data_t1 <- droplevels(data_t1)

contrasts(data_t1$Proficiency) <- contr.treatment(2)
contrasts(data_t1$Proficiency)

m.acc2 <- glmer(accuracy ~ Proficiency +
                  (1|subj)+
                  (1+Proficiency|item),
                data = data_t1, family = binomial, control = glmerControl(calc.derivs = FALSE))
summary(m.acc2)

##### shows significant difference between proficiency groups for T1 responses, with z=2.914


################################################################
###########  analysis for reaction times
###########


###remove incorrect trials 
data1 <- subset(data, correct_new==1)


#### remove only the most obvious of outliers 

data2 <- subset(data1, data1$word4.RT>400)
data2 <- subset(data2, data2$word4.RT<6000)

###percentage removed is less than 3%
10216/10436
#### check for normality of residuals of simple model

m <- lm(word4.RT~ 1, data=data1) 
qqnorm(residuals(m)) 
qqline(residuals(m), col = "red")
Box = boxcox(m,lambda = seq(-6,6,0.1))   
Cox = data.frame(Box$x, Box$y)
Cox2 = Cox[with(Cox, order(-Cox$Box.y)),]
Cox2[1,]
lambda = Cox2[1, "Box.x"]


### residuals are not normally distributed, take log, following box & cox (1964)

data2$log <- log(data2$word4.RT)


###summarize data 

summary <- summarySEwithin(data2, measurevar = "word4.RT", withinvars = c( "condition_new"), betweenvars = c("Proficiency"))

summary_log <- summarySEwithin(data2, measurevar = "log", withinvars = c( "condition_new"), betweenvars = c("Proficiency"))

# plot the raw reaction times

pd <- position_dodge(0.4)
ggplot(summary, aes(x=condition_new, y=word4.RT, fill=Proficiency)) +
  geom_bar(stat = "identity", position="dodge", colour="black", width = 0.4) +
  theme_bw() + 
  scale_fill_manual(name="Proficiency", 
                    breaks=c("low", "high"),
                    labels=c("Low", "High"),values=c("slateblue3", "indianred2", "slateblue3", "indianred2") )+ 
  ggtitle("Reaction times of correct responses (with 95% CI)") +
  geom_errorbar(aes(ymin=word4.RT-ci, ymax=word4.RT+ci), width=.2, position = pd) +

  xlab(" Condition
       ") +
  ylab("
       reaction times (in ms) 
       ") + 
  coord_cartesian(ylim=c(0,2800))+
  scale_y_continuous(breaks=c(seq(0,2800,200)))+
  theme(axis.title=element_text(face="bold", size="12"),
        axis.text=element_text(face="bold", size="12"))


# plot the log reaction times

pd <- position_dodge(0.4)
ggplot(summary_log, aes(x=condition_new, y=log, fill=Proficiency)) +
  geom_bar(stat = "identity", position="dodge", colour="black", width = 0.4) +
  theme_bw() + 
  ggtitle("Reaction times (with 95% CI)") +
  geom_errorbar(aes(ymin=log-ci, ymax=log+ci), width=.2, position = pd) +
  coord_cartesian(ylim=c(7,7.7))+
  scale_y_continuous(breaks=c(seq(7,7.7,0.1)))+
  xlab(" Condition
       ") +
  ylab("
       reaction times (in log-ms) 
       ") + 
  theme(axis.title=element_text(face="bold", size="12"),
        axis.text=element_text(face="bold", size="12"))




#### fit a model

### look at only T1, T4, and T5

data3 <- subset(data2, Condition=="T1"|Condition=="T4" | Condition=="T5")

data3 <- droplevels(data3)
##### change baseline to pragmatic condition and high proficiency 
data3$condition_new <- as.factor(data3$condition_new)
data3$Proficiency <- as.factor(data3$Proficiency)

data3$condition_new<- relevel(data3$condition_new, "T1_logic")
data3$Proficiency<- relevel(data3$Proficiency, "high")

levels(data3$condition_new)
###other preliminary stuff
data3$subj <- as.factor(data3$Subject)

contrasts(data3$Proficiency)
### maximal model 
m1 <- lmer(log ~ condition_new*Proficiency + Trial +
             (1+condition_new*Proficiency|item)+
             (1+condition_new|subj), data3,
           control = lmerControl(calc.derivs = FALSE),REML=F)
summary(m1)

#### m1 shows a t-value of 2.203 for the 
# interaction between proficiency and types of responses to T1

m0 <- lmer(log ~ condition_new+Proficiency + Trial +
             (1+condition_new*Proficiency|item)+
             (1+condition_new|subj), data3,
           control = lmerControl(calc.derivs = FALSE),REML=F)

##### get p-values by doing model comparison 

anova(m0,m1)

### models are significantly different, with p=0.01947


### refit the model but collapsing t2 and t4 to compare with t1 logic

data3_al <- subset(data2, Condition=="T1"|Condition=="T4" | Condition=="T2")

levels(data3_al$condition_new)
data3_al$condition_new<- relevel(data3_al$condition_new, "T1_logic")

data3_al$condition_new_new <- ifelse(data3_al$Condition=="T2"|data3_al$Condition=="T4", "T24", data3_al$condition_new)
data3_al$condition_new_new <- as.factor((as.character(data3_al$condition_new_new)))

levels(data3_al$condition_new_new)

data3_al <- droplevels((data3_al))
data3_al$Proficiency <- as.factor(data3_al$Proficiency)


###other preliminary stuff
data3_al$subj <- as.factor(data3_al$Subject)

contrasts(data3_al$Proficiency)

contrasts(data3_al$Proficiency) <- contr.sum(2)/2
contrasts(data3_al$Proficiency)


m1_al <- lmer(log ~ condition_new_new*Proficiency + Trial+
             (1+condition_new_new*Proficiency|item)+
             (1+condition_new_new|subj), data3_al, REML=F)
summary(m1_al)

#### shows no significant difference between T1_logic and T4 and T2 combined



######### compare T1 to T3, T5 and T6

### look at only T1, T4, and T5

data4 <- subset(data2, Condition=="T1"|Condition=="T3" | Condition=="T5" | Condition=="T6")

data4 <- droplevels(data4)
##### change baseline to pragmatic condition and high proficiency 
data4$condition_new <- as.factor(data4$condition_new)
data4$Proficiency <- as.factor(data4$Proficiency)

data4$condition_new<- relevel(data4$condition_new, "T1_pragmatic")
contrasts(data4$Proficiency) <- contr.sum(2)/2


###other preliminary stuff
data4$subj <- as.factor(data4$Subject)

contrasts(data4$Proficiency)
### maximal model 
m4 <- lmer(log ~ condition_new*Proficiency + Trial +
             (1+condition_new*Proficiency|item)+
             (1+condition_new|subj), data4,
           control = lmerControl(calc.derivs = FALSE))
summary(m4)


### shows significant difference between T1_pragmatic and all other 
### control conditions that had a FALSE answer (all t-values larger than 8)


####################################################################
####################################################################
##########################
##########################
####################################################################
########################
######### EXPLORATORY ANALYSIS looking at individual differences andv their effect on implicature derivation
########
#####

####focus on T1 responses only

data_sub <- subset(data, Condition=="T1")

data_sub <- droplevels(data_sub)
summary(data_sub)
contrasts(data_sub$Proficiency) <- contr.sum(2)/2


###simple model with no random slopes
m.acc <- glmer(accuracy ~ TotalSocialSkill+TotalAttentionToDetail+TotalAttentionSwitching+TotalCommunication+Totalimagination+
                  TotalSystemizing+TotalExtraversion+TotalAgreeableness+TotalConscientiousness+TotalNeuroticism+TotalOpeness+
                 
               (1|item),
            data = data_sub, family = binomial)
summary(m.acc)


#### now only add random slopes to significant predictors 

m.acc1 <- glmer(accuracy ~ TotalSocialSkill+TotalAttentionToDetail+Totalimagination+
                 TotalSystemizing+
                 
                 (0+TotalSocialSkill+TotalAttentionToDetail+Totalimagination+
                    TotalSystemizing|item),
               data = data_sub, family = binomial)
summary(m.acc1)

### model shows that only total social skill and imagination are significant at p<0.002

### now control for interaction of the two with proficiency in individual models
### we re-introduce random variation by subject, since we got rid of all of the other by-subject variables
### the two models serve to account for two different random effects structures, one for imagination
## and one for social skill
 contrasts(data_sub$Proficiency)

m.acc_imagination <- glmer(accuracy ~ Proficiency*Totalimagination+TotalSocialSkill*Proficiency+
                    
                  (1+Totalimagination||item) +  (1+Totalimagination||subj),
                data = data_sub, family = binomial)
summary(m.acc_imagination)

#### model shows that imagination is not significant once controlling for the interaction
### and adding the maximal random effect structure for imagination
### Totalimagination p= 0.007381 


m.acc_social <- glmer(accuracy ~ Proficiency*TotalSocialSkill+ Proficiency*Totalimagination+
                    
                        (0+TotalSocialSkill|item) +  (0+TotalSocialSkill|subj),
                      data = data_sub, family = binomial)
summary(m.acc_social)

#this model shows significant effect of social skill 
### TotalSocialSkill p=0.0000673

#### now, we fit a model to test only for the interaction between social skill and proficiency
### adding an adequate random effects structure for this particular effect

### first, make the predictor Proficiency numeric in order to remove the 
### corelations of random effects

data_sub$prof.num <- sapply(data_sub$Proficiency,function(i) contr.sum(2)[i,])

### now we fit a model tailored for finding this interaction

m.acc_social_inter <- glmer(accuracy ~ Proficiency*TotalSocialSkill+
                        (0+prof.num*TotalSocialSkill||item) +  (0+prof.num*TotalSocialSkill||subj),
                      data = data_sub, family = binomial)
summary(m.acc_social_inter)

##### interaction is not significant according to our strict threshold, with p=0.008095

##### visualize  model results 

###create categorical variable for social skills 
#### note that the social skill subscale is counterintuitive: lower values means higher social skills and viceversa
data_sub$social <- ifelse(data_sub$TotalSocialSkill<5, "social","not social")

data_sub$accuracy <- as.numeric(data_sub$accuracy)

acc.summary_social <- summarySEwithin(data_sub, measurevar = "accuracy", withinvars = c( "Proficiency","social"))
acc.summary_social$accuracy <- acc.summary_social$accuracy-1

# plot
pd <- position_dodge(0.4)
ggplot(acc.summary_social, aes(x=Proficiency, y=accuracy, fill=social)) +
  geom_bar(stat = "identity", position="dodge", colour="black", width = 0.4) +
  scale_fill_manual(name="Social Skills", 
                    breaks=c("not social", "social"),
                    labels=c("Less social", "More social"),values=c("slateblue3", "indianred2", "slateblue3", "indianred2") )+ 
  ggtitle("Average Implicature derivation rate (with 95% CI bars)") +
  geom_errorbar(aes(ymin=accuracy-ci, ymax=accuracy+ci), width=.15, position = pd) +
  coord_cartesian(ylim=c(0,1))+
  scale_y_continuous(breaks=c(seq(0,1,0.1)))+
  xlab("Proficiency") +
  ylab("0 = Pragmatic                       1 = Logical") + 
  theme(axis.title=element_text(face="bold", size="12"),
        axis.text=element_text(face="bold", size="12"))




