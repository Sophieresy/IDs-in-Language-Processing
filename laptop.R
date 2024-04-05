library('Hmisc')
library('corrplot')
library('ggplot2')
library('dplyr')
library('factoextra')
# load data

personality<- read.table(here::here("data", "data_L2_scalars.csv"), header=T, sep= ",")

personality <- personality %>% distinct(Subject, .keep_all = TRUE)
personality <- personality %>% select(TotalSocialSkill, TotalAttentionToDetail,TotalAttentionSwitching, TotalCommunication, Totalimagination, TotalSystemizing,  TotalExtraversion, TotalAgreeableness, TotalConscientiousness, TotalNeuroticism, TotalOpeness)


###correlation Matrix
correlation <- rcorr(as.matrix(personality))
corrplot(correlation$r, type="lower", order="hclust",diag=0,insig = "blank",addCoef.col = 'black')

###pca nach UNI
#pca1 <- psych::principal(personality, nfactors = 7, rotate = "none")

#summary(pca1)

#round(pca1$values,2)
#eigenvalues <- pca1$values
#plot(eigenvalues, type = "b", xlab = "Principal Component", ylab = "Eigenvalue", 
 #    main = "Scree Plot", pch = 19)
#abline(h = 1, col = "red", lty = 2) # Optional: Kaiser criterion line
#biplot(pca1)

#pca tutorial
colSums(is.na(personality))# do we have NA values?
personalityPCA <- prcomp(personality, scale =TRUE) # i need to standardize my data

summary(personalityPCA)
names(personalityPCA) #elemets
personalityPCA$sdev #std dev 
#std dev & mean of variables
personalityPCA$center
personalityPCA$scale

#Principal componetnt scores
personalityPCA$x

#scree plot
fviz_eig(personalityPCA, 
         addlabels = TRUE)

fviz_pca_biplot(personalityPCA,
                label="var")

