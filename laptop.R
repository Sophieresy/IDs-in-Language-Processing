library('Hmisc')
library('corrplot')
library('ggplot2')
library('dplyr')
library('factoextra')
library('ggbiplot')

# load data

personality<- read.table(here::here("data", "data_L2_scalars.csv"), header=T, sep= ",")

personality <- personality %>% distinct(Subject, .keep_all = TRUE)
personality <- personality %>% dplyr::select(TotalSocialSkill, TotalAttentionToDetail,TotalAttentionSwitching, TotalCommunication, Totalimagination, TotalSystemizing,  TotalExtraversion, TotalAgreeableness, TotalConscientiousness, TotalNeuroticism, TotalOpeness)


###correlation Matrix
correlation <- rcorr(as.matrix(personality))
corrplot(correlation$r, type="lower", order="hclust",diag=0,insig = "blank",addCoef.col = 'black')

###pca nach UNI
personality <- scale(personality)
pca1 <- psych::principal(personality, nfactors = 11, rotate = "none")
summary(pca1)

round(pca1$values,2) # we have 6 values around 1 or bigger, so we keep 6
eigenvalues <- pca1$values

# Create a scree plot
plot(eigenvalues, type = "b", main = "Scree Plot", xlab = "Component Number", ylab = "Eigenvalue", pch = 19)
abline(h = 1, col = "red", lty = 2)  # Adds a horizontal line at eigenvalue = 1

#second round of pca
pca2 <- psych::principal(personality, nfactors = 6, rotate = "varimax", scores = TRUE) 
print(pca2)
# openesss loads into RC1 & RC3 almoast the same

#RC1 Agreeableness, Conscientiousness (openess)
#RC2 Social Skill, Communication
#RC3 Negative Attention switching, Positive extraversion (openess)
#RC4 Attention to Detail, Systemizing
#RC5 Neuroticsm
#RC6 Imagination
#plot
colnames(pca2$loadings) <- c("Agreeable & Concious (open)", "-Attention switching & + extraversion (open)", "Social Skill & Communication", "Attention to Detail & Systemizing", "Imagination", "Neuroticsm")
print(pca2)
pca2_prcomp <- prcomp(pca2$scores)
fviz_pca_biplot(pca2_prcomp,label="var", repel = TRUE)

