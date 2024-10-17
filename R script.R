
# 1. Load data
df <- read.csv("df.csv" , header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, check.names = FALSE)

# 2. Load libary
library(dplyr)
library(tidyverse)
library(emmeans)
library(pROC)

# 3. Calculate subgroup differences of a musicality task adjusted for age, sex, musical training and MMSE
MEDT_diff <- aov(formula = MEDT ~ as.factor(diagnosis) + Age + Sex+ MUS_TRAINING + MMSE, data = df)
summary(MEDT_diff)
test(emmeans(MEDT_diff, pairwise ~ diagnosis, var='MEDT', adjust='none'))

# 4. Calculate associations of musicality and social cognition using regression analyses adjusted for age, sex, musical training, and MMSE
SOCIAL_COG = c("XX","YY","ZZ") # create variables with the social cognition tests
resmat_proms = data.frame(matrix(NA,nrow=length(SOCIAL_COG),ncol=4)) # create dataframe
names(resmat_proms) = c('Estimate','Std Error','t value', 'Pr(>|t|)') #create column namnes
rownames(resmat_proms) = SOCIAL_COG #create row names
for(i in 1:length(SOCIAL_COG)){       #Create a for loop for all correlations of the PROMS with the social cognition tests
    eq_proms = paste(SOCIAL_COG[i], '~ PROMS + Age + Sex + MUS_TRAINING + MMSE')
    lm_proms <-summary(lm(as.formula(eq_proms), data=df, na.action = "na.exclude"))
    resmat_proms[i,1]= paste0(round(lm_proms$coefficients['PROMS','Estimate'],4))
    resmat_proms[i,2]= paste0(round(lm_proms$coefficients['PROMS','Std. Error'],4))
    resmat_proms[i,3]= paste0(round(lm_proms$coefficients['PROMS','t value'],4))
    resmat_proms[i,4]= paste0(round(lm_proms$coefficients['PROMS','Pr(>|t|)'],8))
}
View(resmat_proms) #View results
p.adjust(resmat_proms$`Pr(>|t|)`, method="fdr") #Adjust for multiple testing

# 5. Calculate area under the curve (AUC) of each musicality and social cognition test (predictor) adjusted for age, sex and education to discriminate bvFTD from AD and healthy controls (outcome)
par(pty="s") 
glm.fit_MEDT=glm(diagnosis ~ MEDT + Age + Sex + Education, data=ROC, family=binomial)
lines(ROC$MEDT, glm.fit_MEDT$fitted.values)  
MEDT_ROC <- roc(ROC$diagnosis, glm.fit_MEDT$fitted.values, plot=TRUE, legacy.axes=TRUE, col="steelblue", lwd =4, print.auc=TRUE)


