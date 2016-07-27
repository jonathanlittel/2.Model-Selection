# Model testing
library(lmtest)
library(pscl)
library(ggplot2)
library(caret)
library(pROC)
# Load rap_data.csv file
filename <- "https://rootcapital.box.com/shared/static/d6q5d6pfnvzao08x3ev7af4rj6bm6hve.csv"
df.rap <- read.csv(filename, header=TRUE, sep=",")

#####################################################
# Split data into training and test set             #
# Important that this remains consistent throughout #
#####################################################
uniqueIDs <- unique(df.rap$LoanID)
sample_size <- floor(0.80 * length(uniqueIDs))
set.seed(9)
LoanIDtrain <- sample(uniqueIDs, sample_size)
# LoanIDtest <- uniqueIDs[-LoanIDtrain] # Not necessary, better to use negative subset
df.train <- df.rap[df.rap$LoanID %in% LoanIDtrain,]
df.test <- df.rap[!df.rap$LoanID %in% LoanIDtrain,]

# Remove active loans (active is set in rap_cleaning_2 as > 7/1/14)
# Note that this is *keeping* 7 NA closed date loans: sum(is.na(df.rap$active))
# And don't subset on non-active if you want the current portfolio balance:
# sum(na.omit(df.rap$June2016Balance[df.rap$last_year==1]))
# df.rap <- subset(df.rap, active==0)

####################################
## Analyzing and comparing models ##
####################################

# Set cutoff
cutoff <- 0.2

# Make a ROC curve
roc18c <- roc(df.rap$WriteoffsDummy, df.rap$wo_cut)
plot(roc18c)
roc18c$auc


pred19 <- predict(glm19, df.train, type="response")
df.train$predicted_d_19 <- predict(glm19, df.train, type="response")
pred_d_binary_19 <- ifelse(pred19>cutoff,1,0)
confusionMatrix(pred_d_binary_19, df.train$default)


df.rap$predicted_wo <- predict(glm18cWO, df.rap, type="response")
df.rap$predicted_default <- predict(glm18c, df.rap, type="response")
write.csv(df.rap, 
	"C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/Loss Distribution Simulation/predicted_default.csv")

# Sensitivity (also called the true positive rate, or the recall in some fields) measures the proportion of positives that are correctly identified as such (e.g., the percentage of sick people who are correctly identified as having the condition).
# Specificity (also called the true negative rate) measures the proportion of negatives that are correctly identified as such (e.g., the percentage of healthy people who are correctly identified as not having the condition).

pred18c_test <- predict(glm18c, df.test, type="response")
pred18cx_test <- ifelse(pred18c_test>cutoff,1,0)
confusionMatrix(pred18cx_test, df.test$default)

library(popbio)
pred18c[NA] <- 0
logi.hist.plot(pred18c,df.train$default,boxp=FALSE,type="hist",col="gray")   # this needs to have NAs removed
logi.hist.plot(na.omit(pred18c),df.train$default[!is.na(pred18c)],boxp=FALSE,type="hist",col="gray")  

library(ggplot2)
df.train$pred18c_binary <-pred18c_binary
ggplot(df.train, aes(x=pred18c_binary, y=default)) + geom_point() + stat_smooth(method="glm", family="binomial", se=FALSE)

	
pR2(glm18)
pR2(glm18a)
pR2(glm18b)
pR2(glm18c)
pR2	(glm18d)  