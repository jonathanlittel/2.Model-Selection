# Prompt to set the working directory
# work_dir <- choose.dir(caption="Choose the folder to load the data file ")
# setwd(workdir) 

setwd("C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/Loss Model/2.Model Selection")

# Read data 
filename <- "https://rootcapital.box.com/shared/static/d6q5d6pfnvzao08x3ev7af4rj6bm6hve.csv"
df.rap <- read.csv(filename, header=TRUE, sep=",")

# Remove active loans (active is set in rap_cleaning_2 as > 7/1/14)
# Note that this is keeping 7 NA closed date loans: sum(is.na(df.rap$active))
df.rap <- df.rap[df.rap$active==0,]

uniqueIDs <- unique(df.rap$LoanID)
sample_size <- floor(0.80 * length(uniqueIDs))
set.seed(9)
LoanIDtrain <- sample(uniqueIDs, sample_size)
LoanIDtest <- uniqueIDs[-LoanIDtrain]

# Check that there is no overlap in the train/test sets
intersect(LoadIDtest, LoanIDtrain)

df.train <- df.rap[df.rap$LoanID %in% LoanIDtrain,]
df.test <- df.rap[!df.rap$LoanID %in% LoanIDtrain,]

# Logit model
model1 <- "default ~ TOTAL.ASSETS + EBITDA.Margin. + Total.Asset.Turnover + Total.Liabilities.Total.Assets"
glm1 <- glm(model1, data=df.rap, family ="binomial", na.action = na.exclude)
summary(glm1)

model2 <- "default ~ max_pct_per_buyer + log(TOTAL.ASSETS) + Lending.Region + Depth.of.Management "
glm2 <- glm(default ~ max_pct_per_buyer + log(TOTAL.ASSETS) + Lending.Region + Depth.of.Management, data=df.rap, family ="binomial", na.action = na.exclude)
summary(glm2)


# Using variables in risk rating
model3 <- "default ~ Sales + factor(unique_segments) + Depth.of.Management + Product.Recognition + Financial.Flexibility + Financial.Strat.Quality + Cash.Conversion.Cycle. +
EBITDA.Total.Debt.Service + Funded.Debt.EBITDA...Cash.Flow.Leverage"
glm3 <- glm(model3, data=df.rap, family="binomial", na.action=na.exclude)
summary(glm3)

# 
model4 <- "default ~ Sales + factor(unique_segments) + Cash.Conversion.Cycle. +
EBITDA.Total.Debt.Service + Funded.Debt.EBITDA...Cash.Flow.Leverage + "
glm4 <- glm(model4, data=df.rap, family="binomial", na.action=na.exclude)
summary(glm4)
# Check goodness of fit  # http://mathewanalytics.com/2015/09/02/logistic-regression-in-r-part-two/
library(pscl)
pR2(glm3)


model4 <- "default ~ TOTAL.ASSETS + Sales + unique_segments + Cash.Conversion.Cycle. +
EBITDA.Total.Debt.Service + Funded.Debt.EBITDA...Cash.Flow.Leverage + factor(Loan.Type) +EBITDA.Margin. + Working.Capital + COGS.Margin"
glm4 <- glm(model4, data=df.train, family="binomial", na.action=na.exclude)
summary(glm4)

#create a truth vector of default results from training set
S <- df.train$default == 1
#Generate predictions for training data using the predict method of the logistic model. Note that this produces a fair amount of NAs
predTrain4 <- predict(glm4, type="response")
#compute training error use an outcome cutoff at 0.5
notTrained <- sum(is.na(predTrain4))
S <- df.train$default[!is.na(predTrain4)] == 1
 training_error <-sum((predTrain4[!is.na(predTrain4)] >= 0.15) != S[!is.na(predTrain4)] )/ (nrow(df.train) - notTrained)  # need to filter both S and df.train for NAs
 training_error
 1-training_error


thresh  <- 0.5            # threshold for categorizing predicted probabilities
predFac <- cut(predTest4, breaks=c(-Inf, thresh, Inf), labels=c("No Default", "Default"))

yFac <- df.train$default
cTab    <- table(yFac[idxTst], predTrain4, dnn=c("actual", "predicted")) # yFac[idxTst] < - need to fix that part
# > addmargins(cTab)
#       predicted
# actual lo hi Sum
#    lo  12  4  16
#    hi   5  9  14
#    Sum 17 13  30

#Generate predictions for training data using the predict method of the logistic model
predTest4 <- predict(glm4, df.test, type="response")

library(caret)
# confusionMatrix(pred4, 0)

# Plotting

# plot(predTest4, df.test$default)
# lines(predTest4, df.test$default)
# abline(predTest4)
# curve(predict(glm4, type="response"))


# library(popbio)
logi.hist.plot(predTest4,df.test$default,boxp=FALSE,type="hist",col="gray")  

library(ggplot2)
ggplot(df.test, aes(x=predTest4, y=default)) + geom_point() + stat_smooth(method="glm", family="binomial", se=FALSE)