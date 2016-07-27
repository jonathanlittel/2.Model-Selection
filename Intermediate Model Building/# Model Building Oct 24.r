################
#Build new models
library(pROC)
library(pscl)
library(stargazer)


wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/Loss Model/1.Dataset Cleaning and Prep"
setwd(wd)
#filename <-  "https://rootcapital.box.com/shared/static/d7q5d7pfnvzao08x4ev7af4rj7bm7hve.csv"
filename <-  "rap_data.csv"
df.rap <- read.csv(filename, header=TRUE, sep=",")

wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/Loss Model/2.Model Selection"
setwd(wd)

# Remove active loans (active is set in rap_cleaning_4 as > 7/1/14)
# Note that this is *keeping* 7 NA closed date loans: sum(is.na(df.rap$active))
df.rap <- subset(df.rap, active==0)
df.rap <- subset(df.rap, last_year==1)

## Move this to the data cleaning file:
df.rap$debt_service_to_ebitda[!is.finite(df.rap$debt_service_to_ebitda)] <- 0
#TODO fix gross margin variation
df.rap <- df.rap[df.rap$debt_service_to_ebitda>-100,]  # Should probably look in to these
df.rap <- df.rap[df.rap$Debt.Service.Coverage.Ratio..>-100,] # Rather than dropping them
# This is in the cleanign file already, just need to re-run
df.rap$LoanID[df.rap$Cash.Conversion.Cycle.>360]
df.rap$Cash.Conversion.Cycle.[df.rap$Cash.Conversion.Cycle.>360]  <-  (( df.rap$Cash.Conversion.Cycle[df.rap$Cash.Conversion.Cycle.>360]  - 360 ) / 5 ) + 360 

# Drops 10 rows

df.rap$WriteoffsDummy <- as.factor(df.rap$WriteoffsDummy) # Load and set
df.rap$DtoE_adding_RC_Loan <- as.numeric(df.rap$DtoE_adding_RC_Loan)

df.rap$Max_Client_Arrears_Before_Close_1to15days_sq <- df.rap$Max_Client_Arrears_Before_Close_1to15days
df.rap$previous_arrears_dummy <- 0
#df.rap$previous_arrears_dummy[df.rap$Max_Client_Arrears_Before_Close_1to15days>=4] <- 1
df.rap$previous_arrears_dummy <- ifelse(df.rap$Max_Client_Arrears_Before_Close_1to15days>=6,2,0)
df.rap$previous_arrears_dummy <- ifelse(df.rap$Max_Client_Arrears_Before_Close_1to15days>=4,1,0)
df.rap$previous_arrears_dummy <- as.factor(df.rap$previous_arrears_dummy)

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
df.test <- df.rap[!df.rap$LoanID %in% LoanIDtrain,]  # End setup

###### 


modelColumns0 <- c("WriteoffsDummy","EBITDA.Total.Debt.Service", "Cash.Conversion.Cycle.", 
                   "margin_sd","DtoE_adding_RC_Loan", "Sales_log")

df.train.model <- df.train[,names(df.train) %in% modelColumns0]
glm0 <- glm(WriteoffsDummy ~ ., data=df.train.model, family="binomial", na.action=na.exclude)

pred0 <- predict(glm0, df.train, type="response")
roc0 <- roc(df.train$WriteoffsDummy, pred0)
summary(glm0)
roc0$auc
pR4(glm0)

df.train.model <- df.train.model[complete.cases(df.train.model),]
glm0step <- stepAIC(glm0, direction=c("both"))

###### 


modelColumnsa <- c("WriteoffsDummy","EBITDA.Total.Debt.Service", "Cash.Conversion.Cycle."
                   )

df.train.model <- df.train[,names(df.train) %in% modelColumnsa]
glma <- glm(WriteoffsDummy ~ ., data=df.train.model, family="binomial", na.action=na.exclude)

preda <- predict(glma, df.train, type="response")
roca <- roc(df.train$WriteoffsDummy, preda)
summary(glma)
roca$auc
pR4(glma)

df.train.model <- df.train.model[complete.cases(df.train.model),]
glmastep <- stepAIC(glma, direction=c("both"))

#####

#####

modelCols1 <- c(modelColumns0,
                "Negative_Working_Capital_Flag")


df.train.model <- df.train[,names(df.train) %in% modelCols1]
glm1 <- glm(WriteoffsDummy ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
summary(glm1)
pred1 <- predict(glm1, df.train, type="response")
roc1 <- roc(df.train$WriteoffsDummy, pred1)
roc1$auc
pR4(glm1)

df.train.model <- df.train.model[complete.cases(df.train.model),]
glm1step <- stepAIC(glm1, direction=c("both"))
### Previous

#### Basic model



#####

modelCols2 <- c(modelColumns0,
                "multiple_segments")


df.train.model <- df.train[,names(df.train) %in% modelCols2]
glm2 <- glm(WriteoffsDummy ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
summary(glm2)
pred2 <- predict(glm2, df.train, type="response")
roc2 <- roc(df.train$WriteoffsDummy, pred2)
roc2$auc
pR2(glm2)

df.train.model <- df.train.model[complete.cases(df.train.model),]
glm2step <- stepAIC(glm2, direction=c("both"))
### Previous

#####
modelCols3 <- c(modelColumns0,
                "max_pct_per_buyer")


df.train.model <- df.train[,names(df.train) %in% modelCols3]
glm3 <- glm(WriteoffsDummy ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
summary(glm3)
pred3 <- predict(glm3, df.train, type="response")
roc3 <- roc(df.train$WriteoffsDummy, pred3)
roc3$auc
pR3(glm3)

df.train.model <- df.train.model[complete.cases(df.train.model),]
glm3step <- stepAIC(glm3, direction=c("both"))
### Previous

#####
modelCols4 <- c(modelColumns0,
                "Sales_CAGR")


df.train.model <- df.train[,names(df.train) %in% modelCols4]
glm4 <- glm(WriteoffsDummy ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
summary(glm4)
pred4 <- predict(glm4, df.train, type="response")
roc4 <- roc(df.train$WriteoffsDummy, pred4)
roc4$auc
pR2(glm4)

df.train.model <- df.train.model[complete.cases(df.train.model),]
glm4step <- stepAIC(glm4, direction=c("both"))
### Previous

#####
modelCols5 <- c(modelColumns0,
                "member_receivables_dummy")


df.train.model <- df.train[,names(df.train) %in% modelCols5]
glm5 <- glm(WriteoffsDummy ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
summary(glm5)
pred5 <- predict(glm5, df.train, type="response")
roc5 <- roc(df.train$WriteoffsDummy, pred5)
roc5$auc
pR2(glm5)

df.train.model <- df.train.model[complete.cases(df.train.model),]
glm5step <- stepAIC(glm5, direction=c("both"))
### Previous

#####
modelCols6 <- c(modelColumns0,
                "Max_RiskCategory_Before_Close")


df.train.model <- df.train[,names(df.train) %in% modelCols6]
glm6 <- glm(WriteoffsDummy ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
summary(glm6)
pred6 <- predict(glm6, df.train, type="response")
roc6 <- roc(df.train$WriteoffsDummy, pred6)
roc6$auc
pR2(glm6)

df.train.model <- df.train.model[complete.cases(df.train.model),]
glm6step <- stepAIC(glm6, direction=c("both"))
### Previous

#####
modelCols7 <- c(modelColumns0,
                "previous_arrears_dummy")


df.train.model <- df.train[,names(df.train) %in% modelCols7]
glm7 <- glm(WriteoffsDummy ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
summary(glm7)
pred7 <- predict(glm7, df.train, type="response")
roc7 <- roc(df.train$WriteoffsDummy, pred7)
roc7$auc
pR2(glm7)

df.train.model <- df.train.model[complete.cases(df.train.model),]
glm7step <- stepAIC(glm7, direction=c("both"))
### Previous

#####
modelCols8 <- c(modelColumns0,
                "WorkingCapital_log")


df.train.model <- df.train[,names(df.train) %in% modelCols8]
glm8 <- glm(WriteoffsDummy ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
summary(glm8)
pred8 <- predict(glm8, df.train, type="response")
roc8 <- roc(df.train$WriteoffsDummy, pred8)
roc8$auc
pR2(glm8)

df.train.model <- df.train.model[complete.cases(df.train.model),]
glm8step <- stepAIC(glm8, direction=c("both"))
### Previous

##### INteraction in margin vol and cagr
modelCols9 <- c(modelColumns0,
                "margin_sd", "Sales_CAGR")


df.train.model <- df.train[,names(df.train) %in% modelCols9]
glm9 <- glm(WriteoffsDummy ~  margin_sd + Sales_CAGR + margin_sd*Sales_CAGR, data=df.train.model, family="binomial", na.action=na.exclude)
summary(glm9)
pred9 <- predict(glm9, df.train, type="response")
roc9 <- roc(df.train$WriteoffsDummy, pred9)
roc9$auc
pR2(glm9)

df.train.model <- df.train.model[complete.cases(df.train.model),]
glm9step <- stepAIC(glm9, direction=c("both"))
### Previous

compTable <- stargazer(glma, glm0, glm1, glm2, glm3, glm4, glm5, glm6, glm7, glm8, type = "html",
               ci = TRUE)

write(compTable, file = "step_comparison2.html", append = TRUE)
