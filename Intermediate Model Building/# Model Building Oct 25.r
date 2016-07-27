################
#Build new models
library(pROC)
library(pscl)
library(stargazer)
library(caret)

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
df.rap$Cash.Conversion.Cycle.[df.rap$Cash.Conversion.Cycle.>360]  <-  (( df.rap$Cash.Conversion.Cycle[df.rap$Cash.Conversion.Cycle.>460]  - 460 ) / 5 ) + 460 

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
modelCols0 <- c("WriteoffsDummy", "Sales_log", "WorkingCapital_log",
                    "Max_RiskCategory_Before_Close")

df.train.model <- df.train[,names(df.train) %in% modelCols0]
glm0 <- glm(WriteoffsDummy ~ ., data=df.train.model, family="binomial", na.action=na.exclude)

pred0 <- predict(glm0, df.train, type="response")
roc0 <- roc(df.train$WriteoffsDummy, pred0)
summary(glm0)
roc0$auc
pR2(glm0)
### Model 0

######  "DtoE_adding_RC_Loan"
###### 
modelCols1 <- c("WriteoffsDummy", "Sales_log", "WorkingCapital_log",
                "Max_RiskCategory_Before_Close",
                "DtoE_adding_RC_Loan")

df.train.model <- df.train[,names(df.train) %in% modelCols1]
glm1 <- glm(WriteoffsDummy ~ ., data=df.train.model, family="binomial", na.action=na.exclude)

pred1 <- predict(glm1, df.train, type="response")
roc1 <- roc(df.train$WriteoffsDummy, pred1)
summary(glm1)
roc1$auc
pR2(glm1)
### Model 1

###### 
modelCols2 <- c("WriteoffsDummy", "Sales_log", "WorkingCapital_log",
                "Max_RiskCategory_Before_Close",
                "DtoE_adding_RC_Loan", "multiple_segments")

df.train.model <- df.train[,names(df.train) %in% modelCols2]
glm2 <- glm(WriteoffsDummy ~ ., data=df.train.model, family="binomial", na.action=na.exclude)

pred2 <- predict(glm2, df.train, type="response")
roc2 <- roc(df.train$WriteoffsDummy, pred2)
summary(glm2)
roc2$auc
pR2(glm2)
### Model 2

###### 
modelCols1 <- c("WriteoffsDummy", "Sales_log", "WorkingCapital_log",
                "Max_RiskCategory_Before_Close",
                "DtoE_adding_RC_Loan")

df.train.model <- df.train[,names(df.train) %in% modelCols1]
glm1 <- glm(WriteoffsDummy ~ ., data=df.train.model, family="binomial", na.action=na.exclude)

pred1 <- predict(glm1, df.train, type="response")
roc1 <- roc(df.train$WriteoffsDummy, pred1)
summary(glm1)
roc1$auc
pR2(glm1)
### Model 1

###### 
modelCols3 <- c("WriteoffsDummy", "Sales_log", "WorkingCapital_log",
                "Max_RiskCategory_Before_Close",
                "DtoE_adding_RC_Loan", "EBITDA.Total.Debt.Service")

df.train.model <- df.train[,names(df.train) %in% modelCols3]
glm3 <- glm(WriteoffsDummy ~ ., data=df.train.model, family="binomial", na.action=na.exclude)

pred3 <- predict(glm3, df.train, type="response")
roc3 <- roc(df.train$WriteoffsDummy, pred3)
summary(glm3)
roc3$auc
pR2(glm3)
### Model 3

# "isCoffee","multiple_segments"
###### 
modelCols4 <- c("WriteoffsDummy", "Sales_log", "WorkingCapital_log",
                "Max_RiskCategory_Before_Close",
                "DtoE_adding_RC_Loan", "margin_sd")

df.train.model <- df.train[,names(df.train) %in% modelCols4]
glm4 <- glm(WriteoffsDummy ~ ., data=df.train.model, family="binomial", na.action=na.exclude)

pred4 <- predict(glm4, df.train, type="response")
roc4 <- roc(df.train$WriteoffsDummy, pred4)
summary(glm4)
roc4$auc
pR2(glm4)
### Model 4

###### 
modelCols5 <- c("WriteoffsDummy", "Sales_log", "WorkingCapital_log",
                "Max_RiskCategory_Before_Close",
                "DtoE_adding_RC_Loan", "Depth.of.Management")

df.train.model <- df.train[,names(df.train) %in% modelCols5]
glm5 <- glm(WriteoffsDummy ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
#pooled.logit1 <- pglm(WriteoffsDummy ~ ., model=("pooling"), family=binomial(link="logit"), data=df.train.model)

pred5 <- predict(glm5, df.train, type="response")
roc5 <- roc(df.train$WriteoffsDummy, pred5)
summary(glm5)
roc5$auc
pR2(glm5)
### Model 5

###### 
modelCols6 <- c("WriteoffsDummy", "Sales_log", "WorkingCapital_log",
                "Max_RiskCategory_Before_Close",
                "DtoE_adding_RC_Loan", "margin_sd","Depth.of.Management")

df.train.model <- df.train[,names(df.train) %in% modelCols6]
glm6 <- glm(WriteoffsDummy ~ ., data=df.train.model, family="binomial", na.action=na.exclude)

pred6 <- predict(glm6, df.train, type="response")
roc6 <- roc(df.train$WriteoffsDummy, pred6)
summary(glm6)
roc6$auc
pR2(glm6)
### Model 6
compTable <- stargazer(glm0, glm1, glm2, glm3, glm4, glm5, glm6, type = "html",
                       ci = TRUE)

write(compTable, file = "comparison_final.html", append = TRUE)

rocs <- c(roc0$auc,roc1$auc,roc2$auc,
          roc3$auc, roc4$auc, roc5$auc, roc6$auc)
write("ROCs:", file = "comparison_final.html", append = TRUE)
write(rocs, file = "comparison_final.html", append = TRUE)

finalPD <- stargazer(glm4, type = "html",
                     ci = TRUE)
write(finalPD, file = "pd_model.html", append = TRUE)

write.csv(glm4$coefficients, file="pd_model_final.csv")
