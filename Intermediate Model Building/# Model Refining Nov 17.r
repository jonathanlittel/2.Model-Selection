################################################
# Run progressive buildup of pd logit model #
#############################################
library(pROC)
library(pscl)
library(stargazer)
library(caret)

options(scipen=99, digits=3)

wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/PD Model/1.Dataset Cleaning and Prep"
setwd(wd)
#filename <-  "https://rootcapital.box.com/shared/static/d7q5d7pfnvzao08x4ev7af4rj7bm7hve.csv"
filename <-  "rap_data.csv"
df.rap <- read.csv(filename, header=TRUE, sep=",")

wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/PD Model/2.Model Selection"
setwd(wd)

# This is done in rap_cleaning_2.r, but re-doing here since loans me be
# manually added after
df.rap$balance_0915[!is.finite(df.rap$balance_0915)] <- 0
df.rap$active <- ifelse(df.rap$balance_0915>0,1,0)

df.rap <- subset(df.rap, last_year==1)

# Impute missing margin_standard deviation with average
df.rap$margin_sd[!is.finite(df.rap$margin_sd)] <- mean(na.omit(df.rap$margin_sd))

# Change WriteoffsDummy to factor
df.rap$WriteoffsDummy <- as.factor(df.rap$WriteoffsDummy)

#####################################################
# Split data into training and test set             #
# and active / inactive set                         # 
#####################################################

uniqueIDs <- unique(df.rap$LoanID)
sample_size <- floor(0.80 * length(uniqueIDs))
set.seed(10)
LoanIDtrain <- sample(uniqueIDs, sample_size)
df.train <- df.rap[df.rap$LoanID %in% LoanIDtrain,]
df.train <- df.train[which(df.train$active==0 & df.train$last_year==1),]

df.test <- df.rap[!df.rap$LoanID %in% LoanIDtrain,]  
df.test <- df.test[which(df.test$active==0 & df.test$last_year==1),]


df.rap.active <- df.rap[which(df.rap$active==1 & df.rap$balance_0915>0 & df.rap$last_year==1),]
df.rap.inactive <- df.rap[which(df.rap$active==0 & df.rap$last_year==1),]


###### 
modelCols0 <- c("WriteoffsDummy", "Sales_log", "WorkingCapital_log",
                "past_arrears")

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
                "past_arrears",
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
                "past_arrears",
                "multiple_segments")

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
                "past_arrears")

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
                "past_arrears",
                "EBITDA.Total.Debt.Service")

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
                "past_arrears",
                "margin_sd")

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
                "past_arrears",
                "Depth.of.Management")

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
                "past_arrears",
                "margin_sd","Depth.of.Management")

df.train.model <- df.train[,names(df.train) %in% modelCols6]
glm6 <- glm(WriteoffsDummy ~ ., data=df.train.model, family="binomial", na.action=na.exclude)

pred6 <- predict(glm6, df.train, type="response")
roc6 <- roc(df.train$WriteoffsDummy, pred6)
summary(glm6)
roc6$auc
pR2(glm6)
### Model 6

###### 
modelCols7 <- c("WriteoffsDummy", "Sales_log", "WorkingCapital_log",
                "past_arrears",
                "margin_sd","Depth.of.Management",
                "isCoffee")

df.train.model <- df.train[,names(df.train) %in% modelCols7]
glm7 <- glm(WriteoffsDummy ~ ., data=df.train.model, family="binomial", na.action=na.exclude)

pred7 <- predict(glm7, df.train, type="response")
roc7 <- roc(df.train$WriteoffsDummy, pred7)
summary(glm7)
roc7$auc
pR2(glm7)
### Model 7


###### 
modelCols8 <- c("WriteoffsDummy", "Sales_log", "WorkingCapital_log",
                "past_arrears",
                "margin_sd","Depth.of.Management", "past_arrears",
                "Loan.Type")

df.train.model <- df.train[,names(df.train) %in% modelCols8]
glm8 <- glm(WriteoffsDummy ~ ., data=df.train.model, family="binomial", na.action=na.exclude)

pred8 <- predict(glm8, df.train, type="response")
roc8 <- roc(df.train$WriteoffsDummy, pred8)
summary(glm8)
roc8$auc
pR2(glm8)
### Model 8


# Train final model on both test and training
modelColsFinal <- c("WriteoffsDummy", "Sales_log", "WorkingCapital_log",
                "past_arrears",
                "margin_sd","Depth.of.Management", "past_arrears",
                "Loan.Type")
df.train.model <- df.train[,names(df.rap.inactive) %in% modelColsFinal]
glmFinal <- glm(WriteoffsDummy ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
df.train.model <- df.train[,names(df.rap) %in% modelColsFinal]

compTable <- stargazer(glm0, glm1, glm2, glm3, glm4, glm5, glm6, glm7, glm8, type = "html",
                       ci = TRUE)

write(compTable, file = "comparison_final.html", append = TRUE)

rocs <- c(roc0$auc,roc1$auc,roc2$auc,
          roc3$auc, roc4$auc, roc5$auc, roc6$auc, roc7$auc,
          roc8$auc)
write("ROCs:", file = "comparison_final.html", append = TRUE)
write(rocs, file = "comparison_final.html", append = TRUE)


varImp(glm8) # from caret package 

