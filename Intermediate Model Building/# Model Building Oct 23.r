################
#Build new models
library(pROC)
library(pscl)


wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/Loss Model/2.Model Selection"
setwd(wd)
#filename <-  "https://rootcapital.box.com/shared/static/d6q5d6pfnvzao08x3ev7af4rj6bm6hve.csv"
filename <-  "rap_data.csv"
df.rap <- read.csv(filename, header=TRUE, sep=",")

# Remove active loans (active is set in rap_cleaning_2 as > 7/1/14)
# Note that this is *keeping* 7 NA closed date loans: sum(is.na(df.rap$active))
df.rap <- subset(df.rap, active==0)
df.rap <- subset(df.rap, last_year==1)

## Move this to the data cleaning file:
df.rap$debt_service_to_ebitda[!is.finite(df.rap$debt_service_to_ebitda)] <- 0
#TODO fix gross margin variation
df.rap <- df.rap[df.rap$debt_service_to_ebitda>-100,]  # Should probably look in to these
df.rap <- df.rap[df.rap$Debt.Service.Coverage.Ratio..>-100,] # Rather than dropping them
# Drops 10 rows

df.rap$WriteoffsDummy <- as.factor(df.rap$WriteoffsDummy) # Load and set



#####################################################
# Split data into training and test set             #
# Important that this remains consistent throughout #  Split to 
#####################################################
uniqueIDs <- unique(df.rap$LoanID)
sample_size <- floor(0.80 * length(uniqueIDs))
set.seed(9)
LoanIDtrain <- sample(uniqueIDs, sample_size)
# LoanIDtest <- uniqueIDs[-LoanIDtrain] # Not necessary, better to use negative subset
df.train <- df.rap[df.rap$LoanID %in% LoanIDtrain,]
df.test <- df.rap[!df.rap$LoanID %in% LoanIDtrain,]  # End setup

###### 
# Previous 'best' model from last presentation
model18c <- "WriteoffsDummy ~ TotalAssets_log + CurrentAssets_log + COGS.Margin + EBITDA.Margin. + 
	  log( Adjusted.EBITDA + 1 - min(Adjusted.EBITDA)) + Total.Liabilities.Total.Assets +
	 Total.Debt.Service +
	 max_pct_per_buyer +
    WorkingCapital_log + debt_service_to_ebitda +
    multiple_segments +
    log(Sales + 1 - min(Sales)) +
    margin_sd"
glm18c <- glm(model18c, data=df.train, family="binomial", na.action=na.exclude)
summary(glm18c)
pR2	(glm18c)  
pred18c <- predict(glm18c, df.train, type="response")
roc18c <- roc(df.train$WriteoffsDummy, pred18c)
roc18c$auc   # End previous

#### Risk Rating
modelColsRR <- c("WriteoffsDummy","max_pct_per_buyer", "multiple_segments",
                 "Depth.of.Management", "Product.Recognition" , "Financial.Flexibility", "Financial.Strat.Quality",
                 "EBITDA.Total.Debt.Service", "Cash.Conversion.Cycle.", "Total.Asset.Turnover"
                )
df.train.model <- df.train[,names(df.train) %in% modelColsRR]
glmRR <- glm(WriteoffsDummy ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
predRR <- predict(glmRR, df.train, type="response")
rocRR <- roc(df.train$WriteoffsDummy, pred1)
summary(glmRR)
rocRR$auc
pR2(glmRR)
#####

modelColumns0 <- c("WriteoffsDummy", "Sales_log", "max_pct_per_buyer", "multiple_segments" , "Cash.Conversion.Cycle.", 
                   "EBITDA.Total.Debt.Service", "Total.Asset.Turnover", "Gross_Margin_Variation", 
                   "Negative_Working_Capital_Flag", "margin_sd", "COGS.Margin", "isCoffee", 
                   "member_receivables_dummy", "Sales_CAGR")

df.train.model <- df.train[,names(df.train) %in% modelColumns0]
glm0 <- glm(WriteoffsDummy ~ ., data=df.train.model, family="binomial", na.action=na.exclude)

pred0 <- predict(glm0, df.train, type="response")
roc0 <- roc(df.train$WriteoffsDummy, pred0)
summary(glm0)
roc0$auc
pR2(glm0)

df.train.model <- df.train.model[complete.cases(df.train.model),]
glm0step <- stepAIC(glm0, direction=c("both"))

#####

modelCols1 <- c("WriteoffsDummy", "TotalAssets_log", "Sales_log", "EBITDA.Margin.", "Total.Liabilities.Total.Assets",
                   "max_pct_per_buyer", "multiple_segments" , "Cash.Conversion.Cycle.", 
                   "Total.Debt.Service", "max_pct_per_buyer", "Working.Capital",
                   "EBITDA.Total.Debt.Service", "Total.Asset.Turnover", "Gross_Margin_Variation", 
                   "Negative_Working_Capital_Flag", "margin_sd", "COGS.Margin", "isCoffee", 
                   "member_receivables_dummy", "Sales_CAGR")


df.train.model <- df.train[,names(df.train) %in% modelCols1]
glm1 <- glm(WriteoffsDummy ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
summary(glm1)
pred1 <- predict(glm1, df.train, type="response")
roc1 <- roc(df.train$WriteoffsDummy, pred1)
roc1$auc
pR2(glm1)

df.train.model <- df.train.model[complete.cases(df.train.model),]
glm1step <- stepAIC(glm1, direction=c("both"))
### Previous

#### Basic model



modelCols3 <- c("WriteoffsDummy", "TotalAssets_log", "Sales_log", "EBITDA.Margin.", "Total.Liabilities.Total.Assets",
                   "max_pct_per_buyer", "multiple_segments" , "Cash.Conversion.Cycle.", 
                   "Total.Debt.Service", "max_pct_per_buyer", "WorkingCapital_log",
                   "EBITDA.Total.Debt.Service", "Total.Asset.Turnover", "Gross_Margin_Variation", 
                   "Negative_Working_Capital_Flag", "margin_sd", "COGS.Margin", "isCoffee", 
                   "member_receivables_dummy", "Sales_CAGR")

df.train.model <- df.train[,names(df.train) %in% modelCols3]
glm3 <- glm(WriteoffsDummy ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
summary(glm3)
pred3 <- predict(glm3, df.train, type="response")
roc3 <- roc(df.train$WriteoffsDummy, pred3)
roc3$auc
pR2(glm3)

df.train.model <- df.train.model[complete.cases(df.train.model),]
glm3step <- stepAIC(glm3, direction=c("both"))


#### Base model

modelCols4 <- c("WriteoffsDummy","Sales_log","Cash.Conversion.Cycle.","COGS.Margin","isCoffee", 
                "member_receivables_dummy") 

df.train.model <- df.train[,names(df.train) %in% modelCols4]
glm4 <- glm(WriteoffsDummy ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
summary(glm4)
pred4 <- predict(glm4, df.train, type="response")
roc4 <- roc(df.train$WriteoffsDummy, pred4)
roc4$auc
pR2(glm4)

#### Add liabilities to Assets 

modelCols5 <- c("WriteoffsDummy","Sales_log","Cash.Conversion.Cycle.","COGS.Margin","isCoffee", 
                "member_receivables_dummy", "Total.Liabilities.Total.Assets") 

df.train.model <- df.train[,names(df.train) %in% modelCols5]
glm5 <- glm(WriteoffsDummy ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
summary(glm5)
pred5 <- predict(glm5, df.train, type="response")
roc5 <- roc(df.train$WriteoffsDummy, pred5)
roc5$auc
pR2(glm5)

#### Add "Sales_CAGR"

modelCols6 <- c("WriteoffsDummy","Sales_log","Cash.Conversion.Cycle.","COGS.Margin","isCoffee", 
                "member_receivables_dummy", "Total.Liabilities.Total.Assets",
                "Sales_CAGR") 

df.train.model <- df.train[,names(df.train) %in% modelCols6]
glm6 <- glm(WriteoffsDummy ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
summary(glm6)
pred6 <- predict(glm6, df.train, type="response")
roc6 <- roc(df.train$WriteoffsDummy, pred6)
roc6$auc
pR2(glm6)


#### Add Max_Client_Arrears_Before_Close_1to15day

modelCols7 <- c("WriteoffsDummy","Sales_log","Cash.Conversion.Cycle.","COGS.Margin", "isCoffee",
                "member_receivables_dummy", "Total.Liabilities.Total.Assets",
                "Sales_CAGR","Max_Client_Arrears_Before_Close_1to15days") 

df.train.model <- df.train[,names(df.train) %in% modelCols7]
glm7 <- glm(WriteoffsDummy ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
summary(glm7)
pred7 <- predict(glm7, df.train, type="response")
roc7 <- roc(df.train$WriteoffsDummy, pred7)
roc7$auc
pR2(glm7)

