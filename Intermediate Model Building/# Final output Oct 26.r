# Final model
# First run subset/train/test on glm5 from # Model Building Oct 25.r
# Ok to just run that whole file (ideally skip end where it appends to html output)

library(caret)

# Load custom function for graphing
source('# Prediction Plot Function.r')


### Run on test

modelCols5 <- c("WriteoffsDummy", "Sales_log", "WorkingCapital_log",
                "Max_RiskCategory_Before_Close",
                "DtoE_adding_RC_Loan", "margin_sd")

df.test.model <- df.test[,names(df.test) %in% modelCols5]
# glm5t <- glm(WriteoffsDummy ~ ., data=df.test.model, family="binomial", na.action=na.exclude)

pred5t <- predict(glm5, df.test, type="response")
roc5t <- roc(df.test$WriteoffsDummy, pred5t)
# summary(glm5)
roc5t$auc
pR2(glm5)
### Model 5

# Reload dataset (was subset for active only), produce predicted WO 
# based on glm5 for whole set
# And write predicted WOs to file

wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/Loss Model/1.Dataset Cleaning and Prep"
setwd(wd)
#filename <-  "https://rootcapital.box.com/shared/static/d7q5d7pfnvzao08x4ev7af4rj7bm7hve.csv"
filename <-  "rap_data.csv"
df.rap <- read.csv(filename, header=TRUE, sep=",")
df.rap <- subset(df.rap, last_year==1)
#####
# Data cleaning
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
#####



wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/Loss Model/2.Model Selection"
setwd(wd)

df.rap$predicted_wo <- predict(glm5, df.rap, type="response")


write.csv(df.rap, 
          "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Stress Testing/Loss Distribution Simulation/predicted_default.csv")


## Confusion Matrix on test
cutoff <- 0.1
df.test$predicted_wo <- predict(glm5, df.test, type="response")
df.test$predWO_cut <- as.factor(ifelse(df.test$predicted_wo>cutoff,1,0))
confusionMatrix(df.test$predWO_cut, df.test$WriteoffsDummy)

## Confusion Matrix on total
cutoff <- 0.1
df.rap$predicted_wo <- predict(glm5, df.rap, type="response")
df.rap$predWO_cut <- as.factor(ifelse(df.rap$predicted_wo>cutoff,1,0))
confusionMatrix(df.rap$predWO_cut, df.rap$WriteoffsDummy)



# Graph with total
plot_pred_type_distribution(df.rap, cutoff)
# Things to point out here, the density is good, skinny at top on left, fatter higher on right

# Graph with test
plot_pred_type_distribution(df.test, cutoff)

results <- as.table(confusionMatrix(df.test$predWO_cut, df.test$WriteoffsDummy))
write(results, "classification_table_of_test_data.html")

