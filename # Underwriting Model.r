# Final model
# First run subset/train/test on glm5 from # Model Building Oct 25.r
# Ok to just run that whole file (ideally skip end where it appends to html output)

library(caret)
require(pscl)
options(scipen=99, digits=3)

# Load custom function for graphing
source('# Prediction Plot Function.r')

wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/PD Model/3.Outputs"
setwd(wd)
#filename <-  "https://rootcapital.box.com/shared/static/d7q5d7pfnvzao08x4ev7af4rj7bm7hve.csv"
filename <-  "rap_data_Q4_15_4.29.16.csv"
df.rap <- read.csv(filename, header=TRUE, sep=",")
wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/PD Model/2.Model Selection"
setwd(wd)

df.rap <- subset(df.rap, last_year==1)

# Impute missing margin_standard deviation with average
df.rap$margin_sd[!is.finite(df.rap$margin_sd)] <- mean(na.omit(df.rap$margin_sd))

# Change WriteoffsDummy to factor
df.rap$WriteoffsDummy <- as.factor(df.rap$WriteoffsDummy)

# df.rap$margin_sd <- ifelse(df.rap$margin_sd>1,1,df.rap$margin_sd)

#####################################################
# Split data into training and test set             #
# and active / inactive set                         # 
#####################################################

uniqueIDs <- unique(df.rap$LoanID)
sample_size <- floor(0.80 * length(uniqueIDs))
set.seed(10)
LoanIDtrain <- sample(uniqueIDs, sample_size)
df.rap.active <- df.rap[which(df.rap$active==1 & df.rap$balance_1215>0 & df.rap$last_year==1),]
df.rap.inactive <- df.rap[which(df.rap$active==0 & df.rap$last_year==1),]

df.rap$Loan.Type[df.rap$Loan.Type=="Reducing Line to Term Loan"] <- "Term Loan"

#### Train final model on both test and training
  modelColsFinal <- c("WriteoffsDummy", "Sales_log", "WorkingCapital_log",
                      "past_arrears",
                      "margin_sd","Depth.of.Management",
                      "Loan.Type")
  # Train model on all inactive loans
  df.train.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelColsFinal] 
  glmFinal <- glm(WriteoffsDummy ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
  # Produce underwriting pds for all loans in dataset
  df.rap$pd <- predict(glmFinal, df.rap, type="response")
  
  
## Confusion Matrix on total
  cutoff <- 0.1
  df.rap$pd <- predict(glmFinal, df.rap, type="response")
  df.rap$predWO_cut <- as.factor(ifelse(df.rap$pd>cutoff,1,0))
  confusionMatrix(df.rap$predWO_cut, df.rap$WriteoffsDummy)



# Graph with total
plot_pred_type_distribution(df.rap, cutoff)
# Things to point out here, the density is good, skinny at top on left, fatter higher on right

# Graph with test
#   plot_pred_type_distribution(df.test, cutoff)

options(scipen = 99, digits=2)
exp(coefficients(glmFinal))
anova(glmFinal,test="Chisq")

# Write results for loss simulation
df.rap$pd <- predict(glmFinal, df.rap, type="response")

write.csv(df.rap, 
          "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Stress Testing/Loss Distribution Simulation/predicted_default_Q4_2015.csv")

wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/PD Model/2.Model Selection"
setwd(wd)

outputcols <- c("WriteoffsDummy", "Sales_log", "WorkingCapital_log",
                "past_arrears",
                "margin_sd","Depth.of.Management", "past_arrears",
                "Loan.Type", "pd", "LoanID", "balance_1215",
                "Sales", "Working.Capital")  # Nice to have these two for calcs
                                              # Also need to have minumums for the data cleaning file

output.csv <- df.rap[,names(df.rap) %in% outputcols]
write.csv(output.csv,'pds_Q4_2015.csv')

library(stargazer)
finalPD <- stargazer(glmFinal, type = "html",
                     ci = TRUE)

write(finalPD, file = "pd_model_Q4_2015.html", append = FALSE)
save(glmFinal, file = "pd_model_0Q4_2015.16.rda")
write.csv(glmFinal$coefficients, file="pd_model_through_Q4_2015.csv")


quantile(df.rap$pd, probs = seq(0, 1, 0.05), na.rm=TRUE)

# Plot roc curve
  library(pROC)
  rocCurve <- plot.roc(df.rap$WriteoffsDummy, df.rap$pd)

sum(na.omit(df.rap$pd * df.rap$balance_1215)) / sum(df.rap$balance_1215[!is.na(df.rap$pd)])