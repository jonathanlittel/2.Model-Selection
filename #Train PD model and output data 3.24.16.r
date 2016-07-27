# Final model
# To use the training set/testing set approach, use file # Model Building Oct 25.r
# Ok to just run that whole file (ideally skip end where it appends to html output)

library(caret)
library(dplyr)
options(scipen=99, digits=3)

# Load data and subset for active loans
  wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/PD Model/3.Outputs"
  setwd(wd)
  #filename <-  "https://rootcapital.box.com/shared/static/d7q5d7pfnvzao08x4ev7af4rj7bm7hve.csv"
  filename <-  "rap_data_Q4_15_4.25.16.csv"
  df.rap <- read.csv(filename, header=TRUE, sep=",")
  wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/PD Model/2.Model Selection"
  setwd(wd)
  df.rap <- subset(df.rap, last_year==1)

# Load custom function for graphing
  source('# Prediction Plot Function.r')

# Change WriteoffsDummy to factor
  df.rap$WriteoffsDummy <- as.factor(df.rap$WriteoffsDummy)

# Change Loan.Type to Loan.Type.at.Origination
  df.rap$Loan.Type <- df.rap$Loan.Type.at.Origination

# Make some data corrections
  # Change 'Reducing Line to Term Loan' to 'Term Loan' (double check that this is the right thing to do)
  df.rap <- df.rap %>% 
    mutate( Loan.Type = replace(Loan.Type, Loan.Type=="Reducing Line to Term Loan", "Term Loan"))
  # Impute missing margin_standard deviation with average
  df.rap$margin_sd[!is.finite(df.rap$margin_sd)] <- mean(na.omit(df.rap$margin_sd))
  # Add values for two missing values (still don't know correct values)
  df.rap$past_arrears[df.rap$LoanID==1969] <- 0
  df.rap$Depth.of.Management[df.rap$LoanID==1924] <- 7


#####################################################
# Split data into active / inactive loans           #
# and train model                                   # 
#####################################################

  df.rap.active <- df.rap[which(df.rap$active==1 & df.rap$balance_0915>0 & df.rap$last_year==1),]
  df.rap.inactive <- df.rap[which(df.rap$active==0 & df.rap$last_year==1),]



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


# Add LDG and EAD
  LGD_no_guarantee <- 0.90
  df.rap$LGD <- LGD_no_guarantee
  # df.rap$LGD <- ifelse(rap.active$Guarantee_pct<.01, LGD_no_guarantee, (1 - rap.active$Guarantee_pct))
  # not adding guarantees at this point. Still happens in loss simulation file                                    # Note

  # Create EAD, as 50% and 70% of original approved amount,
  # for Lines of Credit and Term loan types, respectively
  EAD_pct_LOC <- 0.49
  EAD_pct_Term <- 0.69
  df.rap$EAD <- ifelse(df.rap$Loan.Type=="Line of Credit", df.rap$Amount * EAD_pct_LOC, NA)
  df.rap$EAD <- ifelse(df.rap$Loan.Type=="Term Loan", df.rap$Amount * EAD_pct_Term, df.rap$EAD)
  df.rap$EL <- df.rap$pd * df.rap$EAD * df.rap$LGD

# Write results for loss simulation
  df.rap$pd <- predict(glmFinal, df.rap, type="response")
  wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Stress Testing/Loss Distribution Simulation"
  setwd(wd)
  write.csv(df.rap, 
            "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Stress Testing/Loss Distribution Simulation/predicted_default.csv")
  wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/PD Model/3.Outputs"
  setwd(wd)

  outputcols <- c("WriteoffsDummy", "Sales_log", "WorkingCapital_log",
                  "past_arrears",
                  "margin_sd","Depth.of.Management", "past_arrears",
                  "Loan.Type", "pd", "LoanID", "balance_1215",
                  "Sales", "Working.Capital", "Amount", "LGD", "EAD", "EL")  # Nice to have these two for calcs
                                                # Also need to have minumums for the data cleaning file
                                                # Re-write to be c(modelColsFinal, "xxx")


output.csv <- df.rap[,names(df.rap) %in% outputcols]
write.csv(output.csv,'underwriting_pds_Q4_2015.csv')

library(stargazer)
finalPD <- stargazer(glmFinal, type = "html",
                     ci = TRUE)

write(finalPD, file = "pd_model_Q4_2015.html", append = FALSE)
save(glmFinal, file = "pd_model_Q4_2015.rda")
write.csv(glmFinal$coefficients, file="pd_model_final.csv")


quantile(df.rap$pd, probs = seq(0, 1, 0.05), na.rm=TRUE)

# Plot roc curve
  library(pROC)
  rocCurve <- plot.roc(df.rap$WriteoffsDummy, df.rap$pd)

exp(coefficients(glmFinal))
sum(na.omit(df.rap$pd * df.rap$balance_0915)) / sum(df.rap$balance_0915[!is.na(df.rap$pd)])