################
#Build new models
library(pROC)
library(pscl)

filename <-  "https://rootcapital.box.com/shared/static/d6q5d6pfnvzao08x3ev7af4rj6bm6hve.csv"
# filename <-  "rap_data.csv"
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

df.rap$WriteoffsDummy <- as.factor(df.rap$WriteoffsDummy)


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



##########
# Using variables in risk rating
model1 <- "WriteoffsDummy ~ log(Sales + 1 - min(na.omit(Sales))) + max_pct_per_buyer +
multiple_segments + 
Cash.Conversion.Cycle. + EBITDA.Total.Debt.Service +
Total.Asset.Turnover + Gross_Margin_Variation + Negative_Working_Capital_Flag +
margin_sd + COGS.Margin + isCoffee + member_receivables_dummy"

# debt_service_to_ebitda +

glm1 <- glm(model1, data=df.train, family="binomial", na.action=na.exclude)
summary(glm1)
pred1 <- predict(glm1, df.train, type="response")
roc1 <- roc(df.train$WriteoffsDummy, pred1)
roc1$auc
pR2(glm1)


# Add sales CAGR
model2 <- "WriteoffsDummy ~ log(Sales + 1 - mean(Sales)) + max_pct_per_buyer +
  multiple_segments + 
  Depth.of.Management + Product.Recognition + Financial.Flexibility + Financial.Strat.Quality +
  Cash.Conversion.Cycle. + EBITDA.Total.Debt.Service +
  Total.Asset.Turnover + Gross_Margin_Variation + Negative_Working_Capital_Flag +
  Years_of_Financials +
  Sales_CAGR"

glm2 <- glm(model2, data=df.train, family="binomial", na.action=na.exclude)
summary(glm2)
pred2 <- predict(glm2, df.train, type="response")
roc2 <- roc(df.train$WriteoffsDummy, pred2)
roc2$auc


################
model3 <- "WriteoffsDummy ~ 
Depth.of.Management + Product.Recognition + Financial.Flexibility + Financial.Strat.Quality"

glm3 <- glm(model3, data=df.train, family="binomial", na.action=na.exclude)
summary(glm3)
pred3 <- predict(glm3, df.train, type="response")
roc3 <- roc(df.train$WriteoffsDummy, pred3)
roc3$auc
pR2(glm3)

####
model4 <- "WriteoffsDummy ~ log(Sales + 1 - min(Sales)) + max_pct_per_buyer +
multiple_segments + 
Depth.of.Management + Product.Recognition + Financial.Flexibility + Financial.Strat.Quality +
Cash.Conversion.Cycle. + EBITDA.Total.Debt.Service +
Total.Asset.Turnover + Gross_Margin_Variation + Negative_Working_Capital_Flag +
Years_of_Financials + log(TOTAL.ASSETS + 1)"

glm4 <- glm(model4, data=df.train, family="binomial", na.action=na.exclude)
summary(glm4)
pred4 <- predict(glm4, df.train, type="response")
roc4 <- roc(df.train$WriteoffsDummy, pred1)
roc4$auc
pR2(glm4)



# Total.Debt.Service + member_receivables_dummy
# restructure


# 
library(ggplot2)
# ex
df.train$pred <- pred1
plot_pred_type_distribution(df.train, 0.10)