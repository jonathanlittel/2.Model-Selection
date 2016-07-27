# Model selection
# 
# Note: It would be possible to do step-wise selection, but that it not an ideal approach given that we have some good understanding of the features
# We split into an 80/20 train/test set, so that we have something to see if our model is working
# Once we select a model, we will train it on the full set

# Check to see if required packages are installed, and if not, install them
list.of.packages <- c("ggplot2", "Rcpp", "pscl", "caret")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(pscl)
library(pROC)
# Set the number of significant digits to display
options(scipen=100)
options(digits=3)

filename <-  "https://rootcapital.box.com/shared/static/d6q5d6pfnvzao08x3ev7af4rj6bm6hve.csv"
df.rap <- read.csv(filename, header=TRUE, sep=",")

# Remove active loans (active is set in rap_cleaning_2 as > 7/1/14)
# Note that this is *keeping* 7 NA closed date loans: sum(is.na(df.rap$active))
df.rap <- subset(df.rap, active==0)

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

# Check that there is no overlap in the train/test sets
intersect(LoadIDtest, LoanIDtrain)


# Cross tab of segments and 
xtabs(~WriteoffsDummy + unique_segments, data = df.rap)


# Simple intercept
model0 <- "WriteoffsDummy ~ 1"
glm0 <- glm(model0, data=df.train, family="binomial", na.action=na.exclude)
summary(glm0)
glm0

# AIC is 3814.5, let's use that as a starting point

# Now let's try adding a single account, Total Assets
model1 <- "WriteoffsDummy ~ TOTAL.ASSETS"
glm1 <- glm(model1, data=df.train, family="binomial", na.action=na.exclude)
summary(glm1)
pR2(glm1)
# AIC is 3815.5, that's actually worse
# Let's look at the distribution of Total Assets
plot(sort(df.train$TOTAL.ASSETS))

# Now let's try adding a single account, Total Assets
model2 <- "WriteoffsDummy ~ log(TOTAL.ASSETS + 1)"
glm2 <- glm(model2, data=df.train, family="binomial", na.action=na.exclude)
summary(glm2)
pR2(glm2)
# Ok, that's a little better, not much though. Asset size itself might not predict risk, though it could help once other terms are added
model3 <- "WriteoffsDummy ~ log(TOTAL.ASSETS + 1) + COGS.Margin"
glm3 <- glm(model3, data=df.train, family="binomial", na.action=na.exclude)
summary(glm3)
pR2(glm3)
pred3 <- predict(glm3, df.train, type="response")
roc3 <- roc(df.train$WriteoffsDummy, pred3)
roc3$auc

# Let's look at this one
plot(sort(df.train$COGS.Margin[df.train$COGS.Margin<10 & df.train$COGS.Margin>-10]))

# Let's try adding several ratios
model4 <- "WriteoffsDummy ~ log(TOTAL.ASSETS + 1) + COGS.Margin + EBITDA.Margin. +
	 Total.Liabilities.Total.Assets"
glm4 <- glm(model4, data=df.train, family="binomial", na.action=na.exclude)
summary(glm4)
pR2(glm4)
pred4 <- predict(glm4, df.train, type="response")
roc4 <- roc(df.train$WriteoffsDummy, pred4)
roc4$auc


cor(df.train$Net.Profit.Margin.,df.train$EBITDA.Margin.)

# What about controlling for region
model5 <- "WriteoffsDummy ~ log(TOTAL.ASSETS + 1) + COGS.Margin + EBITDA.Margin. + 
	 Total.Asset.Turnover + Current.Ratio + Adjusted.EBITDA + Total.Liabilities.Total.Assets +
	 Lending.Region"
glm5 <- glm(model5, data=df.train, family="binomial", na.action=na.exclude)
summary(glm5)
pR2(glm5)
pred5 <- predict(glm5, df.train, type="response")
roc5 <- roc(df.train$WriteoffsDummy, pred5)
roc5$auc

# Hmm, that seemed to help, but let's put region on the back burner.
model6 <- "WriteoffsDummy ~ log(TOTAL.ASSETS + 1) + COGS.Margin + EBITDA.Margin. + 
	 Total.Asset.Turnover + Current.Ratio + Adjusted.EBITDA + Total.Liabilities.Total.Assets +
	 Total.Debt.Service"
glm6 <- glm(model6, data=df.train, family="binomial", na.action=na.exclude)
summary(glm6)
pR2(glm6)
pred6 <- predict(glm6, df.train, type="response")
roc6 <- roc(df.train$WriteoffsDummy, pred6)
roc6$auc


# Let's look at the percentage of sales to the largest buyer
model7 <- "WriteoffsDummy ~ log(TOTAL.ASSETS + 1) + COGS.Margin + EBITDA.Margin. + 
	 Total.Asset.Turnover + Current.Ratio + Adjusted.EBITDA + Total.Liabilities.Total.Assets +
	 Total.Debt.Service +
	 max_pct_per_buyer "
glm7 <- glm(model7, data=df.train, family="binomial", na.action=na.exclude)
summary(glm7)
pR2(glm7)
pred7 <- predict(glm7, df.train, type="response")
roc7 <- roc(df.train$WriteoffsDummy, pred7)
roc7$auc


# Let's add in all the qualitative ratings
model8 <- "WriteoffsDummy ~ log(TOTAL.ASSETS + 1) + COGS.Margin + EBITDA.Margin. + 
	 Total.Asset.Turnover + Current.Ratio + Adjusted.EBITDA + Total.Liabilities.Total.Assets +
	 Total.Debt.Service +
	 max_pct_per_buyer +
	 Depth.of.Management + Product.Recognition + Financial.Flexibility + Financial.Strat.Quality"
glm8 <- glm(model8, data=df.train, family="binomial", na.action=na.exclude)
summary(glm8)
pR2(glm8)
pred8 <- predict(glm8, df.train, type="response")
roc8 <- roc(df.train$WriteoffsDummy, pred8)
roc8$auc

# Add number of sales channels
model9 <- "WriteoffsDummy ~ log(TOTAL.ASSETS + 1) + COGS.Margin + EBITDA.Margin. + 
	 Total.Asset.Turnover + Current.Ratio + Adjusted.EBITDA + Total.Liabilities.Total.Assets +
	 Total.Debt.Service +
	 max_pct_per_buyer +
	 Depth.of.Management + Product.Recognition + Financial.Flexibility + Financial.Strat.Quality +
    unique_segments"
glm9 <- glm(model9, data=df.train, family="binomial", na.action=na.exclude)
# Note that unique_segments drops a lot of observations
pred9 <- predict(glm9, df.train, type="response")
roc9 <- roc(df.train$WriteoffsDummy, pred9)
roc9$auc

# Switch to a dummy for >1 segment
model9a <- "WriteoffsDummy ~ log(TOTAL.ASSETS + 1) + COGS.Margin + EBITDA.Margin. + 
Total.Asset.Turnover + Current.Ratio + Adjusted.EBITDA + Total.Liabilities.Total.Assets +
Total.Debt.Service +
max_pct_per_buyer +
Depth.of.Management + Product.Recognition + Financial.Flexibility + Financial.Strat.Quality +
multiple_segments"
glm9a <- glm(model9a, data=df.train, family="binomial", na.action=na.exclude)
# Note that unique_segments drops a lot of observations
pred9a <- predict(glm9a, df.train, type="response")
roc9a <- roc(df.train$WriteoffsDummy, pred9a)
roc9a$auc

# Using variables in risk rating
model10 <- "WriteoffsDummy ~ Sales + factor(unique_segments) + Depth.of.Management + Product.Recognition + Financial.Flexibility + Financial.Strat.Quality + Cash.Conversion.Cycle. +
EBITDA.Total.Debt.Service + Funded.Debt.EBITDA...Cash.Flow.Leverage"
glm10 <- glm(model10, data=df.rap, family="binomial", na.action=na.exclude)
summary(glm10)
pred10 <- predict(glm10, df.train, type="response")
roc10 <- roc(df.train$WriteoffsDummy, pred10)
roc10$auc

# 
model11 <- "WriteoffsDummy ~ log(TOTAL.ASSETS + 1) + log(Total.current.assets + 1) + COGS.Margin + EBITDA.Margin. + 
	 Total.Asset.Turnover + Current.Ratio + Adjusted.EBITDA + Total.Liabilities.Total.Assets +
	 Total.Debt.Service +
	 max_pct_per_buyer +
	 Depth.of.Management + Product.Recognition + Financial.Flexibility + Financial.Strat.Quality +
    Working.Capital"
glm11 <- glm(model11, data=df.train, family="binomial", na.action=na.exclude)
summary(glm11)
pred11 <- predict(glm1, df.train, type="response")
roc11 <- roc(df.train$WriteoffsDummy, pred11)
roc11$auc

# Create debt service to ebitda ratio
df.rap$debt_service_to_ebitda <- df.rap$Debt.Service.Coverage / df.rap$EBITDA.1


#
model12 <- "WriteoffsDummy ~ log(TOTAL.ASSETS + 1) + log(Total.current.assets + 1) + COGS.Margin + EBITDA.Margin. + 
	 Total.Asset.Turnover + Current.Ratio + Adjusted.EBITDA + Total.Liabilities.Total.Assets +
	 Total.Debt.Service +
	 max_pct_per_buyer +
	 Depth.of.Management + Product.Recognition + Financial.Flexibility + Financial.Strat.Quality +
    Working.Capital + debt_service_to_ebitda"
glm12 <- glm(model12, data=df.train, family="binomial", na.action=na.exclude)
summary(glm12)
pR2	(glm12)
pred12 <- predict(glm12, df.train, type="response")
roc12 <- roc(df.train$WriteoffsDummy, pred12)
roc12$auc

# Add segments
model13 <- "WriteoffsDummy ~ log(TOTAL.ASSETS + 10000) + log(Total.current.assets + 10000) + COGS.Margin + EBITDA.Margin. + 
	 Total.Asset.Turnover + Current.Ratio + log( Adjusted.EBITDA + 1 - min(Adjusted.EBITDA)) + Total.Liabilities.Total.Assets +
	 Total.Debt.Service +
	 max_pct_per_buyer +
	 Depth.of.Management + Product.Recognition + Financial.Flexibility + Financial.Strat.Quality +
    log(Working.Capital + 1 - min(Working.Capital)) + debt_service_to_ebitda +
    multiple_segments"
glm13 <- glm(model13, data=df.train, family="binomial", na.action=na.exclude)
summary(glm13)
pR2	(glm13)
pred13 <- predict(glm13, df.train, type="response")
roc13 <- roc(df.train$WriteoffsDummy, pred13)
roc13$auc

# Add sector
model14 <- "WriteoffsDummy ~ log(TOTAL.ASSETS + 1) + log(Total.current.assets + 1 - min(Total.current.assets)) + COGS.Margin + EBITDA.Margin. + 
	 Total.Asset.Turnover + Current.Ratio + log( Adjusted.EBITDA + 1 - min(Adjusted.EBITDA)) + Total.Liabilities.Total.Assets +
	 Total.Debt.Service +
	 max_pct_per_buyer +
	 Depth.of.Management + Product.Recognition + Financial.Flexibility + Financial.Strat.Quality +
    log(Working.Capital + 1 - min(Working.Capital)) + debt_service_to_ebitda +
    multiple_segments + Sector.and.Perishability"
glm14 <- glm(model14, data=df.train, family="binomial", na.action=na.exclude)
summary(glm14)
pR2	(glm14)
pred14 <- predict(glm14, df.train, type="response")
roc14 <- roc(df.train$WriteoffsDummy, pred13)
roc14$auc


# Add Sales 
model15 <- "WriteoffsDummy ~ log(TOTAL.ASSETS + 1) + log(Total.current.assets + 1 - min(Total.current.assets)) + COGS.Margin + EBITDA.Margin. + 
	 Total.Asset.Turnover + Current.Ratio + log( Adjusted.EBITDA + 1 - min(Adjusted.EBITDA)) + Total.Liabilities.Total.Assets +
	 Total.Debt.Service +
	 max_pct_per_buyer +
	 Depth.of.Management + Product.Recognition + Financial.Flexibility + Financial.Strat.Quality +
    log(Working.Capital + 1 - min(Working.Capital)) + debt_service_to_ebitda +
    multiple_segments +
    log(Sales + 1 - min(Sales))"
glm15 <- glm(model15, data=df.train, family="binomial", na.action=na.exclude)
summary(glm15)
pR2	(glm15)   # + Sector.and.Perishability +
pred15 <- predict(glm15, df.train, type="response")
roc15 <- roc(df.train$WriteoffsDummy, pred15)
roc15$auc


# Add Renewal and Loan Usage 
model16 <- "WriteoffsDummy ~ log(TOTAL.ASSETS + 1) + log(Total.current.assets + 1 - min(Total.current.assets)) + COGS.Margin + EBITDA.Margin. + 
	 Total.Asset.Turnover + Current.Ratio + log( Adjusted.EBITDA + 1 - min(Adjusted.EBITDA)) + Total.Liabilities.Total.Assets +
	 Total.Debt.Service +
	 max_pct_per_buyer +
	 Depth.of.Management + Product.Recognition + Financial.Flexibility + Financial.Strat.Quality +
    log(Working.Capital + 1 - min(Working.Capital)) + debt_service_to_ebitda +
    multiple_segments +
    log(Sales + 1 - min(Sales)) + Loan.Use + Loan.Type"
glm16 <- glm(model16, data=df.train, family="binomial", na.action=na.exclude)
summary(glm16)
pR2	(glm16)   # + Sector.and.Perishability +
pred16 <- predict(glm16, df.train, type="response")
roc16 <- roc(df.train$WriteoffsDummy, pred16)
roc16$auc

# Add Standard deviation of cogs margin 
model17 <- "WriteoffsDummy ~ log(TOTAL.ASSETS + 1) + log(Total.current.assets + 1 - min(Total.current.assets)) + COGS.Margin + EBITDA.Margin. + 
	 Total.Asset.Turnover + Current.Ratio + log( Adjusted.EBITDA + 1 - min(Adjusted.EBITDA)) + Total.Liabilities.Total.Assets +
	 Total.Debt.Service +
	 max_pct_per_buyer +
	 Depth.of.Management + Product.Recognition + Financial.Flexibility + Financial.Strat.Quality +
    log(Working.Capital + 1 - min(Working.Capital)) + debt_service_to_ebitda +
    multiple_segments +
    log(Sales + 1 - min(Sales)) + Loan.Use + Loan.Type +
    margin_sd"
glm17 <- glm(model17, data=df.train, family="binomial", na.action=na.exclude)
summary(glm17)
pR2	(glm17)  
pred17 <- predict(glm17, df.train, type="response")
roc17 <- roc(df.train$WriteoffsDummy, pred17)
roc17$auc

# Dropping out some variables that aren't too powerful or intuitive
model18 <- "WriteoffsDummy ~ log(TOTAL.ASSETS + 1) + log(Total.current.assets + 1 - min(Total.current.assets)) + COGS.Margin + EBITDA.Margin. + 
	  log( Adjusted.EBITDA + 1 - min(Adjusted.EBITDA)) + Total.Liabilities.Total.Assets +
	 Total.Debt.Service +
	 max_pct_per_buyer +
	 Depth.of.Management + Product.Recognition + Financial.Flexibility + Financial.Strat.Quality +
    log(Working.Capital + 1 - min(Working.Capital)) + debt_service_to_ebitda +
    multiple_segments +
    log(Sales + 1 - min(Sales)) + Loan.Use + Loan.Type +
    margin_sd"
glm18 <- glm(model18, data=df.train, family="binomial", na.action=na.exclude)
summary(glm18)
pR2	(glm18)  
pred18 <- predict(glm18, df.train, type="response")
roc18 <- roc(df.train$WriteoffsDummy, pred18)
roc18$auc

# Dropping Loan.Type
model18a <- "WriteoffsDummy ~ log(TOTAL.ASSETS + 1) + log(Total.current.assets + 1 - min(Total.current.assets)) + COGS.Margin + EBITDA.Margin. + 
	  log( Adjusted.EBITDA + 1 - min(Adjusted.EBITDA)) + Total.Liabilities.Total.Assets +
	 Total.Debt.Service +
	 max_pct_per_buyer +
	 Depth.of.Management + Product.Recognition + Financial.Flexibility + Financial.Strat.Quality +
    log(Working.Capital + 1 - min(Working.Capital)) + debt_service_to_ebitda +
    multiple_segments +
    log(Sales + 1 - min(Sales)) + Loan.Use +
    margin_sd"
glm18a <- glm(model18a, data=df.train, family="binomial", na.action=na.exclude)
summary(glm18a)
pR2	(glm18a)
pred18a <- predict(glm18a, df.train, type="response")
roc18a <- roc(df.train$WriteoffsDummy, pred18a)
roc18a$auc


# Dropping Loan.Use
model18b <- "WriteoffsDummy ~ log(TOTAL.ASSETS + 1) + log(Total.current.assets + 1 - min(Total.current.assets)) + COGS.Margin + EBITDA.Margin. + 
	  log( Adjusted.EBITDA + 1 - min(Adjusted.EBITDA)) + Total.Liabilities.Total.Assets +
	 Total.Debt.Service +
	 max_pct_per_buyer +
	 Depth.of.Management + Product.Recognition + Financial.Flexibility + Financial.Strat.Quality +
    log(Working.Capital + 1 - min(Working.Capital)) + debt_service_to_ebitda +
    multiple_segments +
    log(Sales + 1 - min(Sales)) +  Loan.Type +
    margin_sd"
glm18b <- glm(model18b, data=df.train, family="binomial", na.action=na.exclude)
summary(glm18b)
pR2	(glm18b)
pred18b <- predict(glm18b, df.train, type="response")
roc18b <- roc(df.train$WriteoffsDummy, pred18b)
roc18b$auc


# Dropping both Loan.Type and Loan.Use
model18c <- "WriteoffsDummy ~ log(TOTAL.ASSETS + 1) + log(Total.current.assets + 1 - min(Total.current.assets)) + COGS.Margin + EBITDA.Margin. + 
	  log( Adjusted.EBITDA + 1 - min(Adjusted.EBITDA)) + Total.Liabilities.Total.Assets +
	 Total.Debt.Service +
	 max_pct_per_buyer +
	 Depth.of.Management + Product.Recognition + Financial.Flexibility + Financial.Strat.Quality +
    log(Working.Capital + 1 - min(Working.Capital)) + debt_service_to_ebitda +
    multiple_segments +
    log(Sales + 1 - min(Sales)) +
    margin_sd"
glm18c <- glm(model18c, data=df.train, family="binomial", na.action=na.exclude)
summary(glm18c)
pR2	(glm18c)  
pred18c <- predict(glm18c, df.train, type="response")
roc18c <- roc(df.train$WriteoffsDummy, pred18c)
roc18c$auc


# Run the same model with Writeoffs
# Dropping both Loan.Type and Loan.Use
model18cWO <- "WriteoffsDummy ~ log(TOTAL.ASSETS + 1) + log(Total.current.assets + 1 - min(Total.current.assets)) + COGS.Margin + EBITDA.Margin. + 
	  log( Adjusted.EBITDA + 1 - min(Adjusted.EBITDA)) + Total.Liabilities.Total.Assets +
	 Total.Debt.Service +
	 max_pct_per_buyer +
	 Depth.of.Management + Product.Recognition + Financial.Flexibility + Financial.Strat.Quality +
    log(Working.Capital + 1 - min(Working.Capital)) + debt_service_to_ebitda +
    multiple_segments +
    log(Sales + 1 - min(Sales)) +
    margin_sd"
glm18cWO <- glm(model18cWO, data=df.train, family="binomial", na.action=na.exclude)


# # Now try model 18c using on the final year as having a default  # This gives an error that the algorithm did not converge
model19 <- "default_last_year ~ log(TOTAL.ASSETS + 1) + log(Total.current.assets + 1 - min(Total.current.assets)) + COGS.Margin + EBITDA.Margin. + 
	  log( Adjusted.EBITDA + 1 - min(Adjusted.EBITDA)) + Total.Liabilities.Total.Assets +
	 Total.Debt.Service +
	 max_pct_per_buyer +
	 Depth.of.Management + Product.Recognition + Financial.Flexibility + Financial.Strat.Quality +
    log(Working.Capital + 1 - min(Working.Capital)) + debt_service_to_ebitda +
    multiple_segments +
    log(Sales + 1 - min(Sales)) +
    margin_sd + Sector.and.Perishability + Loan.Type + Loan.Use + Lending.Region"
glm19 <- glm(model19, data=df.train, family="binomial", na.action=na.exclude)
summary(glm19)
pR2	(glm19)  
pred19 <- predict(glm19, df.train, type="response")
roc19 <- roc(df.train$default, pred19)
roc19$auc

# #  Adding Cash...Banks + Accounts.receivable...members
model20<- "default_last_year ~ log(TOTAL.ASSETS + 1) + log(Total.current.assets + 1 - min(Total.current.assets)) + COGS.Margin + EBITDA.Margin. + 
	  log( Adjusted.EBITDA + 1 - min(Adjusted.EBITDA)) + Total.Liabilities.Total.Assets +
	 Total.Debt.Service +
	 max_pct_per_buyer +
	 Depth.of.Management + Product.Recognition + Financial.Flexibility + Financial.Strat.Quality +
    log(Working.Capital + 1 - min(Working.Capital)) + debt_service_to_ebitda +
    multiple_segments +
    log(Sales + 1 - min(Sales)) +
    margin_sd + Cash...Banks  + member_receivables_dummy"
glm20 <- glm(model20, data=df.train, family="binomial", na.action=na.exclude)
summary(glm20)
pR2	(glm20)  
pred20 <- predict(glm20, df.train, type="response")
roc20 <- roc(df.train$default, pred20)
roc20$auc

# #  adding neg wc to 18c
model21 <- "default ~ log(TOTAL.ASSETS + 1) + log(Total.current.assets + 1 - min(Total.current.assets)) + COGS.Margin + EBITDA.Margin. + 
	  log( Adjusted.EBITDA + 1 - min(Adjusted.EBITDA)) + Total.Liabilities.Total.Assets +
	 Total.Debt.Service +
	 max_pct_per_buyer +
	 Depth.of.Management + Product.Recognition + Financial.Flexibility + Financial.Strat.Quality +
    log(Working.Capital + 1 - min(Working.Capital)) + debt_service_to_ebitda +
    multiple_segments +
    log(Sales + 1 - min(Sales)) +
    margin_sd + negative_working_capital"
glm21 <- glm(model21, data=df.train, family="binomial", na.action=na.exclude)
summary(glm21)
pR2	(glm21)  
pred21 <- predict(glm21, df.train, type="response")
roc21 <- roc(df.train$default, pred21)
roc21$auc
