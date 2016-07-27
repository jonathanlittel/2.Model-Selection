################ #######
#load libraries and data
	library(pROC)
	library(pscl)
	library(stargazer)
	library(caret)
	library(dplyr)
	options(scipen=99, digits=3)
# Load info for writeoffs and inactive since 12/31/15
# Data is updated through about 4/30
	wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/PD Model/1.Dataset Cleaning and Prep/data"
	setwd(wd)
	filename <- "List of Write-offs.csv"
	wo <- read.csv(filename, skip=1)
	colnames(wo) <- c('LoanID', 'Actual_WOs', 'Manual_WOs','Tag' )

# Load rap dataset and merge		
	wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/PD Model/3.Outputs"
	setwd(wd)
	filename <-  "rap_data_Q4_15_4.29.16.csv"
	df.rap <- read.csv(filename, header=TRUE, sep=",")

# Merge in new WO	 		
	df.rap <- subset(df.rap, last_year==1)
	wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/PD Model/2.Model Selection"
	setwd(wd)
	df.rap <- merge(df.rap, wo, by="LoanID")



# Create a new variable for 'active' if a loan has been repaid or marked as a writeoff
	df.rap <- mutate(df.rap,
		active = ifelse(Tag==3, 1, 0),
		WO = ifelse(Tag==1, 1, 0) )
	df.rap.inactive <- filter(df.rap, active==0)

#####################################################
# Split data into training and test set             #
# Important that this remains consistent throughout # 
#####################################################
	uniqueIDs <- unique(df.rap.inactive$LoanID)
	sample_size <- floor(0.80 * length(uniqueIDs))
	set.seed(9)
	LoanIDtrain <- sample(uniqueIDs, sample_size)
	df.train <- df.rap.inactive[df.rap.inactive$LoanID %in% LoanIDtrain,]
	df.test <- df.rap.inactive[!df.rap.inactive$LoanID %in% LoanIDtrain,] 

metrics <- data.frame(ROC = 0,
	AIC = 0)
rownames(metrics) <- 'temp'

library(leaps)
# Subset selection
		modelCols <- c("WO",  "Tenor_days",
		'Financial.Strat.Quality', 'Funded.Debt...EBITDA..Risk.Rating.Score.', 'Funded_debt_to_EBITDA',
		'Max_Cash_Conversion_Cycle', 'EBITDA.Total.Debt.Service',
		'Total.Asset.Turnover')
		df.train.model <- df.train[,names(df.train) %in% modelCols]
	regfit.full <- regsubsets(df.train.model$WO ~., df.train.model, nvmax = 7, method='exhaustive')
	summary(regfit.full)
# Examine outcome 
	reg.summary <- summary(regfit.full)
	reg.summary$rsq
	fit.compare <- ggplot(reg.summary)


	par(mfrow=c(2,2))
	plot(reg.summary$rss, xlab="", ylab = "RSS", type='l')
	plot(reg.summary$adjr2, xlab="", ylab = "Adjusted R Squared", type='l')
	plot(reg.summary$bic, xlab="", ylab = "BIC", type='l')


# # Model 1
# 	modelCols1 <- c("WO",  "Tenor_days"
# 		)

# 	df.train.model <- df.train[,names(df.train) %in% modelCols1]
# 	glm1 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
# 	roc <- roc(df.train$WO,predict(glm1, df.train, type="response"))
# 	met_row <- c(roc$auc, glm1$aic)
# 	metrics <- rbind(metrics, met_row)

# # Model 2
# 	modelCols2 <- c("WO", "WorkingCapital_log", "Tenor_days")

# 	df.train.model <- df.train[,names(df.train) %in% modelCols2]
# 	glm2 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
# 	roc <- roc(df.train$WO,predict(glm2, df.train, type="response"))
# 	met_row <- c(roc$auc, glm2$aic)
# 	metrics <- rbind(metrics, met_row)

# Model 3
	modelCols3 <- c("WO", "Tenor_days"
		)
	df.train.model <- df.train[,names(df.train) %in% modelCols3]
	glm3 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.train$WO,predict(glm3, df.train, type="response"))
	met_row <- c(roc$auc, glm3$aic)
	metrics <- rbind(metrics, met_row)

# Model 4
	modelCols4 <- c("WO",  "Tenor_days",
		"Financial.Strat.Quality")
	df.train.model <- df.train[,names(df.train) %in% modelCols4]
	glm4 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.train$WO,predict(glm4, df.train, type="response"))
	met_row <- c(roc$auc, glm4$aic)
	metrics <- rbind(metrics, met_row)

# Model 5
	modelCols5 <- c("WO",  "Tenor_days",
		'Gross_Margin_Variation')
	df.train.model <- df.train[,names(df.train) %in% modelCols5]
	glm5 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.train$WO,predict(glm5, df.train, type="response"))
	met_row <- c(roc$auc, glm5$aic)
	metrics <- rbind(metrics, met_row)

df.train$Funded_debt_to_EBITDA <- ( df.train$Current.portion.of.Root.Capital.long.term.debt + df.train$Current.portion.of.long.term.debt + 
												df.train$Root.Capital.long.term.debt + df.train$Long.term.debt ) / df.train$EBITDA
sum(is.infinite(df.train$Funded_debt_to_EBITDA))
df.train$Funded_debt_to_EBITDA[is.infinite(df.train$Funded_debt_to_EBITDA),] <- NA

# Model 6
	modelCols6 <- c("WO",  "Tenor_days",
		"Funded_debt_to_EBITDA")
	df.train.model <- df.train[,names(df.train) %in% modelCols6]
	glm6 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.train$WO,predict(glm6, df.train, type="response"))
	met_row <- c(roc$auc, glm6$aic)
	metrics <- rbind(metrics, met_row)

# Model 7
	modelCols7 <- c("WO",  "Tenor_days",
		"Max_Cash_Conversion_Cycle")
	df.train.model <- df.train[,names(df.train) %in% modelCols7]
	glm7 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.train$WO,predict(glm7, df.train, type="response"))
	met_row <- c(roc$auc, glm7$aic)
	metrics <- rbind(metrics, met_row)

# Model 8
	modelCols8 <- c("WO",  "Tenor_days",
		'EBITDA.Total.Debt.Service')
	df.train.model <- df.train[,names(df.train) %in% modelCols8]
	glm8 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.train$WO,predict(glm8, df.train, type="response"))
	met_row <- c(roc$auc, glm8$aic)
	metrics <- rbind(metrics, met_row)

# Model 9
	modelCols9 <- c("WO",  "Tenor_days",
		'Total.Asset.Turnover')
	df.train.model <- df.train[,names(df.train) %in% modelCols9]
	glm9 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.train$WO,predict(glm9, df.train, type="response"))
	met_row <- c(roc$auc, glm9$aic)
	metrics <- rbind(metrics, met_row)

# Model 10
	modelCols10 <- c("WO",  "Tenor_days",
		'Max_Cash_Conversion_Cycle')
	df.train.model <- df.train[,names(df.train) %in% modelCols10]
	glm10 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.train$WO,predict(glm10, df.train, type="response"))
	met_row <- c(roc$auc, glm10$aic)
	metrics <- rbind(metrics, met_row)

# Model 11
	modelCols11 <- c("WO",  "Tenor_days",
		'Financial.Strat.Quality')
	df.train.model <- df.train[,names(df.train) %in% modelCols11]
	glm11 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.train$WO,predict(glm11, df.train, type="response"))
	met_row <- c(roc$auc, glm11$aic)
	metrics <- rbind(metrics, met_row)

# Model 12
	modelCols12 <- c("WO",  "Tenor_days",
		'Max_Cash_Conversion_Cycle', 'Financial.Strat.Quality')
	df.train.model <- df.train[,names(df.train) %in% modelCols12]
	glm12 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.train$WO,predict(glm12, df.train, type="response"))
	met_row <- c(roc$auc, glm12$aic)
	metrics <- rbind(metrics, met_row)

# Model 13
	modelCols13 <- c("WO",  "Tenor_days",
		'Max_Cash_Conversion_Cycle', 'Financial.Strat.Quality', 'Total.Asset.Turnover')
	df.train.model <- df.train[,names(df.train) %in% modelCols13]
	glm13 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.train$WO,predict(glm13, df.train, type="response"))
	met_row <- c(roc$auc, glm13$aic)
	metrics <- rbind(metrics, met_row)

# Model 14
	modelCols14 <- c("WO",  "Tenor_days",
		'Max_Cash_Conversion_Cycle', 'Financial.Strat.Quality', 'Total.Asset.Turnover',
		'EBITDA.Total.Debt.Service')
	df.train.model <- df.train[,names(df.train) %in% modelCols14]
	glm14 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.train$WO,predict(glm14, df.train, type="response"))
	met_row <- c(roc$auc, glm14$aic)
	metrics <- rbind(metrics, met_row)

	df.train.model <- df.train.model[complete.cases(df.train.model),]
	glm14 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	backwards <- step(glm14) 
# #### Train final model on both test and training
#   modelColsFinal <- c("WO",  "WorkingCapital_log",
#                       "past_arrears",
#                       "margin_sd","Depth.of.Management",
#                       "Loan.Type")
#   # Train model on all inactive loans
#   df.train.model <- df.train[,names(df.train) %in% modelColsFinal] 
#   glmFinal <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
# 	roc <- roc(df.train$WO,predict(glmFinal, df.train, type="response"))
# 	met_row <- c(roc$auc, glmFinal$aic)
# 	metrics <- rbind(metrics, met_row)

metrics <- metrics[-1,]

rownames(metrics) <- c(1:nrow(metrics))
# Print html file
	compTable <- stargazer(glm3, glm4, glm5, glm6, glm7, glm8, glm9, glm10,  t(metrics[1:8,]), type = "html",
                       ci = TRUE) # , order=c(2,3,6,4,1,5,7,8)
	# metricTable <- stargazer(metrics)
	write(compTable, file = "model_comparison_Fin_Strat_05.10.16.html", append = FALSE)

		compTable <- stargazer(glm11, glm12, glm13, glm14, t(metrics[9:12,]), type = "html",
                       ci = TRUE)
	write(compTable, file = "model_comparison_Fin_Strat_05.10.16.html", append = TRUE)
	# write(metricTable, file = "model_comparisons_05.10.16.html", append = TRUE)
	file.show("model_comparison_Fin_Strat_05.10.16.html")