################ #######
#load libraries and data
	library(pROC)
	library(pscl)
	library(stargazer)
	library(caret)
	library(dplyr)
	options(scipen=99, digits=3)

# Run script to load and process data
	source('Preprocess data file.r')

metrics <- data.frame(ROC = 0,
	AIC = 0)
rownames(metrics) <- 'temp'

# Model 0
	modelCols0 <- c("WO", "Sales_log", "WorkingCapital_log")

	df.train.model <- df.train[,names(df.train) %in% modelCols0]
	glm0 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.train$WO,predict(glm0, df.train, type="response"))
	met_row <- c(roc$auc, glm0$aic)
	metrics <- rbind(metrics, met_row)


# Model 1
	modelCols1 <- c("WO", "Sales_log", "Tenor_days"
		)

	df.train.model <- df.train[,names(df.train) %in% modelCols1]
	glm1 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.train$WO,predict(glm1, df.train, type="response"))
	met_row <- c(roc$auc, glm1$aic)
	metrics <- rbind(metrics, met_row)

# Model 2
	modelCols2 <- c("WO", "Sales_log", "Tenor_days",
		"WorkingCapital_log")

	df.train.model <- df.train[,names(df.train) %in% modelCols2]
	glm2 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.train$WO,predict(glm2, df.train, type="response"))
	met_row <- c(roc$auc, glm2$aic)
	metrics <- rbind(metrics, met_row)

# Model 3
	modelCols3 <- c("WO", "Sales_log", "Tenor_days",
		"TotalAssets_log")
	df.train.model <- df.train[,names(df.train) %in% modelCols3]
	glm3 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.train$WO,predict(glm3, df.train, type="response"))
	met_row <- c(roc$auc, glm3$aic)
	metrics <- rbind(metrics, met_row)

# Model 4
	modelCols4 <- c("WO", "Sales_log", "Tenor_days",
		"Depth.of.Management")
	df.train.model <- df.train[,names(df.train) %in% modelCols4]
	glm4 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.train$WO,predict(glm4, df.train, type="response"))
	met_row <- c(roc$auc, glm4$aic)
	metrics <- rbind(metrics, met_row)

# Model 5
	modelCols5 <- c("WO", "Sales_log", "Tenor_days",
		'Gross_Margin_Variation')
	df.train.model <- df.train[,names(df.train) %in% modelCols5]
	glm5 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.train$WO,predict(glm5, df.train, type="response"))
	met_row <- c(roc$auc, glm5$aic)
	metrics <- rbind(metrics, met_row)

# Model 6
	modelCols6 <- c("WO", "Sales_log", "Tenor_days",
		"Depth.of.Management", 'Gross_Margin_Variation')
	df.train.model <- df.train[,names(df.train) %in% modelCols6]
	glm6 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.train$WO,predict(glm6, df.train, type="response"))
	met_row <- c(roc$auc, glm6$aic)
	metrics <- rbind(metrics, met_row)

# Model 7
	modelCols7 <- c("WO", "Sales_log", "Tenor_days",
		"Depth.of.Management", 'Rev_growth_sq')
	df.train.model <- df.train[,names(df.train) %in% modelCols7]
	glm7 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.train$WO,predict(glm7, df.train, type="response"))
	met_row <- c(roc$auc, glm7$aic)
	metrics <- rbind(metrics, met_row)

# Model 8
	modelCols8 <- c("WO", "Sales_log", "Tenor_days",
		'Revenue_growth')
	df.train.model <- df.train[,names(df.train) %in% modelCols8]
	glm8 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.train$WO,predict(glm8, df.train, type="response"))
	met_row <- c(roc$auc, glm8$aic)
	metrics <- rbind(metrics, met_row)

# Model 9
	modelCols9 <- c("WO", "Sales_log", "Tenor_days",
		'Revenue_growth', 'Rev_growth_sq')
	df.train.model <- df.train[,names(df.train) %in% modelCols9]
	glm9 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.train$WO,predict(glm9, df.train, type="response"))
	met_row <- c(roc$auc, glm9$aic)
	metrics <- rbind(metrics, met_row)


# #### Train final model on both test and training
#   modelColsFinal <- c("WO", "Sales_log", "WorkingCapital_log",
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

rownames(metrics) <- c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7", "Model 8", 
	"Model 9", "Model 10" )
# Print html file
	compTable <- stargazer(glm0, glm1, glm2, glm3, glm4, glm5, glm6, glm7, glm8, glm9, t(metrics), type = "html",
                       ci = TRUE)
	# metricTable <- stargazer(metrics)
	write(compTable, file = "model_comparisons_05.10.16.html", append = FALSE)
	# write(metricTable, file = "model_comparisons_05.10.16.html", append = TRUE)
	file.show("model_comparisons_05.10.16.html")