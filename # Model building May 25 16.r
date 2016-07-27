# Multivariate credit default logit model 
# Preprocess data and loan training data
	#load libraries and data
		library(pROC)
		library(pscl)
		library(stargazer)
		library(caret)
		library(dplyr)
		options(scipen=99, digits=3)
	# Run script to load and process data
		wd <- paste0("C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/",
			"Risk Profile/PD Model/2.Model Selection", sep="")
		setwd(wd)
		source('Preprocess data file.r')

# Define unction to produce AUC, AIC and coefs
### 
	fit1 <- function(df, m) {
		df.train.model <- df[,names(df) %in% m]
		glmx <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
		return(coef(glmx))
		# return(roc(df$WO,predict(glmx, df, type="response"))$auc)
	}

	auc1 <- function(df, m) {
		df.train.model <- df[,names(df) %in% m]
		glmx <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
		roc <- roc(df$WO,predict(glmx, df, type="response"))
		met_row <- c(roc$auc, glmx$aic)
		# names(met_row) <- c("AUC", "AIC")
		return(met_row)
	}



# Model using variables labeled as 'High'
	# Fit model
	modelCols <- c('WO', 'Tenor_years', 'sales_concent_cat', 'Depth.of.Management',
		'gross_margin_cat', 'Financial.Flexibility', 'past_arrears', 'wc_sales_cat',  
		'Financial.Strat.Quality')
		df.train.model <- df.train[,names(df.train) %in% modelCols]
		glm.high <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
		roc <- roc(df.train$WO,predict(glm.high, df.train, type="response"))
		met_row <- c(roc$auc, glm.high$aic)
		metrics <- rbind(metrics, met_row)

	# Print results
	confint(glm.high)
	print(glm.high)
	exp(coef(glm.high))
	summary(glm.high)
	# Confusion Matrix
	cutoff <- 0.15
	pred <- as.factor(ifelse(predict(glm.high, df.train.model, type='response')<cutoff, 0, 1 ))
	confusionMatrix(pred, 
			df.train.model$WO)


# look in to square at some point
		modelCols <- c('WO', 'working_capital_to_sales',
		'working_capital_to_sales_sq')
		df.train.model <- df.train[,names(df.train) %in% modelCols]
		glm.sales <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
		roc <- roc(df.train$WO,predict(glm.high, df.train, type="response"))
		met_row <- c(roc$auc, glm.high$aic)
		metrics <- rbind(metrics, met_row)

# predict(glm5, df.train, type="response"))
