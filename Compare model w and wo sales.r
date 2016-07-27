
# source('Preprocess data file.r')

# Load custom function for graphing
source('# Prediction Plot Function.r')

metrics <- data.frame(AUC = 0,
	AIC = 0)
rownames(metrics) <- 'temp'

cutoff <- 0.15

# Model 1
	modelCols1 <- c('WO', 'Tenor_years', 'sales_concent_cat', 'Depth.of.Management',
		'gross_margin_cat', 'Financial.Flexibility', 'past_arrears', 'wc_sales_cat',  
		'Financial.Strat.Quality')

	df.train.model <- df.train[,names(df.train) %in% modelCols1]
	glm1 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.test$WO,predict(glm1, df.test, type="response"))
	met_row <- c(roc$auc, glm1$aic)
	metrics <- rbind(metrics, met_row)
	df.test$pd <- predict(glm1, df.test, type='response')
	 df.test$predWO_cut <- as.factor(ifelse(df.test$pd>cutoff,1,0))
	 confusionMatrix(df.test$predWO_cut, df.test$WO)
	plot(roc)
	plot_pred_type_distribution(df.test, cutoff)


# Model 9
	modelCols9 <- c('WO', 'Tenor_years', 'Sales_log', 'sales_concent_cat', 'Depth.of.Management',
		'gross_margin_cat', 'Financial.Flexibility', 'past_arrears', 'wc_sales_cat',  
		'Financial.Strat.Quality')
	df.train.model <- df.train[,names(df.train) %in% modelCols9]
	glm9 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.test$WO,predict(glm9, df.test, type="response"))
	met_row <- c(roc$auc, glm9$aic)
	metrics <- rbind(metrics, met_row)
	df.test$pd <- predict(glm9, df.test, type='response')
	df.test$predWO_cut <- as.factor(ifelse(df.test$pd>cutoff,1,0))
	confusionMatrix(df.test$predWO_cut, df.test$WO)
	plot(roc)

metrics <- metrics[-1,]
	row.names(metrics) <-1:nrow(metrics)


#####
# plot_pred_type_distribution(df.rap, cutoff)

#   # Produce underwriting pds for all loans in dataset
#   df.rap$pd <- predict(glmFinal, df.rap, type="response")
  
  
# ## Confusion Matrix on total
#   df.rap$pd1 <- predict(glmFinal, df.rap, type="response")
#   df.rap$pd <- predict(glmFinal, df.rap, type="response")
#   df.rap$predWO_cut <- as.factor(ifelse(df.rap$pd>cutoff,1,0))
#   confusionMatrix(df.rap$predWO_cut, df.rap$WriteoffsDummy)



rownames(metrics) <- c("Model 1", "Model 2" )
# Print html file
	compTable <- stargazer(glm1, glm9, t(metrics), type = "html",
                       ci = TRUE)
	# metricTable <- stargazer(metrics)
	write(compTable, file = "model_comparison_candidates_05.26.16.html", append = FALSE)
	# write(metricTable, file = "model_comparisons_05.10.16.html", append = TRUE)
	file.show("model_comparison_candidates_05.26.16.html")


# Model X
	modelCols9 <- c('WO', 'Tenor_years', 'Sales_log', 'DSCRadj', 'Depth.of.Management',
		'gross_margin_cat', 'FundedDebtToEBITDA', 'LoanAmountOverSales', 'wc_sales_cat' 
		)
	df.train.model <- df.train[,names(df.train) %in% modelCols9]
	glm9 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.test$WO,predict(glm9, df.test, type="response"))
	met_row <- c(roc$auc, glm9$aic)
	metrics <- rbind(metrics, met_row)
	df.test$pd <- predict(glm9, df.test, type='response')
	df.test$predWO_cut <- as.factor(ifelse(df.test$pd>cutoff,1,0))
	confusionMatrix(df.test$predWO_cut, df.test$WO)
	plot(roc)


