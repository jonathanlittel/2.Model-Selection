library(stargazer)
	library(pROC)
	library(pscl)

source('Preprocess data file.r')
metrics <- data.frame(AUC = 0,
	AIC = 0)
rownames(metrics) <- 'temp'


	modelCols <- c('WO', 'Tenor_years', 'Sales_log', 'sales_concent_cat', 'Depth.of.Management',
		'gross_margin_cat', 'Financial.Flexibility', 'past_arrears', 'wc_sales_cat',  
		'Financial.Strat.Quality')


df.train <- df.train %>%
mutate(FundedDebt = log(Current.portion.of.Root.Capital.long.term.debt +
				Current.portion.of.long.term.debt + Root.Capital.long.term.debt + 
				Long.term.debt ))

# df.train$DSCR_bin <- cut(df.train$DSCRadj, c(-Inf, -0.0001, 0.0001, 0.5, 1, Inf)) #ifelse(df.train$DSCRadj<0.5, 1, 0)


# Model 1
	modelCols1 <- c('WO', 'Tenor_years', 'sales_concent_cat', 'Depth.of.Management',
		'gross_margin_cat', 'Financial.Flexibility', 'past_arrears', 'wc_sales_cat',  
		'Financial.Strat.Quality')

	df.train.model <- df.train[,names(df.train) %in% modelCols1]
	glm1 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.train$WO,predict(glm1, df.train, type="response"))
	met_row <- c(roc$auc, glm1$aic)
	metrics <- rbind(metrics, met_row)

# Model 2
	modelCols2 <- c('WO', 'Tenor_years', 'Sales_log', 'Depth.of.Management',
		'gross_margin_cat', 'Financial.Flexibility', 'past_arrears', 'wc_sales_cat',  
		'Financial.Strat.Quality')

	df.train.model <- df.train[,names(df.train) %in% modelCols2]
	glm2 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.train$WO,predict(glm2, df.train, type="response"))
	met_row <- c(roc$auc, glm2$aic)
	metrics <- rbind(metrics, met_row)

# Model 3
	modelCols3 <- c('WO', 'Tenor_years', 'Sales_log', 'sales_concent_cat',
		'gross_margin_cat', 'Financial.Flexibility', 'past_arrears', 'wc_sales_cat',  
		'Financial.Strat.Quality')
	df.train.model <- df.train[,names(df.train) %in% modelCols3]
	glm3 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.train$WO,predict(glm3, df.train, type="response"))
	met_row <- c(roc$auc, glm3$aic)
	metrics <- rbind(metrics, met_row)

# Model 4
	modelCols4 <- c('WO', 'Tenor_years', 'Sales_log', 'sales_concent_cat', 'Depth.of.Management',
		'Financial.Flexibility', 'past_arrears', 'wc_sales_cat',  
		'Financial.Strat.Quality')

	df.train.model <- df.train[,names(df.train) %in% modelCols4]
	glm4 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.train$WO,predict(glm4, df.train, type="response"))
	met_row <- c(roc$auc, glm4$aic)
	metrics <- rbind(metrics, met_row)

# Model 5

	modelCols5 <- c('WO', 'Tenor_years', 'Sales_log', 'sales_concent_cat', 'Depth.of.Management',
		'gross_margin_cat', 'past_arrears', 'wc_sales_cat',  
		'Financial.Strat.Quality')
	df.train.model <- df.train[,names(df.train) %in% modelCols5]
	glm5 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.train$WO,predict(glm5, df.train, type="response"))
	met_row <- c(roc$auc, glm5$aic)
	metrics <- rbind(metrics, met_row)

# Model 6
	modelCols6 <- c('WO', 'Tenor_years', 'Sales_log', 'sales_concent_cat', 'Depth.of.Management',
		'gross_margin_cat', 'Financial.Flexibility', 'wc_sales_cat',  
		'Financial.Strat.Quality')
	df.train.model <- df.train[,names(df.train) %in% modelCols6]
	glm6 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.train$WO,predict(glm6, df.train, type="response"))
	met_row <- c(roc$auc, glm6$aic)
	metrics <- rbind(metrics, met_row)

# Model 7
	modelCols7 <- c('WO', 'Tenor_years', 'Sales_log', 'sales_concent_cat', 'Depth.of.Management',
		'gross_margin_cat', 'Financial.Flexibility', 'past_arrears', 
		'Financial.Strat.Quality')
	df.train.model <- df.train[,names(df.train) %in% modelCols7]
	glm7 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.train$WO,predict(glm7, df.train, type="response"))
	met_row <- c(roc$auc, glm7$aic)
	metrics <- rbind(metrics, met_row)

# Model 8
	modelCols8 <- c('WO', 'Tenor_years', 'Sales_log', 'sales_concent_cat', 'Depth.of.Management',
		'gross_margin_cat', 'Financial.Flexibility', 'past_arrears', 'wc_sales_cat' 
		)
	df.train.model <- df.train[,names(df.train) %in% modelCols8]
	glm8 <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.train$WO,predict(glm8, df.train, type="response"))
	met_row <- c(roc$auc, glm8$aic)
	metrics <- rbind(metrics, met_row)

# Model 9
	modelCols9 <- c('WO', 'Tenor_years', 'Sales_log', 'sales_concent_cat', 'Depth.of.Management',
		'gross_margin_cat', 'Financial.Flexibility', 'past_arrears', 'wc_sales_cat',  
		'Financial.Strat.Quality')
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
	row.names(metrics) <-1:nrow(metrics)

rownames(metrics) <- c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7", "Model 8", 
	"Model 9" )
# Print html file
	compTable <- stargazer(glm1, glm2, glm3, glm4, glm5, glm6, glm7, glm8, glm9, t(metrics), type = "html",
                       ci = TRUE)
	# metricTable <- stargazer(metrics)
	write(compTable, file = "model_comparison_05.26.16.html", append = TRUE)
	# write(metricTable, file = "model_comparisons_05.10.16.html", append = TRUE)
	file.show("model_comparison_05.26.16.html")


