################ #######
#load libraries and data
	library(pROC)
	library(pscl)
	library(caret)
	library(dplyr)
	options(scipen=99, digits=3)
# Run script to load and process data
	wd <- paste0("C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/",
		"Risk Profile/PD Model/2.Model Selection", sep="")
	setwd(wd)
	source('Preprocess data file.r')

# Define unction to produce AUC, AIC and coefs
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

## Short list
	###
	sl <- c('WO', 'Tenor_years', 'Sales_log', 'Sales.Concentration.to.a.Single.Buyer', 'TotalAssets_log', 'Segmental.Diversification',
		 'Depth.of.Management', 'margin_sd', 'Product.Recognition',                                       # 1st: 'Exports.vs..Local.Sales', - currently all NA
		'Longevity...Quality.of.Relationship.with.Key.Buyer.s.','Years_of_Sales', 'TOTAL.EQUITY',
		'Sales_Growth_sd', 'Audited.vs..non.audited.financials..1', 'Financial.Flexibility',
		'past_arrears', 'Funded.Debt...EBITDA..Risk.Rating.Score.', 'WorkingCapital_log',
		'Total.Asset.Turnover', 'Financial.Strat.Quality', 'Cash.Conversion.Cycle.',
		'interest_coverage', 'working_capital_to_sales')

	new_vars <- c('Revenue_growth', 'Years_of_Sales', 'Sales_growth_avg', 'Sales_Growth', 'Sales_Growth_sd', 'DtoE_adding_RC_Loan',
		'DtoE','DSCRadj', 'LoanAmountOverSales','DSCRadj_plus_20pctLoanAmt', 'Sales_CAGR', 'Gross_Margin_range', 'gross_profit_margin',
		'fundedDebttoEBITDA_cat') 

	ids <- c('LoanID', 'Year')

# Subset training data for above columns
	df <- select(df.train, one_of(c(new_vars, sl)))

# Run base model
	# aucs <- data.frame(AUC=0, AIC=0) # blank df
	modelCols <- c('WO', 'Tenor_years')
	s <- auc1(df=df, m = modelCols)
	aucs <- data.frame(AUC=s[1],
		AIC=s[2])

# 1. # Run functions
	# modelCols <- c("WO", "Tenor_years", sl[2])
		metricName <- 'Base'
		for (i in 3:length(sl)) {
			modelCols <- c('WO', 'Tenor_years', sl[i])
			s <- auc1(df=df, m = modelCols)
			metricName <- c(metricName, sl[i])
			aucs <- rbind(aucs, s)
		}

		aucs <- cbind(metricName, aucs)
		names(aucs)[1] <- "Metric"

# Plot of AUCs by dual variable regression
	p <- ggplot(aucs, aes(x=Metric, y=AUC))	
	q <- p + geom_bar(colour="grey", stat="identity")  + coord_flip(ylim = c(0.5, 0.8))
	q + geom_hline(yintercept = aucs[aucs$Metric=="Base", "AUC"] )



# 2. # Run functions
	# modelCols <- c("WO", "Tenor_years", sl[2])
			# aucs <- data.frame(AUC=0, AIC=0) # blank df
	modelCols <- c('WO', 'Tenor_years')
	s <- auc1(df=df, m = modelCols)
	aucs <- data.frame(AUC=s[1],
		AIC=s[2])


		metricName <- 'Base'
		for (i in 1:length(new_vars)) {
			modelCols <- c('WO', 'Tenor_years', new_vars[i])
			s <- auc1(df=df, m = modelCols)
			metricName <- c(metricName, new_vars[i])
			aucs <- rbind(aucs, s)
		}

		aucs <- cbind(metricName, aucs)
		names(aucs)[1] <- "Metric"

# Plot of AUCs by dual variable regression
	p <- ggplot(aucs, aes(x=Metric, y=AUC))	
	q <- p + geom_bar(colour="grey", stat="identity")  + coord_flip(ylim = c(0.5, 0.8))
	q + geom_hline(yintercept = aucs[aucs$Metric=="Base", "AUC"], color='brown' )


	# wald.test(b = coef(glms[[1]]), Sigma = vcov(glms[[1]]), Terms = 4:6)

	# exp(coef(glms[1]))

	# confusionMatrix()

# Write output for robbie to check
# 	Subset training data for above columns
	df.r <- select(df.rap, one_of(c(ids, sl, new_vars )))
	write.csv(df.r, 'calculation_check.csv')
