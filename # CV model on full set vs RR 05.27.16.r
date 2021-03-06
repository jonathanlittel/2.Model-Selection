# Multivariate credit default logit model 
# Preprocess data and loan training data
	#load libraries and data
		library(pROC)
		library(pscl)
		# library(stargazer)
		library(caret)
		library(dplyr)
		options(scipen=99, digits=3)
	# Run script to load and process data
		wd <- paste0("C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/",
			"Risk Profile/PD Model/2.Model Selection", sep="")
		setwd(wd)
		source('Preprocess data file.r') # Cleans and creates variables
# Load custom function for graphing
	source('# Prediction Plot Function.r')

require(caret)
# Create folds
	set.seed(527)
	folds <- createFolds(df.rap.inactive$WO)

cutoff <- 0.15

# Train a model on each of five folds

   	metrics_new_mod <- data.frame(
   		AUC=0,
   		AIC=0)

	modelCols9 <- c('WO', 'Tenor_years', 'sales_concent_cat', 'Depth.of.Management',
		'gross_margin_cat', 'Financial.Flexibility', 'past_arrears', 'wc_sales_cat',  
		'Financial.Strat.Quality')
		df.train.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelCols9]

	for (i in 1:5) {
		f <- paste("Fold0",i, sep="")
		fold_list <- get(f, folds)
		fold <- unlist(fold_list)
		df.trn <- df.train.model[-c(fold),]
		df.tst <- df.train.model[c(fold),]
		name <- paste('glm', i, sep="")
		glm <- glm(WO ~ ., data=df.trn, family="binomial", na.action=na.exclude)
		assign(name, glm)
		roc <- roc(df.tst$WO,predict(glm, df.tst, type="response"))
		met_row <- c(roc$auc, glm$aic)
		metrics_new_mod <- rbind(metrics_new_mod, met_row)
		pd <- predict(glm, df.tst, type='response')
		wo_cut <- as.factor(ifelse(pd>cutoff,"Writeoff","Non_Writeoff"))
		cm <- confusionMatrix(wo_cut, df.tst$WO, 
			positive="Writeoff")
		cm_name <- paste("cm", i, sep="")
		assign(cm_name, cm)
		# p <- plot_pred_type_distribution(df.tst, cutoff)
		# p_name <- paste("p", i, sep="")
		# assign(p_name, p)
	}
	metrics_new_mod <- metrics_new_mod[-1,]
	metrics_new_mod["average",] <- apply(metrics_new_mod, 2, mean)


# Train a model on each of five folds using the risk rating glm model

   	metrics_rr <- data.frame(
   		AUC=0,
   		AIC=0)
   	
	modelColsrr <- c('WO', 'RiskRating')
		df.train.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelColsrr]

	for (i in 1:5) {
		f <- paste("Fold0",i, sep="")
		fold_list <- get(f, folds)
		fold <- unlist(fold_list)
		df.trn <- df.train.model[-c(fold),]
		df.tst <- df.train.model[c(fold),]
		name <- paste('glmRR', i, sep="")
		glm <- glm(WO ~ ., data=df.trn, family="binomial", na.action=na.exclude)
		assign(name, glm)
		roc <- roc(df.tst$WO,predict(glm, df.tst, type="response"))
		met_row <- c(roc$auc, glm$aic)
		metrics_rr <- rbind(metrics_rr, met_row)
		pd <- predict(glm, df.tst, type='response')
		wo_cut <- as.factor(ifelse(pd>cutoff,"Writeoff","Non_Writeoff"))
		cm <- confusionMatrix(wo_cut, df.tst$WO, 
			positive="Writeoff")
		cm_name <- paste("cmRR", i, sep="")
		assign(cm_name, cm)
	}
	metrics_rr <- metrics_rr[-1,]
	metrics_rr["average",] <- apply(metrics_rr, 2, mean)

# Using caret to to CV on full dataset

	modelCols9 <- c('WO', 'Tenor_years', 'sales_concent_cat', 'Depth.of.Management',
		'gross_margin_cat', 'Financial.Flexibility', 'past_arrears', 'wc_sales_cat',  
		'Financial.Strat.Quality')
	df.train.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelCols9]

   	tc <- trainControl("cv",
   			number=5, 
   			savePred=TRUE,
   			classProbs = TRUE,
   			summaryFunction = twoClassSummary)
 	# train_control <- trainControl(method="repeatedcv", number=5, repeats=2)
   	set.seed(1348)
   	glm.mod.all <- train(WO ~ .	,
   		data = df.train.model,
   		method='glm',
   		trControl=tc,
   		metric = "ROC",
   		family='binomial') 

	pd <- predict(glm.mod.all, df.test, type="prob")[,2]
	roc <- roc(df.test$WO, pd)
	df.test$predWO_cut <- as.factor(ifelse(pd>cutoff,"Writeoff", "Non_Writeoff"))
	confusionMatrix(df.test$predWO_cut, df.test$WO)
	summary(glm.mod.all$pred)
	glm.mod.all$results$ROC  # print the cv AUC
	confint(glm.mod.all$finalModel, level = 0.75)
	confint(glm.mod.all$finalModel, level = 0.33)


	# model on all data
		df.rap.inactive$pd <- predict(glm.mod.all, df.rap.inactive, type="prob")[,2]
		pd <- predict(glm.mod.all, df.rap.inactive, type="prob")[,2]
		roc <- roc(df.rap.inactive$WO, pd)
		df.rap.inactive$predWO_cut <- as.factor(ifelse(pd>cutoff,"Writeoff", "Non_Writeoff"))
		confusionMatrix(df.rap.inactive$predWO_cut, df.rap.inactive$WO, 
			positive="Writeoff")

	# RR on all data	
		modelColsRR <- c('WO', 'RiskRating')
		df.train.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelColsRR]

		glm.rr <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
		pd <- predict(glm.rr, df.rap.inactive, type="response")
		roc <- roc(df.rap.inactive$WO, pd)
		predWO_cut <- as.factor(ifelse(pd>cutoff,"Writeoff", "Non_Writeoff"))
		confusionMatrix(predWO_cut, df.rap.inactive$WO, 
			positive="Writeoff")

	# Predict confidence intervals for pd
		pr <- predict(glm.mod.all$finalModel, df.rap, type="response", se.fit = TRUE)	
		df.rap$pd_lower <- pr$fit - qnorm(0.95) * pr$se.fit
		df.rap$pd_upper <- pr$fit + qnorm(0.95) * pr$se.fit 

######################
##    Save output   ##
######################


# Matrix with coefficients
	coefs <- data.frame((coef(glm1)))
	coefs[,2] <- data.frame(coef(glm2))
	coefs[,3] <- data.frame(coef(glm3))
	coefs[,4] <- data.frame(coef(glm4))
	coefs[,5] <- data.frame(coef(glm5))	
	coefs[,6] <- apply(coefs, 1, mean)	
	names(coefs) <- c('m1', 'm2', 'm3', 'm4', 'm5', 'mean')

	# Matrix with coefficients for rr
	coefs_rr <- data.frame((coef(glmRR1)))
	coefs_rr[,2] <- data.frame(coef(glmRR2))
	coefs_rr[,3] <- data.frame(coef(glmRR3))
	coefs_rr[,4] <- data.frame(coef(glmRR4))
	coefs_rr[,5] <- data.frame(coef(glmRR5))	
	coefs_rr[,6] <- apply(coefs_rr, 1, mean)	
	names(coefs_rr) <- c('m1', 'm2', 'm3', 'm4', 'm5', 'mean')

	df.rap$pd_rr <- 1 / (1 + exp(-(coefs_rr[1,6] + coefs_rr[2,6]*df.rap$RiskRating)))
	df.rap.inactive$pd_rr <-	predict(glm.rr, df.rap.inactive, type="response")
	df.rap$pd_rr <-	predict(glm.rr, df.rap, type="response")
	df.rap$pd <- predict(glm.mod.all, df.rap, type="prob")[,2]

# run file with upper/lower bounds
	source('# Upper and lower intervals and graphs.r')
	summary(df.rap$pd_lower)
	# TODO recode <0 to 0
	
# Save each glm
	glm_five <- list(glm1, glm2, glm3, glm4, glm5)
	glm_five_rr <- list(glmRR1, glmRR2, glmRR3, glmRR4, glmRR5)
	saveRDS(glm_five, 'glms_pd_model_5.27.16.rds')
	saveRDS(glm_five, 'glms_rr_model_5.27.16.rds')
	saveRDS(glm.mod.all, 'glm_model_05.27.16.rds')

	c <- coef(glm.mod.all$finalModel)
	c_rr <- coef(glm.rr)
	df.out <- dplyr::select(df.rap, LoanID, Account.Name, active, one_of(modelCols9), pd, RiskRating, pd_rr,
		pd_lower, pd_upper,
		Sales.Concentration.to.a.Single.Buyer, working_capital_to_sales,
		Working.Capital, Sales, Gross_Margin_range, Close.Date, Maturity.at.Origination, Max_Risk_Category,
		Risk_Category, RC.Opp.Number)

	write.csv(df.out, 'pds_05.28.16.csv')
	write.csv(c, 'pd_model_coefs.csv')
	saveRDS(glm_five, 'glms_pd_model_5.27.16.rds')
	saveRDS(glm_five, 'glms_rr_model_5.27.16.rds')
	saveRDS(glm.mod.all, 'glm_model_05.27.16.rds')

# 	require(MuMIn)
# 	glm_Final <- model.avg(glm_five)


	p <- ggplot(df.rap, aes(x=pd, y=RiskRating))
	p + geom_point() + geom_smooth()

	df.rap <- arrange(df.rap, desc(WO))
	p <- ggplot(df.rap, aes(x=pd, y=pd_rr, colour=WO))
	p + geom_point(size=0.5) + geom_abline(color='purple', size=1) + geom_smooth() 
	p + geom_point(aes(size=WO),alpha=.75) + geom_abline(color='purple', size=1) # aes(size=WO), 


	# reorder the factor levels
	df.rap$WO <- factor(df.rap$WO, levels = rev(levels(factor(df.rap$WO))))

	r <- ggplot(df.rap, aes(pd, Tenor_years )) + geom_jitter() # geom_smooth() + fac
	r + facet_grid(. ~ WO) + geom_smooth()
	r + facet_grid(WO ~ .)


anova(glm.mod.all,test="Chisq")
drop1(glm.mod.all,test="Chisq")
confint(glm.mod.all$finalModel, level = 0.75)
confint(glm.mod.all$finalModel, level = 0.33)

