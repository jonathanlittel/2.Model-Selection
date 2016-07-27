require(caret)
# Create folds
	folds <- createFolds(df.test$WO)
	str(folds)
	split_up <- lapply(folds, function(ind, dat) dat[ind,], dat = df.test)
   	dim(df.test)
   	unlist(lapply(split_up, nrow))



# Create folds
	folds <- createFolds(df.rap.inactive$WO)
	str(folds)
	split_up <- lapply(folds, function(ind, dat) dat[ind,], dat = df.rap.inactive)
   	dim(df.rap.inactive)
   	unlist(lapply(split_up, nrow))


# Cut columns
   	df.train$WO <- as.factor(df.train$WO)
   	   	df.train$WO <- as.factor(df.train$WO)
# train model with caret
		m <- c('WO', 'Tenor_years',  'sales_concent_cat', 'Depth.of.Management', # Sales_log
		'gross_margin_cat', 'Financial.Flexibility', 'past_arrears', 'wc_sales_cat',  
		'Financial.Strat.Quality')
   		df.train.model <- df.train[,names(df.train) %in% m]
   		df.train.model <- df.train.model[complete.cases(df.train.model),]

   	# tc <- trainControl("cv",3,savePred=TRUE, classProbs=TRUE)
   	# glm.mod <- train(WO ~ .	,
   	# 	data = df.train.model,
   	# 	method='glm',
   	# 	trControl=tc, # =folds
   	# 	metric='ROC',
   	# 	family='binomial') # poisson(link = "log")
   		set.seed(9)
   	tc <- trainControl("cv",5,savePred=TRUE)
   	glm.mod <- train(WO ~ .	,
   		data = df.train.model,
   		method='glm',
   		trControl=tc,
   		metric = "Kappa",
   		family='binomial') 

head(glm.mod$pred)
print(glm.mod)
exp(glm.mod$finalModel$coefficients)

	pd <- predict(glm.mod, df.test, type="prob")[,2]
	roc <- roc(df.test$WO, pd)
	df.test$predWO_cut <- as.factor(ifelse(pd>cutoff,1,0))
	confusionMatrix(df.test$predWO_cut, df.test$WO)

 #   	require(ICEbox)
 #   	icey <- ice(glm1, X=df.train.model, y='Sales_log', predictor="WO", logodds=TRUE)
 #   	df.train.model$WO <- as.numeric(df.train.model$WO)
	# icey <- ice(glm.mod, X=df.train.model, y='Sales_log', predictor=as.numeric(df.train.model$WO), logodds=TRUE)


   	plot.ice()



# Cross validation on full dataset
   		m <- c('WO', 'Tenor_years', 'sales_concent_cat', 'Depth.of.Management',
		'gross_margin_cat', 'Financial.Flexibility', 'past_arrears', 'wc_sales_cat',  
		'Financial.Strat.Quality')

   		df.train.model <- df.rap[,names(df.rap) %in% m]
   		df.train.model <- df.train.model[complete.cases(df.train.model),]

   	# tc <- trainControl("cv",3,savePred=TRUE, classProbs=TRUE)
   	# glm.mod <- train(WO ~ .	,
   	# 	data = df.train.model,
   	# 	method='glm',
   	# 	trControl=tc, # =folds
   	# 	metric='ROC',
   	# 	family='binomial') # poisson(link = "log")
   		set.seed(9)
   	tc <- trainControl("cv",5,savePred=TRUE)
   	glm.mod <- train(WO ~ .	,
   		data = df.train.model,
   		method='glm',
   		trControl=tc,
   		family='binomial') 
