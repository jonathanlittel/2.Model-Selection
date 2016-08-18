library(stargazer)
library(pROC)
library(pscl)
options(digits = 3)
source('Preprocess data file.r')
metrics <- data.frame(AUC = 0,
	AIC = 0)
rownames(metrics) <- 'temp'

	# 7/26/16: remove coffee and wc_sales_cat
	modelCols <- c('WO', 'tenor_years_min1',  'sales_concent_cat', 'Depth.of.Management',
		'gross_margin_cat', 'Financial.Flexibility', 'past_arrears', 'sales_growth_pct_rank',  
		'sales_growth_none', 'country_risk')

	# # on 6/22/16 (with coffee, wc_sales_cat) 'Financial.Strat.Quality',
	# modelCols <- c('WO', 'tenor_years_min1', 'sales_concent_cat', 'Depth.of.Management',
	# 	'gross_margin_cat',  'past_arrears', 'wc_sales_cat',  
	# 	'Financial.Strat.Quality', 'coffee', 'country_risk')	

# df.rap.inactive$wc_sales_cat <- factor(df.rap.inactive$wc_sales_cat)
df.rap.inactive <- df.rap.inactive %>%
mutate(FundedDebt = log(Current.portion.of.Root.Capital.long.term.debt +
				Current.portion.of.long.term.debt + Root.Capital.long.term.debt + 
				Long.term.debt ))
# moved to Preprocess file
# normalize <- function(x) {
# 	norm <- ( x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
# 	norm
# }
df.rap.inactive$margin_sd_sc <- normalize(df.rap.inactive$margin_sd)
df.rap.inactive$margin_sd_sc_sq <- df.rap.inactive$margin_sd_sc^2
# moved to Preprocess file

# df.rap.inactive$sales_prev_year <-  df.rap.inactive$Sales / ( df.rap.inactive$Sales_growth_t1 + 1)
# df.rap.inactive$sales_growth_nom <- log(
# 	normalize(
# 	 df.rap.inactive$Sales - df.rap.inactive$sales_prev_year
# 	) + 0.0000001) # to avoid 0 value for log

# df.rap.inactive$Sales_growth_t1_sq <- df.rap.inactive$Sales_growth_t1 ^ 2
# df.rap.inactive$sales_growth_pct_rank <- rank(df.rap.inactive$sales_growth_nom) / nrow(df.rap.inactive)
# df.rap.inactive$sales_growth_none <- df.rap.inactive$sales_growth_pct_rank ^ 2 
# df.rap.inactive$DSCR_bin <- cut(df.rap.inactive$DSCRadj, c(-Inf, -0.0001, 0.0001, 0.5, 1, Inf)) #ifelse(df.rap.inactive$DSCRadj<0.5, 1, 0)


# Model 1
	modelCols1 <- c('WO',  'sales_concent_cat', 'Depth.of.Management',
		'gross_margin_cat', 'Financial.Flexibility', 'past_arrears', 'sales_growth_pct_rank',   
		'sales_growth_none', 'country_risk')

	df.rap.inactive.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelCols1]
	glm1 <- glm(WO ~ ., data=df.rap.inactive.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.rap.inactive$WO,predict(glm1, df.rap.inactive, type="response"))
	met_row <- c(roc$auc, glm1$aic)
	metrics[1,] <- met_row
	missing <- setdiff(modelCols, modelCols1)
	metrics$ommitted <- missing

# Model 2
	modelCols2 <- c('WO', 'tenor_years_min1',  'Depth.of.Management',
		'gross_margin_cat', 'Financial.Flexibility', 'past_arrears', 'sales_growth_pct_rank',   
		'sales_growth_none', 'country_risk')

	df.rap.inactive.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelCols2]
	glm2 <- glm(WO ~ ., data=df.rap.inactive.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.rap.inactive$WO,predict(glm2, df.rap.inactive, type="response"))
	met_row <- c(roc$auc, glm2$aic)
	metrics <- rbind(metrics, met_row)
	missing <- setdiff(modelCols, modelCols2)
	metrics$ommitted[2] <- missing

# Model 3
	modelCols3 <- c('WO', 'tenor_years_min1',  'sales_concent_cat',
		'gross_margin_cat', 'Financial.Flexibility', 'past_arrears', 'sales_growth_pct_rank',   
		'sales_growth_none', 'country_risk')
	df.rap.inactive.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelCols3]
	glm3 <- glm(WO ~ ., data=df.rap.inactive.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.rap.inactive$WO,predict(glm3, df.rap.inactive, type="response"))
	met_row <- c(roc$auc, glm3$aic)
	metrics <- rbind(metrics, met_row)
	missing <- setdiff(modelCols, modelCols3)
	metrics$ommitted[3] <- missing
	
# Model 4
	modelCols4 <- c('WO', 'tenor_years_min1',  'sales_concent_cat', 'Depth.of.Management',
		'Financial.Flexibility', 'past_arrears', 'sales_growth_pct_rank',   
		'sales_growth_none', 'country_risk')

	df.rap.inactive.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelCols4]
	glm4 <- glm(WO ~ ., data=df.rap.inactive.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.rap.inactive$WO,predict(glm4, df.rap.inactive, type="response"))
	met_row <- c(roc$auc, glm4$aic)
	metrics <- rbind(metrics, met_row)
	missing <- setdiff(modelCols, modelCols4)
	metrics$ommitted[4] <- missing
	
# Model 5

	modelCols5 <- c('WO', 'tenor_years_min1',  'sales_concent_cat', 'Depth.of.Management',
		'gross_margin_cat', 'past_arrears', 'sales_growth_pct_rank',   
		'sales_growth_none', 'country_risk')
	df.rap.inactive.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelCols5]
	glm5 <- glm(WO ~ ., data=df.rap.inactive.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.rap.inactive$WO,predict(glm5, df.rap.inactive, type="response"))
	met_row <- c(roc$auc, glm5$aic)
	metrics <- rbind(metrics, met_row)
	missing <- setdiff(modelCols, modelCols5)
	metrics$ommitted[5] <- missing
	
# Model 6
	modelCols6 <- c('WO', 'tenor_years_min1',  'sales_concent_cat', 'Depth.of.Management',
		'gross_margin_cat', 'Financial.Flexibility', 'sales_growth_pct_rank',   
		'sales_growth_none', 'country_risk')
	df.rap.inactive.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelCols6]
	glm6 <- glm(WO ~ ., data=df.rap.inactive.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.rap.inactive$WO,predict(glm6, df.rap.inactive, type="response"))
	met_row <- c(roc$auc, glm6$aic)
	metrics <- rbind(metrics, met_row)
	missing <- setdiff(modelCols, modelCols6)
	metrics$ommitted[6] <- missing
	
# Model 7
	modelCols7 <- c('WO', 'tenor_years_min1',  'sales_concent_cat', 'Depth.of.Management',
		'gross_margin_cat', 'Financial.Flexibility', 'past_arrears', 
		'sales_growth_pct_rank', 'country_risk')
	df.rap.inactive.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelCols7]
	glm7 <- glm(WO ~ ., data=df.rap.inactive.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.rap.inactive$WO,predict(glm7, df.rap.inactive, type="response"))
	met_row <- c(roc$auc, glm7$aic)
	metrics <- rbind(metrics, met_row)
	missing <- setdiff(modelCols, modelCols7)
	metrics$ommitted[7] <- missing
	
# Model 8
	modelCols8 <- c('WO', 'tenor_years_min1',  'sales_concent_cat', 'Depth.of.Management',
		'gross_margin_cat', 'Financial.Flexibility', 'past_arrears', 'sales_growth_pct_rank',  
		'sales_growth_none'
		)
	df.rap.inactive.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelCols8]
	glm8 <- glm(WO ~ ., data=df.rap.inactive.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.rap.inactive$WO,predict(glm8, df.rap.inactive, type="response"))
	met_row <- c(roc$auc, glm8$aic)
	metrics <- rbind(metrics, met_row)
	missing <- setdiff(modelCols, modelCols8)
	metrics$ommitted[8] <- missing
	
# Model 9
	modelCols9 <- c('WO', 'tenor_years_min1',  'sales_concent_cat', 'Depth.of.Management',
		'gross_margin_cat', 'Financial.Flexibility', 'past_arrears',   
		'sales_growth_none', 'country_risk')
	df.rap.inactive.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelCols9]
	glm9 <- glm(WO ~ ., data=df.rap.inactive.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.rap.inactive$WO,predict(glm9, df.rap.inactive, type="response"))
	met_row <- c(roc$auc, glm9$aic)
	metrics <- rbind(metrics, met_row)
	missing <- setdiff(modelCols, modelCols9)
	metrics$ommitted[9] <- missing
	
# Model 10
	modelCols10 <-  c('WO', 'tenor_years_min1',  'sales_concent_cat', 'Depth.of.Management',
		'gross_margin_cat', 'Financial.Flexibility', 'past_arrears', 
		'country_risk')

	df.rap.inactive.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelCols10]
	glm10 <- glm(WO ~ ., data=df.rap.inactive.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.rap.inactive$WO,predict(glm10, df.rap.inactive, type="response"))
	met_row <- c(roc$auc, glm10$aic)
	metrics <- rbind(metrics, met_row)
	missing <- setdiff(modelCols, modelCols10)
	metrics$ommitted[10] <- missing
	
# Model 11
	modelCols11 <- c('WO', 'tenor_years_min1',  'sales_concent_cat', 'Depth.of.Management',
		'gross_margin_cat', 'Financial.Flexibility', 'past_arrears', 'sales_growth_pct_rank',   
		'sales_growth_none', 'country_risk')
	df.rap.inactive.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelCols11]
	glm11 <- glm(WO ~ ., data=df.rap.inactive.model, family="binomial", na.action=na.exclude)
	roc <- roc(df.rap.inactive$WO,predict(glm11, df.rap.inactive, type="response"))
	met_row <- c(roc$auc, glm11$aic)
	metrics <- rbind(metrics, met_row)
	missing <- setdiff(modelCols, modelCols11)
	metrics$ommitted[11] <- 'none'
	
# #### Train final model on both test and training
#   modelColsFinal <- c("WO", "Sales_log", "WorkingCapital_log",
#                       "past_arrears",
#                       'sales_growth_pct_rank', "Depth.of.Management",
#                       "Loan.Type")
#   # Train model on all inactive loans
#   df.rap.inactive.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelColsFinal] 
#   glmFinal <- glm(WO ~ ., data=df.rap.inactive.model, family="binomial", na.action=na.exclude)
# 	roc <- roc(df.rap.inactive$WO,predict(glmFinal, df.rap.inactive, type="response"))
# 	met_row <- c(roc$auc, glmFinal$aic)
# 	metrics <- rbind(metrics, met_row)


	row.names(metrics) <- 1:nrow(metrics)
	# metrics <- cbind(metrics, modelCols[-1])

rownames(metrics) <- c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7", "Model 8", 
	"Model 9", "Model 10", "Model 11" )
# Print html file
	compTable <- stargazer(glm1, glm2, glm3, glm4, glm5, glm6, glm7, glm8, glm9, glm10, glm11,
					t(metrics), type = "html",
                    ci = TRUE)
	# metricTable <- stargazer(metrics)
	write(compTable, file = "model_comparison_08.17.16.html", append = FALSE)

	# write(t(metrics), file = "model_comparison_06.17.16.html", append = TRUE)
	# names(modelCols) <- 0:(length(modelCols)-1)
	# write(modelCols[-1], file = "model_comparison_06.17.16.html", append = TRUE)

	# write(metricTable, file = "model_comparisons_05.10.16.html", append = TRUE)
	file.show("model_comparison_08.17.16.html")


