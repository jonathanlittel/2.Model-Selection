# The purpose of this file is to do the data tweaking, imputing, and variable creation for 
# the default logit model

	require(dplyr)
	require(caret)
	require(scales)
#-------------------------------------------
# LOAD DATA
#-----------------------
# Load info for writeoffs and inactive since 12/31/15
# Data is updated through about 4/30
	data_wd <- paste("C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP",
	" Modules/Risk Profile/PD Model/1.Dataset Cleaning and Prep/data", sep = "")
	setwd(data_wd)
	filename <- "List of Write-offs.csv"
	wo <- read.csv(filename, skip=1)
	colnames(wo) <- c('LoanID', 'Actual_WOs', 'Manual_WOs','Tag' )

# load data for general working capital loan typeof
	filename <- 'Loan_Characteristics.csv'
	gwc <- read.csv(filename)
	gwc <- dplyr::select(gwc, LoanID = Loan.ID, ltgwc = LT.GWC_dd)

# load data for country_risk
	data_wd <- paste("C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP",
	" Modules/Risk Profile/PD Model/",
		"2.Model Selection/source", sep="")
	setwd(data_wd)
	filename <- "country_risk.csv"
	cr_data <- read.csv(filename)
	# change Swaziland to 9
	cr_data$country_risk[cr_data$Country=="Swaziland"] <- 9

# Load rap dataset and merge		
	wd <- paste("C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP",
	" Modules/Risk Profile/PD Model/3.Outputs", sep = "")
	setwd(wd)
	filename <-  "rap_data_Q4_15_05.24.16.csv"
	df.fin <- read.csv(filename, header=TRUE, sep=",")
	df2 <- merge(df.fin, cr_data, by='Country')
	df3 <- merge(df2, gwc, by = 'LoanID')
	df.rap <- df3

  df.rap$Close.Date <- as.Date(df.rap$Close.Date, "%Y-%m-%d")
#-------------------------------------------
# CHECKS
#-----------------------

# Check that all have scores and merge in country risk scores
	co_w_score <- levels(cr_data$Country)  # El Salvador, United Kingdom, United States
	co_in_data <- levels(df.rap$Country)
	setdiff(co_w_score, co_in_data)
	setdiff(co_in_data, co_w_score)	
	# if( length ( setdiff(co_w_score, co_in_data) ) > 0 ) stop("There's a score without a country")
	if( length ( setdiff(co_in_data, co_w_score) ) > 0 ) stop("There's a country without a score")

#-------------------------------------------
# CREATE VARIABLES
#-----------------------

# Sales_growth_t1 is the YoY growth of the most recent year over the 2nd most recent,
# t2 is the second most recent over the third most recent

	df.rap <- df.rap %>%  
	 	arrange(LoanID, Year) %>%
	 	group_by(LoanID)  %>%
	 	mutate(Sales_growth_t1 = Sales / lag(Sales,1, default=NA)- 1) %>%
	 	mutate(Sales_growth_t2 = lag(Sales,1, default=NA) / lag(Sales,2, default=NA) - 1) %>%
	 	mutate(Sales_growth_t3 = lag(Sales,2, default=NA) / lag(Sales,3, default=NA)- 1) %>%
	 	mutate(Revenue_growth=Total.Income / lag(Total.Income, 1, default=NA) - 1) %>%
	 	mutate(Revenue_growth = replace(Revenue_growth, is.infinite(Revenue_growth), NA)) %>%
	 	mutate(Sales_growth_t1 = replace(Sales_growth_t1, is.infinite(Sales_growth_t1), NA)) %>%
	 	mutate(Sales_growth_t2 = replace(Sales_growth_t2, is.infinite(Sales_growth_t2), NA)) %>%
	 	mutate(Sales_growth_t3 = replace(Sales_growth_t3, is.infinite(Sales_growth_t3), NA)) 

	df.rap$Revenue_growth[df.rap$Revenue_growth>10] <- 10	
	df.rap$Rev_growth_sq <- df.rap$Revenue_growth^2

# Years of Sales and CAGR
	df.rap <- df.rap %>%  
	 	arrange(LoanID, Year) %>%
	 	group_by(LoanID)  %>%
	 	mutate(Years_of_Sales = sum(Sales>5000),
	 		years_of_gross_profit = sum(Gross.Profit>0)) %>%
	 	mutate(Sales_CAGR = ( Sales[Year==max(Year)] / Sales[Year==min(Year)] ) ^ (1 / (max(Year) - min(Year))) - 1 ) %>%
	 	mutate(Sales_CAGR = replace(Sales_CAGR, is.infinite(Sales_CAGR), NA)) 	 	
	 	# mutate(min_year = min(Year)) %>%
	 	# mutate(Sales_cagr = )

df.rap$min_year <-NULL

# Sales_growth_avg is the average of the three YoY sales numbers, omitting NAs
	df.rap$Sales_growth_avg <- rowMeans(df.rap[,c('Sales_growth_t1','Sales_growth_t2','Sales_growth_t3')],
		na.rm = TRUE)

# use this if you need to spot check
	# head(select(df.rap, LoanID, Year, Sales, Sales_growth_t1,Sales_growth_t2, Sales_growth_t3,
	# 	Sales_growth_avg ))

	df.rap$gross_profit_margin <- df.rap$Gross.Profit / df.rap$Total.Income
	df.rap$gross_profit_margin[is.infinite(df.rap$gross_profit_margin)] <- NA


# margin range and negative margin
	df.rap <- df.rap %>%  
	 	arrange(LoanID, Year) %>%	
	 	group_by(LoanID)  %>%
	 	mutate(Sales_Growth = Sales / lag(Sales,1, default=NA)- 1) %>%
	 	mutate(Sales_Growth = replace(Sales_Growth, is.infinite(Sales_Growth), NA)) %>%
	 	mutate(Sales_Growth_sd = sd(Sales_Growth, na.rm=TRUE)) %>%
	 	mutate(Sales_Growth_sd = replace(Sales_Growth_sd, is.infinite(Sales_Growth_sd), NA)) %>%
	 	mutate(Sales_Growth_rate_range = max(Sales_Growth) - min(Sales_Growth)) %>%
	 	mutate(neg_margins = sum(gross_profit_margin<0), # count of years of neg margins
	 			high_margins = sum(gross_profit_margin>1), # count of years of margins above 100%
	 			years_of_margins = sum(gross_profit_margin>=0)) %>%	 	
	 	mutate(Gross_Margin_range = max(gross_profit_margin) - min(gross_profit_margin))

	 	# check
	 	df.rap$filter <- factor('not classified',
	 		levels = c('not classified', 'really na', 'neg margins',
	 			'margins above 1', 'less than 2 years'))
	 	df.rap$filter <- 'not classified'
		df.rap$filter[is.na(df.rap$Gross_Margin_range)] <- 'really na'	
		df.rap$filter <- ifelse(df.rap$neg_margins>0, 'neg margins', df.rap$filter)
		df.rap$filter <- ifelse(df.rap$high_margins>0, 'margins above 1', df.rap$filter)
		df.rap$filter <- ifelse(df.rap$years_of_gross_profit<2, 'less than 2 years', df.rap$filter)
		
 # prop.table(table(
 # 	df.rap.inactive$WO[!df.rap.inactive$filter==c(
 # 		'not classified', 'really na') & df.rap.inactive$gross_margin_cat==21]))

 # prop.table(table(
 # 	df.rap.inactive$WO[!df.rap.inactive$filter==c(
 # 		'less than 2 years') & df.rap.inactive$gross_margin_cat==21]))


 # prop.table(table(
 # 	df.rap.inactive$WO[df.rap.inactive$gross_margin_cat==21 & df.rap.inactive$filter=='less than 2 years']
 # 	))
 # df.rap %>% 
 # filter(active==0) %>%
 # group_by(filter, gross_margin_cat) %>% summarise(meanWO = mean(as.numeric(WO)-1))

# FundedDebtToEBITDA
df.rap<- df.rap %>% 
			mutate(FundedDebtToEBITDA = (Current.portion.of.Root.Capital.long.term.debt +
				Current.portion.of.long.term.debt + Root.Capital.long.term.debt + 
				Long.term.debt ) / EBITDA ) %>%
			mutate(FundedDebtToEBITDA = replace(FundedDebtToEBITDA, is.nan(FundedDebtToEBITDA), NA))

# FundedDebtToSales
df.rap<- df.rap %>% 
			mutate(FundedDebtToSales = (Current.portion.of.Root.Capital.long.term.debt +
				Current.portion.of.long.term.debt + Root.Capital.long.term.debt + 
				Long.term.debt ) / Sales ) %>%
			mutate(FundedDebtToSales = replace(FundedDebtToSales, is.infinite(FundedDebtToSales), NA))

# Sales to Equity
	 	df.rap$sales_to_equity <- df.rap$Sales / df.rap$TOTAL.EQUITY
	 	df.rap$working_capital_to_sales <- df.rap$Working.Capital / df.rap$Sales
		df.rap$working_capital_to_sales[is.infinite(df.rap$working_capital_to_sales)] <- NA
		df.rap$working_capital_to_sales_sq <- df.rap$working_capital_to_sales^2

# Create new calculated metrics
	df.rap <- df.rap %>%
		mutate(DtoE_adding_RC_Loan = (TOTAL.LIABILITIES+Amount)/TOTAL.EQUITY ) %>%
		mutate(DtoE_adding_RC_Loan = replace(DtoE_adding_RC_Loan, is.infinite(DtoE_adding_RC_Loan), NA)) %>%		
		mutate(DtoE = TOTAL.LIABILITIES/TOTAL.EQUITY ) %>%
		mutate(DSCRadj = EBITDA/(Interest.Expense+Current.portion.of.long.term.debt+Current.portion.of.Root.Capital.long.term.debt)) %>%
		mutate(DSCRadj = replace(DSCRadj, is.infinite(DSCRadj), NA)) %>%
		mutate(DSCRadj_adj = Adjusted.EBITDA/(Interest.Expense+Current.portion.of.long.term.debt+Current.portion.of.Root.Capital.long.term.debt)) %>%
		mutate(DSCRadj_adj = replace(DSCRadj_adj, is.infinite(DSCRadj_adj), NA)) %>%
		mutate(LoanAmountOverSales = Amount/Sales) %>%
		mutate(LoanAmountOverSales = replace(LoanAmountOverSales, is.infinite(LoanAmountOverSales), NA)) 

# count number of records per RC.Account.Number
# account_count <- df.rap %>%  
#     group_by(RC.Account.Number, Year) %>%
#     row_number()==1 %>%
#     summarize(n=n()) 

# count number of records per RC.Account.Number
# note that this won't be effective at the time of underwriting
sales_years <- df.rap %>%  
    group_by(RC.Account.Number) %>%
    arrange(LoanID) %>%
    filter(!duplicated(Year)) %>%
    # select(RC.Account.Number, Year, Sales) %>%
    mutate(Years_Sales_by_account=max(Year)-min(Year) + 1 ) # count of years of sales, by account

df.rap$Tenor_years <- df.rap$Tenor_days / 365

table(sales_years$Years_Sales_by_account)

# Create growth variables
	 df.rap <- df.rap %>%  
	 	arrange(LoanID, Year) %>%
	 	group_by(LoanID)  %>%
	 	# mutate(Sales_growth=lag(Sales,1, default=NA)) %>%
	 	mutate(Revenue_growth=Total.Income / lag(Total.Income, 1, default=NA) - 1 ) %>%
	 	mutate(Revenue_growth = replace(Revenue_growth, is.infinite(Revenue_growth), NA))

	 df.rap$Revenue_growth[df.rap$Revenue_growth>10] <- 10	
	 df.rap$Rev_growth_sq <- df.rap$Revenue_growth^2

# nominal sales growth
	df.rap <- df.rap %>%  
	 	arrange(LoanID, Year) %>%
	 	group_by(LoanID)      %>%
	 	mutate(sales_growth_nom = Sales - lag(Sales,1, default=NA),
	 		   sales_growth_pct = (Sales / lag(Sales, 1, default = NA) ) - 1 )  %>%
	 	mutate(sales_growth_nom = replace(sales_growth_nom, is.infinite(sales_growth_nom), NA),
	 		   sales_growth_pct = replace(sales_growth_pct, is.infinite(sales_growth_pct), NA))

# Merge in new WO	 		
	df.rap <- subset(df.rap, last_year==1)
	wd2 <- paste("C:/Box Sync/Risk Appetite - Provisioning Project/",
		"Working Folders for RAP Modules/Risk Profile/PD Model/2.Model Selection/source",
		sep = "")
	setwd(wd2)
	df.rap <- merge(df.rap, wo, by="LoanID")
	# df.rap$interest_coverage <- as.numeric(df.rap$Interest.Coverage.)
	df.rap$interest_coverage <- df.rap$EBITDA / df.rap$Interest.Expense
	df.rap$interest_coverage[is.infinite(df.rap$interest_coverage)] <- NA

# Merge in risk rating with decimals

	rr <- read.csv('rr.csv')
	df.rap <- merge(df.rap, y=rr, all.x=TRUE, all.y=FALSE)
	# df.train <- merge(df.train, rr, all.y=FALSE)
	# df.test <- merge(df.test, rr, all.y=FALSE)
	# df.rap.inactive <- merge(df.rap.inactive, rr, all.y=FALSE)


# Create categorical with values at midpoints of Sales.Concentration.to.a.Single.Buyer categories in rr tool
# including replacing sales_concent_cat with median if NA
	df.rap <- df.rap %>%
				mutate(sales_concent_cat = NA) %>%
				mutate(sales_concent_cat = replace(sales_concent_cat, Sales.Concentration.to.a.Single.Buyer==4, 0.05)) %>%
				mutate(sales_concent_cat = replace(sales_concent_cat, Sales.Concentration.to.a.Single.Buyer==5, 0.20)) %>%
				mutate(sales_concent_cat = replace(sales_concent_cat, Sales.Concentration.to.a.Single.Buyer==6, 0.45)) %>%
				mutate(sales_concent_cat = replace(sales_concent_cat, Sales.Concentration.to.a.Single.Buyer==7, 0.70)) %>%
				mutate(sales_concent_cat = replace(sales_concent_cat, Sales.Concentration.to.a.Single.Buyer==8, 0.85)) %>%
				mutate(sales_concent_cat = replace(sales_concent_cat, Sales.Concentration.to.a.Single.Buyer==9, 0.95)) %>%
				mutate(sales_concent_cat = replace(sales_concent_cat, Sales.Concentration.to.a.Single.Buyer==10, 1.00)) %>%
				mutate(sales_concent_cat = replace(sales_concent_cat, is.na(Sales.Concentration.to.a.Single.Buyer), median(sales_concent_cat, na.rm=TRUE)))

	data.frame(table(df.rap$sales_concent_cat))			

	df.rap <- df.rap %>%
		mutate(wc_sales_cat = ifelse(working_capital_to_sales>=0 & working_capital_to_sales <= 0.05, 8,
			ifelse(working_capital_to_sales>0.05 & working_capital_to_sales <= 0.40, 6, NA))) %>%
		mutate(wc_sales_cat = replace(wc_sales_cat, is.na(working_capital_to_sales), 44)) %>%
		mutate(wc_sales_cat = replace(wc_sales_cat, working_capital_to_sales<0, 30)) %>%
		mutate(wc_sales_cat = replace(wc_sales_cat, working_capital_to_sales>0.4, 12))

# Create dummy for coffee
		df.rap$coffee <- as.factor(df.rap$Sector.and.Perishability=='Coffee')

# Create categorical for if funded debt is within a good range
# group of different negative effects into the 1 bucket (no debt, negative ebitda, high debt)
	df.rap <- df.rap %>%
		mutate(fundedDebttoEBITDA_cat = ifelse(FundedDebtToEBITDA > 0 & FundedDebtToEBITDA <= 5, 0,
			ifelse(FundedDebtToEBITDA > 5 | FundedDebtToEBITDA <= 0, 1, 1 ))) %>%
		mutate(fundedDebttoEBITDA_cat = as.factor(fundedDebttoEBITDA_cat))

# rank all loans by sales growth in nominal terms in previous year, normalize
	normalize <- function(x) {
		norm <- ( x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
		norm
	}

# create dummy for missing/low sales in prev year
  df.rap$sales_growth_none   <- 0
  df.rap$sales_growth_none[is.na(df.rap$sales_growth_pct)] <- 1
  df.rap$sales_growth_none[df.rap$sales_growth_pct > 100] <- 1
  
  # df.rap$sales_growth_pct[df.rap$sales_growth_none == 1] <- median(df.rap$sales_growth_pct, na.rm = TRUE)

	df.rap$sales_growth_rank <- rank(
			# df.rap$Sales - df.rap$sales_prev_year
			df.rap$sales_growth_nom, na.last = FALSE # FALSE means NAs are put first, to keep na: 'keep'
					) / nrow(df.rap) * 100

	df.rap$sales_growth_pct_rank <- rank(
			# df.rap$Sales - df.rap$sales_prev_year
			df.rap$sales_growth_pct, na.last = FALSE # FALSE means NAs are put first, to keep na: 'keep'
					) / nrow(df.rap) * 100

# replace with median
  # df.rap$sales_growth_rank[is.na(df.rap$sales_growth_pct)] <- median(df.rap$sales_growth_rank)
  # df.rap$sales_growth_pct_rank[is.na(df.rap$sales_growth_pct)] <- median(df.rap$sales_growth_pct_rank)
  
df.rap %>% arrange(sales_growth_rank) %>% select(Account.Name, Sales, sales_growth_nom, sales_growth_rank, sales_growth_pct) %>% tail()
  # df.rap$sales_growth_rank <- rank(df.rap$sales_growth_pct) / nrow(df.rap)

  df.rap$sales_growth_rank_sq <- df.rap$sales_growth_rank ^ 2
  df.rap$sales_growth_pct_rank_sq <- df.rap$sales_growth_pct_rank ^ 2


#---------------------------------------------------------------
#     Remove outliers and create categorical vars   
#---------------------------

	# create categorical for gross margin range, where it gets the highest (worst)
		# score when either it's missing margin range (ie <2 years sales history) or
		# it had negative margins in any year (6 of 29 WOs ~21%)
	df.rap$gross_margin_cat <- 999
	df.rap$gross_margin_cat <- ifelse(df.rap$neg_margins>0, 13, df.rap$gross_margin_cat)
	df.rap$gross_margin_cat <- ifelse(df.rap$years_of_gross_profit<2, 21, df.rap$gross_margin_cat)	
	# df.rap$gross_margin_cat[df.rap$neg_margs>0] <- 21	
	df.rap$gross_margin_cat[df.rap$Gross_Margin_range==0] <- 13	
	df.rap$gross_margin_cat <- ifelse(df.rap$Gross_Margin_range<0.15 & df.rap$Gross_Margin_range>0, 6, df.rap$gross_margin_cat)
	df.rap$gross_margin_cat <- ifelse(df.rap$Gross_Margin_range>=0.15 & df.rap$Gross_Margin_range<=1, 12, df.rap$gross_margin_cat)
	df.rap$gross_margin_cat <- ifelse(df.rap$Gross_Margin_range>1, 13, df.rap$gross_margin_cat)
	df.rap$gross_margin_cat <- ifelse(df.rap$high_margins>0, 13, df.rap$gross_margin_cat)  # change 
	df.rap$gross_margin_cat[is.na(df.rap$Gross_Margin_range)] <- 21


	# function to remove outliers
	remove_outliers <- function(x, na.rm = TRUE, ...) {
	  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
	  H <- 1.5 * IQR(x, na.rm = na.rm)
	  y <- x
	  y[x < (qnt[1] - H)] <- NA
	  y[x > (qnt[2] + H)] <- NA
	  y
	}

	df.rap$DSCRadj2 <- remove_outliers(df.rap$DSCRadj)

	# Replace with median
	replace_median <- function(x, na.rm = TRUE, ...) {
	  if(class(x) != 'numeric') stop('item must be numeric')
	  med <- median(x, na.rm=TRUE)
	  y <- x
	  y[is.na(x)] <- med
	  y
	}

	# impute missing values with median for columns in list
	med_recodes <- 	modelCols <- c( 'Sales_log', 'sales_concent_cat', 'Depth.of.Management',
		'gross_margin_cat', 'Financial.Flexibility', 'past_arrears', 'wc_sales_cat',  
		'Financial.Strat.Quality')
	sapply(df.rap[,names(df.rap) %in% med_recodes], class)
	for (i in med_recodes) {
		print(i)
		temp <- as.numeric(unlist(df.rap[,i]))
		df.rap[,i] <- replace_median(temp)
	}

# Create a new variable for 'active' if a loan has been repaid or marked as a writeoff
	df.rap <- mutate(df.rap,
		active = ifelse(Tag==3, 1, 0),
		WO = ifelse(Tag==1, 1, 0) )
	df.rap$WO <- factor(df.rap$WO,
		labels = c('Non_Writeoff', 'Writeoff'))

# Create years of sales by client id, and merge that back in to loan level
	yS <- df.rap %>%
		group_by(RC.Account.Number) %>%
		summarise(years_of_sales_account = n())
	df.rap <- merge(df.rap, yS, by='RC.Account.Number')

# Create tenor with min of 1 year
	df.rap$tenor_years_min1 <- ifelse(df.rap$Tenor_years<=1, 1, df.rap$Tenor_years)
	df.rap$tenor_years_min1 <- ifelse(df.rap$ltgwc==1, 1, df.rap$tenor_years_min1)

df.rap$neg_working_cap <- ifelse(df.rap$Working.Capital<0, 1, 0)

#------------------------
# add currency volatility
root_wd <- paste("C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP",
	" Modules/Risk Profile/PD Model/",
		"2.Model Selection/", sep="")
	setwd(root_wd)
# source('currency volatility.r')

# Create data frame on loans that are no longer active (or written off)
	df.rap.inactive <- filter(df.rap, active==0)

#-----------------------------------------------------------
# Split data into training and test set             
#--------------------------------
	set.seed(9)
	# uniqueIDs <- unique(df.rap.inactive$LoanID)	 # old method - caret attempts to keep WO balanced
	# sample_size <- floor(0.80 * length(uniqueIDs))
	# LoanIDtrain <- sample(uniqueIDs, sample_size)   
	trainIndex <- createDataPartition(as.factor(df.rap.inactive$WO),
		times = 1,
		p = 0.80,
		list = FALSE)
	# df.train <- df.rap.inactive[df.rap.inactive$LoanID %in% LoanIDtrain,]
	# df.test <- df.rap.inactive[!df.rap.inactive$LoanID %in% LoanIDtrain,] 
	df.train <- df.rap.inactive[trainIndex,]
	df.test <- df.rap.inactive[-trainIndex,]

# end 