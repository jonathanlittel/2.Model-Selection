library(caret)
library(glmnet) # for lasso
library(dplyr)
options (scipen = 99, ddigits = 3)

# Load rap data
	wd <- paste("C:/Box Sync/Risk Appetite - Provisioning Project/",
	            "Working Folders for RAP Modules/Risk Profile/PD Model",
	            "/3.Outputs",
	            sep="")
	setwd(wd)
	filename <- 'rap_data_Q4_15_3.26.16.csv'
	df.rap <- read.csv(filename, header=TRUE, sep=",")
	filename <- 'underwriting_pds_Q4_2015.csv'
	pds <- read.csv(filename, header=TRUE, sep=",")
	pds <- select(pds, LoanID, pd)
	wd <- paste("C:/Box Sync/Risk Appetite - Provisioning Project/",
	            "Working Folders for RAP Modules/Risk Profile/PD Model",
	            "/2.Model Selection/1.Risk Rating Components",
	            sep="")
	setwd(wd) 
## Risk Rating score and map
	# 1	Scale and Diversification                                    
	# 	a. Total Sales (USD Million)						
	# 	b. Sales Concentration to a Single Buyer			
	# 	c. Exports vs Local Sales 
	# 	d. Projected vs. Actual Volume Sold
	# 	e. Segmental Diversification
			
	# 2	Franchise Strength and Growth Potential
	# 	a. Longevity & Quality of Relationship with Key Buyer(s)
	# 	b. Depth of Management 	
	# 	c. Product Recognition

	# 3	Financial Flexibility
	# 	a. Financial Flexibility to Access Additional Capital

	# 4	Financial Strategy
	# 	a. Financial Strategy & Quality of Financial Information
	# 	b. Funded Debt / EBITDA
		
	# 	c. Cash Conversion Cycle
	# 	d. EBITDA / Interest Expense
	# 	e. Total Asset Turnover
		
## Risk Rating map to data
	# 1	Scale and Diversification                                    
	# 	a. Sales										 			Sales # Sales_log
	# 	b. Sales Concentration to a Single Buyer         			max_pct_per_buyer # Sales.Concentration.to.a.Single.Buyer
	# 	c. Exports vs Local Sales 									Exports.vs..Local.Sales
	# 	d. Projected vs. Actual Volume Sold							Projected.vs..Actual.Volume.Sold
	# 	e. Segmental Diversification				 				multiple_segments  # Segmental.Diversification
			
	# 2	Franchise Strength and Growth Potential
	# 	a. Longevity & Quality of Relationship with Key Buyer(s)    Longevity...Quality.of.Relationship.with.Key.Buyer.s.
	# 	b. Depth of Management 										Depth.of.Management
	# 	c. Product Recognition										Product.Recognition

	# 3	Financial Flexibility
	# 	a. Financial Flexibility to Access Additional Capital		Financial.Flexibility

	# 4	Financial Strategy
	# 	a. Financial Strategy & Quality of Financial Information    Financial.Strat.Quality
	# 	b. Funded Debt / EBITDA										Funded.Debt...EBITDA..Risk.Rating.Score. 
	#												        /// or  ( Current.portion.of.Root.Capital.long.term.debt + Current.portion.of.long.term.debt + 
	#																Root.Capital.long.term.debt + Long.term.debt ) / EBITDA
		
	# 	c. Cash Conversion Cycle									Cash.Conversion.Cycle. or Max_Cash_Conversion_Cycle or Cash.Conversion.Cycle..Risk.Rating.Score.
	# 	d. EBITDA / Interest Expense								EBITDA.Total.Debt.Service # EBITDA...Interest.Expense..Risk.Rating.Score.
	# 	e. Total Asset Turnover										Total.Asset.Turnover
	

df.rap$Funded_debt_to_EBITDA <- ( df.rap$Current.portion.of.Root.Capital.long.term.debt + df.rap$Current.portion.of.long.term.debt +
									df.rap$Root.Capital.long.term.debt + df.rap$Long.term.debt ) / df.rap$EBITDA
sum(is.infinite(df.rap$Funded_debt_to_EBITDA))
df.rap$Funded_debt_to_EBITDA[is.infinite(df.rap$Funded_debt_to_EBITDA),] <- NA

# list of items in risk rating
reg_list <- c('Sales_log', 'max_pct_per_buyer', 'multiple_segments', 'Depth.of.Management', 'Product.Recognition',
				'Financial.Flexibility', 'Financial.Strat.Quality', 'Funded_debt_to_EBITDA', 'Cash.Conversion.Cycle.',
				'EBITDA.Total.Debt.Service', 'Total.Asset.Turnover')

reg_cols <- select(df.rap.inactive, reg_list)




