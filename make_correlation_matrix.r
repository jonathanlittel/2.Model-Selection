
wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/Loss Model/2.Model Selection"
setwd(wd)

## Make correlation plots
filename <- "rap_data.csv"
filename <-  "https://rootcapital.box.com/shared/static/d6q5d6pfnvzao08x3ev7af4rj6bm6hve.csv"
df.rap <- read.csv(filename, header=TRUE, sep=",")

# Select only the numeric columns
	df.rap$default <- as.numeric(df.rap$default)
	df.nummed <- sapply(df.rap, function(x) as.numeric(as.character(x)))  # change all columns to numeric
	# num <- sapply(df.rap, as.numeric)   # change all columns to numeric
	nums <- sapply(df.rap, is.numeric) # select only numeric columns
	df.nums <- df.nummed[,nums]
	# Produce correlation matrix 
	cor.rap <- cor(df.nums, method = "pearson") # Parameter  treat pairwise NAs as missing and exclude  # , use = "na.or.complete"
	write.csv(cor.rap, "rap_correlation_matrix5.csv")
	# symnum(cor.rap)

###############################################################################
# Below is a bunch of other ways of doing this prettier.					      #		
# GGally is probably the most standard							      #			
# The performance analytics one worked pretty well for large number of vars                                 #
###############################################################################


library(lattice)
splom(~df.num[,16:18])

library(ggplot2)
# plotmatrix(df.num[1:10]) # this is depracated

library(GGally)
ggpairs(df.nums, columns = 16:18, title = "",
 upper = list(), lower = list(continuous="smooth"), diag = list(),
 params = NULL, axisLabels = "internal",
 legends = FALSE, verbose = FALSE) 




# Create a short list of vars
boring_cols <- c("X", 
		"LoanID", 
		"Year", 
		"Period.End", 
		"Accounts.receivable...clients", 
		"Inventories", 
		"Other.current.assets", 
		"Total.current.assets", 
		"Total.PP.E", 
		"Depreciation..negative.", 
		"Notes.receivable", 
		"Prepaid.expenses", 
		"Investments.in.financial.instruments", 
		"Other.investments", 
		"Other.long.term.assets", 
		"Accounts.payable.members", 
		"Accounts.payable.suppliers.others", 
		"Root.Capital.short.term.debt", 
		"Current.portion.of.Root.Capital.long.term.debt", 
		"Other.short.term.liabilities", 
		"Root.Capital.long.term.debt", 
		"Other.long.term.liabilities", 
		"Total.long.term.liabilities", 
		"Initial.paid.in.capital", 
		"Additional.paid.in.capital", 
		"Donations", 
		"Reserves", 
		"Revaluation.adjustments", 
		"Retained.earnings.losses.beginning.balance", 
		"Profit.loss.of.the.year", 
		"Adjustments.to.retained.earnings", 
		"Retained.earnings.losses.ending.balance", 
		"TOTAL.LIABILITIES...EQUITY", 
		"Total.Income", 
		"Purchases.from.producers", 
		"Other.costs.of.goods.sold", 
		"Salaries", 
		"Transportation.expenses", 
		"Marketing.expenses", 
		"Other.Operating.Expense", 
		"Depreciation...amortization", 
		"Donations.1", 
		"Other.interest.income.Other.income", 
		"Total.Other.Income", 
		"Other.Expenses", 
		"Commissions...Fees", 
		"Other.expenses", 
		"Total.Other.Expenses", 
		"Taxes", 
		"Reserves.1", 
		"Net.Income", 
		"Operating.Profit.Margin.", 
		"Net.Profit.Margin.", 
		"Days.Accounts.Receivable...Clients", 
		"Accounts.Receivable.Turnover...Members.", 
		"Days.Accounts.Receivable...Members", 
		"Accounts.Payable.Turnover...Members", 
		"Accounts.Payable.Turnover...Suppliers...Others", 
		"Days.Accounts.Payable...Members", 
		"Days.Accounts.Payable...Suppliers...Others", 
		"Liquidity.Ratios", 
		"Quick.Ratio.", 
		"Adjusted.EBITDA", 
		"Total.Debt.Service.", 
		"Scheduled.Principal.Payments..Prior.Year.CPLTD.", 
		"Total.Debt.Service", 
		"Debt.Service.Coverage.", 
		"Interest.Coverage.", 
		"EBITDA.Total.Debt.Service", 
		"X...Adjusted.EBITDA.Total.Debt.Service", 
		"Cash.Conversion.Cycle..Risk.Rating.Score.", 
		"Total.Asset.Turnover..Risk.Rating.Score.", 
		"Net.Allowance.for.Loan.Loss.Expense.Total.Loan.Level", 
		"RC.Opp.Number", 
		"RC.Account.Number", 
		"Loan.Tenor", 
		"Disbursement.Fee....", 
		"Amount", 
		"PeakArrears", 
		"June2016Balance", 
		"active"
		) 

df.short <- df.nums[,!names(df.rap) %in% boring_cols]
df.short$default <- as.numeric(df.rap$default)


cor_list <- c("default","WriteoffsDummy", )
s <- ggpairs(df.short, columns = 1:10, title = "",
 upper = list(), lower = list(continuous="smooth"), diag = list(),
 params = NULL, axisLabels = "internal",
 legends = FALSE, verbose = FALSE) 

# Save the plots
pdf('scatterplot_matrices.pdf')
print(s)
dev.off()


shortish_list <- c(1:3,6,7,9,11:18,20,22)
short_list <- c(1,6,11:13,18,20,22, 45, 46, 54)
pairs.chrt <- ggpairs(df.short, columns=c(1:3,6,7,9,11:18,20,22), # groupColumn = default,
                      lower = list(continuous = "smooth", size=I(0.2)),
                      diag = list(size=1), # size?
                      upper = list(), 
                      axisLabels = "internal") +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(angle = 0, vjust = 1, color = "black"),
        panel.border = element_rect(fill = NA))


# Write to pdf
pdf('scatterplot_matrices2.pdf')
pairs.chrt
dev.off()


# This one is nice, works
library(PerformanceAnalytics)
c <- chart.Correlation(df.short[,short_list], histogram=TRUE, pch=19)


#######
# Using GGally
# This seems like best library


library(GGally)

library(GGally)
df.rap$default_dummy <- as.numeric(df.rap$default)
ggpairs(df.rap[,c("default_dummy", "WriteoffsDummy", "TOTAL.ASSETS","COGS.Margin", "max_pct_per_buyer", "Total.current.assets")])


cor.test(faithful[,1],faithful[,2])