################################################
# Run progressive buildup of pd logit model #
#############################################

## Load file and setup r
	library(pROC)
	library(pscl)
	library(stargazer)
	library(caret)

	options(scipen=99, digits=3)
	wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/PD Model/1.Dataset Cleaning and Prep"
	setwd(wd)
	filename <-  "rap_data.csv"
	df.rap <- read.csv(filename, header=TRUE, sep=",")

	wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/PD Model/2.Model Selection"
	setwd(wd)

	df.rap <- subset(df.rap, last_year==1)

##################################
# Split data into training and test set         #
# and active / inactive set                          # 
##################################

	uniqueIDs <- unique(df.rap$LoanID)
	sample_size <- floor(0.80 * length(uniqueIDs))
	set.seed(10)
	LoanIDtrain <- sample(uniqueIDs, sample_size)
	df.rap.active <- df.rap[which(df.rap$active==1 & df.rap$balance_1215>0 & df.rap$last_year==1),]
	df.rap.inactive <- df.rap[which(df.rap$active==0 & df.rap$last_year==1),]











reg_cols <- c("WriteoffsDummy", "Sales_log", "WorkingCapital_log",
                "past_arrears",
                "margin_sd","Depth.of.Management", "past_arrears",
                "Loan.Type", "pd", "LoanID", "balance_1215",
                "Sales", "Working.Capital") 








library(dplyr)
df.rap %>% 
  select(reg_cols)
  # filter(from != "Hillary Clinton" & to != "Hillary Clinton") %>% 
  datatable()

