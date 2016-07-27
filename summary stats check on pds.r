# Summary stats on pds

library(dplyr)

df.rap.inactive$writeoff <- as.numeric(df.rap.inactive$WO) - 1
df.rap.inactive$pd_lasso <- pd_lasso
df.rap.inactive %>%
	group_by(Year) %>%	
	dplyr::select(writeoff, pd, pd_lasso) %>%
	summarise(meanPD=mean(pd, na.rm=TRUE), 
		mean_lasso=mean(pd_lasso), 
		meanWO = mean(as.numeric(writeoff), na.rm=TRUE), count = n())

 # # Also try grouping by:
	Risk.Rating.Category
	Year
	# Depth.of.Management
	# Lending.Region
	# Portfolio
	# Sector.and.Perishability
	# Risk.Bucket


	# group by pd cuts, then show pds on test from model trained on training, 

