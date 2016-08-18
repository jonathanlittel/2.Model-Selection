# Pred vs actual by cuts on pds
# first load the underwriting pds, and filter by inactive
library(dplyr)
library(xlsx)
# convert to number so averages nicely
df.rap.inactive$writeoff <- as.numeric(df.rap.inactive$WO) - 1

# function to find predicted vs actual of writeoffs
	vet_pd <- function(x, cut) {
		x %>%
			dplyr::group_by_(cut) %>%
			dplyr::select(writeoff, pd) %>%
			summarise(meanPD=mean(pd, na.rm=TRUE), 
			# mean_lasso=mean(pd_lasso), 
			meanWO = mean(as.numeric(writeoff), na.rm=TRUE), count = n())
	}
	df.rap.inactive$sales_cut <- cut(df.rap.inactive$Sales, quantile(df.rap.inactive$Sales))
	df.rap.inactive$tenor_cut <- cut(df.rap.inactive$Tenor_years, quantile(df.rap.inactive$Tenor_years))
	df.rap.inactive$pd_cut <- cut(df.rap.inactive$pd, quantile(df.rap.inactive$pd))
	df.rap.inactive$asset_cut <- cut(df.rap.inactive$TOTAL.ASSETS, quantile(df.rap.inactive$TOTAL.ASSETS))

	c1 <- vet_pd(x = df.rap.inactive, cut = "Lending.Region")
	c2 <- vet_pd(x = df.rap.inactive, cut = "Year")
	c3 <- vet_pd(x = df.rap.inactive, cut = "Risk.Rating.Category")
	c4 <- vet_pd(x = df.rap.inactive, cut = "Depth.of.Management")
	c5 <- vet_pd(x = df.rap.inactive, cut = "Sector.and.Perishability")
	c6 <- vet_pd(x = df.rap.inactive, cut = "Risk.Bucket")		
	c7 <- vet_pd(x = df.rap.inactive, cut = "sales_cut")		
  	c8 <- vet_pd(x = df.rap.inactive, cut = "tenor_cut")		
  	c9 <- vet_pd(x = df.rap.inactive, cut = "asset_cut")		
  	c10 <- vet_pd(x = df.rap.inactive, cut = "pd_cut")		
    
  setwd(output_dir)
  	write.xlsx(c1, 'pred_vs_actual_comparison_08.17.16.xlsx', sheetName = names(c1)[1])
	write.xlsx(c2, 'pred_vs_actual_comparison_08.17.16.xlsx', sheetName = names(c2)[1], append = TRUE)
	write.xlsx(c3, 'pred_vs_actual_comparison_08.17.16.xlsx', sheetName = names(c3)[1], append = TRUE)
	write.xlsx(c4, 'pred_vs_actual_comparison_08.17.16.xlsx', sheetName = names(c4)[1], append = TRUE)
	write.xlsx(c5, 'pred_vs_actual_comparison_08.17.16.xlsx', sheetName = names(c5)[1], append = TRUE)
	write.xlsx(c6, 'pred_vs_actual_comparison_08.17.16.xlsx', sheetName = names(c6)[1], append = TRUE)
	write.xlsx(c7, 'pred_vs_actual_comparison_08.17.16.xlsx', sheetName = names(c7)[1], append = TRUE)
	write.xlsx(c8, 'pred_vs_actual_comparison_08.17.16.xlsx', sheetName = names(c8)[1], append = TRUE)
	write.xlsx(c9, 'pred_vs_actual_comparison_08.17.16.xlsx', sheetName = names(c9)[1], append = TRUE)
	write.xlsx(c10, 'pred_vs_actual_comparison_08.17.16.xlsx', sheetName = names(c10)[1], append = TRUE)


# df.rap.inactive %>%
# 	group_by(Lending.Region) %>%	
# 	dplyr::select(writeoff, pd) %>%
# 	summarise(meanPD=mean(pd, na.rm=TRUE), 
# 		# mean_lasso=mean(pd_lasso), 
# 		meanWO = mean(as.numeric(writeoff), na.rm=TRUE), count = n())

 # #  try grouping by:
	# Risk.Rating.Category
	# Year
	# Depth.of.Management
	# Lending.Region
	# Portfolio
	# Sector.and.Perishability
	# Risk.Bucket
