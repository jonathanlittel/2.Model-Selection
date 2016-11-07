# add adjustments to pds
library(readr)
library(dplyr)

wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/PD Model/2.Model Selection/outputs/final 08.19.16"
setwd(wd)
filename <-  "pds_08.19.16_unadjusted.csv" 
df.rap <- read_csv(filename)

# add adjustments for
# 1. coffee / non coffee
# 2. working capital to sales
# 3. max / min absolutes
# 4. tenor

df.rap <- df.rap %>%
  mutate(
    pd_temp = ifelse(
      coffee == TRUE, pd_one_year * 0.7, pd_one_year * 1.4
    ),
    wc_sales_adj = ifelse(
      working_capital_to_sales < -0.05, 0.04,
      ifelse(
        working_capital_to_sales <= 0 & working_capital_to_sales >= -0.05, 0.02,
        ifelse(working_capital_to_sales <= 0.02 & working_capital_to_sales > 0, 0.01,
               ifelse(working_capital_to_sales > 0.02 & working_capital_to_sales <= 0.09, 0,
                      ifelse(working_capital_to_sales > 0.09 & working_capital_to_sales < 0.4, -0.005,
                             ifelse(working_capital_to_sales >= 0.4 & working_capital_to_sales <= 1, 0,
                                    ifelse( working_capital_to_sales > 1, 0.01,
                      NA))))))),
    wc_sales_adj = replace(wc_sales_adj, is.na(working_capital_to_sales), 0.01),
    pd_temp = pd_temp + wc_sales_adj,
    pd_temp = replace(pd_temp, pd_temp > 0.4, 0.4),
    pd_temp = replace(pd_temp, pd_temp < 0.015, 0.015),
    pd_new  = 1 - (1 - pd_temp ) ^ tenor_years_min1,
    pd_adjusted_pre_tenor = pd_temp,
    pd_temp = NULL
  )


# plot(df.rap$pd, df.rap$pd_temp)
# abline(a = 0, b = 1)

# rename so that the new pd is just 'pd'
df.rap <- rename(df.rap, pd_old = pd, pd = pd_new)

wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/PD Model/2.Model Selection"
write.csv(df.rap, 'pds_08.19.16.csv', row.names = FALSE)
file.show('pds_08.19.16.csv')
names(df.rap)

# Client Max /Min	 	WC / Sales 	 	Industry 	 	 
# Table	 	 	Adjustment Table	 	 Multiplaction Factor
# Client Max = 40%	 	<-5%: +4%	 	 	Coffee	0.7	 
# Client Min = 1.5%	 	-5% to 0: +2%	 	 	Non-Coffee	1.4	 
# 0 to 2%: +1%	 	 			 
#   
#   >9%: -0.5%	 