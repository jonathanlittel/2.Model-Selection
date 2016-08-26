library(dplyr)
library(pROC)
library(readr)

dfr <- read.csv('https://rootcapital.box.com/shared/static/vombahid4q356z8s6lsmzx50jjidst0d.csv', skip = 1)
glimpse(dfr)

pds_new <- select(dfr, RC.Opp.Number, Loan.PD.Calc, pd_new = Accounting.fo.Tenor)
class(pds_new)

plot(pds_new$pd_new)
apply(pds_new, 2, class)

wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/PD Model/3.Outputs"
setwd(wd)
filename <-  "pds_08.19.16.csv" 
df.rap <- read_csv(filename)

df.rap <- left_join(df.rap, pds_new, by = 'RC.Opp.Number')

df <- filter(df.rap, active == 0)
roc <- roc(df$WO, df$pd)
roc_new <- roc(df$WO, df$pd_new)
roc
roc_new
