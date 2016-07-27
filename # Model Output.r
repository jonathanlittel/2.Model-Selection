# Model Output

# This file is mostly important to avoid confusing the subsetted dataset for fitting, that excudes active loans

library(pscl)
library(ggplot2)
library(caret)
library(pROC)


wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/Loss Model/1.Dataset Cleaning and Prep"
setwd(wd)
#filename <-  "https://rootcapital.box.com/shared/static/d7q5d7pfnvzao08x4ev7af4rj7bm7hve.csv"
filename <-  "rap_data.csv"
df.rap <- read.csv(filename, header=TRUE, sep=",")

# remember to reload the whole dataset, so it's not subset for active

df.rap$predicted_wo <- predict(glm4, df.rap, type="response")

write.csv(df.rap, 
	"C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Stress Testing/Loss Distribution Simulation/predicted_default.csv")
df.rap$predicted_default <- predict(glm4, df.rap, type="response")



library(stargazer)
s <- stargazer(glm4, glm5, glm6, glm7, type = "html",
          ci = TRUE)

write(s, file = "comparison.html")
