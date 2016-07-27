# Load file and set options
#####
options(scipen=99, digits=3)

wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Stress Testing/Loss Distribution Simulation"
setwd(wd)
#filename <-  "https://rootcapital.box.com/shared/static/d7q5d7pfnvzao08x4ev7af4rj7bm7hve.csv"
filename <-  "predicted_default.csv"
df.rap <- read.csv(filename, header=TRUE, sep=",")

wd <- "C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/Risk Profile/Loss Model/2.Model Selection"
setwd(wd)


# This is done in rap_cleaning_2.r, but re-doing here since loans me be
# manually added after

df.rap$balance_0915[!is.finite(df.rap$balance_0915)] <- 0
df.rap$active <- ifelse(df.rap$balance_0915>0,1,0)

############
# Progressive loss model
df.rap.active <- df.rap[which(df.rap$active==1 & df.rap$balance_0915>0 & df.rap$last_year==1),]
df.rap.inactive <- df.rap[which(df.rap$active==0 & df.rap$last_year==1),]

# Special Mention #
##################
modelColsSM <- c("WriteoffsDummy",
                 "Special_Mention", 
                 "pd")

df.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelColsSM]
lmSM <- lm(WriteoffsDummy ~ ., data=df.model, na.action=na.exclude)
summary(lmSM)
df.rap.inactive$pdSM <- predict(lmSM, df.rap.inactive, type='response')
df.rap.active$pdSM <- predict(lmSM, df.rap.active, type='response')
df.rap$pdSM <- predict(lmSM, df.rap, type='response')

# Substandard
#####
modelColsSub <- c("WriteoffsDummy",
                  "Substandard", 
                  "pdSM")
df.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelColsSub]
lmSub <- lm(WriteoffsDummy ~ ., data=df.model, na.action=na.exclude)
summary(lmSub)
df.rap.active$pdSub <- predict(lmSub, df.rap.active, type='response')
df.rap.inactive$pdSub <- predict(lmSub, df.rap.inactive, type='response')
df.rap$pdSub <- predict(lmSub, df.rap, type='response')


# Doubtful
#####
modelColsD <- c("WriteoffsDummy",
                "Doubtful", 
                "pdSub")
df.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelColsD]
lmD <- lm(WriteoffsDummy ~ ., data=df.model, na.action=na.exclude)
summary(lmD)
df.rap.active$pdD <- predict(lmD, df.rap.active, type='response')
df.rap.inactive$pdD <- predict(lmD, df.rap.inactive, type='response')
df.rap$pdD <- predict(lmD, df.rap, type='response')


df.rap.active$test <- df.rap.active$pd + df.rap.active$Special_Mention*lmSM$coefficients[2] 
# need to include intercepts here

lmSM$coefficients[2] 
lmSub$coefficients[2]
lmD$coefficients[2] 