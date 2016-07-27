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
  glmSM <-glm(WriteoffsDummy ~ ., data=df.model, family='binomial', na.action=na.exclude)
  summary(glmSM)
  df.rap.inactive$pdSM <- predict(glmSM, df.rap.inactive, family='binomial', type='response')
  df.rap.active$pdSM <- predict(glmSM, df.rap.active, family='binomial', type='response')
  df.rap$pdSM <- predict(glmSM, df.rap, family='binomial', type='response')
  #####
  
# Substandard #
###############
  modelColsSub <- c("WriteoffsDummy",
                  "Substandard", 
                  "pd")
  df.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelColsSub]
  glmSub <-glm(WriteoffsDummy ~ ., data=df.model, family='binomial', na.action=na.exclude)
  summary(glmSub)
  df.rap.active$pdSub <- predict(glmSub, df.rap.active, family='binomial', type='response')
  df.rap.inactive$pdSub <- predict(glmSub, df.rap.inactive, family='binomial', type='response')
  df.rap$pdSub <- predict(glmSub, df.rap, family='binomial', type='response')
  #####

# Doubtful #
############
  modelColsD <- c("WriteoffsDummy",
                  "Doubtful", 
                  "pd")
  df.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelColsD]
  glmD <-glm(WriteoffsDummy ~ ., data=df.model, family='binomial',na.action=na.exclude)
  summary(glmD)
  df.rap.active$pdD <- predict(glmD, df.rap.active, family='binomial', type='response')
  df.rap.inactive$pdD <- predict(glmD, df.rap.inactive, family='binomial', type='response')
  df.rap$pdD <- predict(glmD, df.rap, family='binomial', type='response')
  #####

# Select the pd for the actual risk category
df.rap$pd_active <- ifelse(df.rap$September_Risk_Category=='Current',df.rap$pdSM,
                           ifelse(df.rap$September_Risk_Category=='Special Mention', df.rap$pdSM,
                                  ifelse(df.rap$September_Risk_Category=='Substandard', df.rap$pdSub,
                                         ifelse(df.rap$September_Risk_Category=='Doubtful', df.rap$pdD,
                                                'yikes'))))
  
df.rap$EL <- df.rap$pd_active * df.rap$balance_0915
sum(na.omit(df.rap$EL))
mean(na.omit(df.rap$pd))
mean(na.omit(df.rap$pd_active))

plot(sort(df.rap$pd_active))

mean(na.omit(df.rap$pd_active[df.rap$active==1]))

  
outputCols <- c('LoanID', 'pd', 'pd_active')
df.output <- df.rap[,names(df.rap) %in% outputCols]
