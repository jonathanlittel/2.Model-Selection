# Stepwise regression



df.trainx <- df.train[,c(9:10, 25, 35,122)]
null=glm(WriteoffsDummy ~ 1, data=df.train, family="binomial", na.action=na.exclude)
full=glm(WriteoffsDummy ~ ., data=df.trainx, family="binomial", na.action=na.exclude)


step(null, scope=list(lower=null, upper=full), direction="forward")




library(MASS)

modelColumns0 <- c("WriteoffsDummy", "TotalAssets_log", "Sales_log", "EBITDA.Margin.", "Total.Liabilities.Total.Assets",
	"max_pct_per_buyer", "multiple_segments" , "Cash.Conversion.Cycle.", 
	"Total.Debt.Service", "max_pct_per_buyer", "Working.Capital",
	"EBITDA.Total.Debt.Service", "Total.Asset.Turnover", "Gross_Margin_Variation", 
	"Negative_Working_Capital_Flag", "margin_sd", "COGS.Margin", "isCoffee", 
	"member_receivables_dummy", "Sales_CAGR")

modelColumns1 <- c("WriteoffsDummy", "Sales", "max_pct_per_buyer", "multiple_segments" , "Cash.Conversion.Cycle.", 
	"EBITDA.Total.Debt.Service", "Total.Asset.Turnover", "Gross_Margin_Variation", 
	"Negative_Working_Capital_Flag", "margin_sd", "COGS.Margin", "isCoffee", 
	"member_receivables_dummy", "Sales_CAGR")

df.train.model <- df.train[,names(df.train) %in% modelColumns]

# Two ways to subset for removing NAs
# na.omit(df.train.model)
df.model <- df.train.model[complete.cases(df.train.model),]

model1 <- "WriteoffsDummy ~ log(Sales + 1 - min(na.omit(Sales))) + max_pct_per_buyer +
    multiple_segments + 
    Cash.Conversion.Cycle. + EBITDA.Total.Debt.Service +
    Total.Asset.Turnover + Gross_Margin_Variation + Negative_Working_Capital_Flag +
      margin_sd + COGS.Margin + isCoffee + member_receivables_dummy"

glm1 <- glm(model1, data=df.model, family="binomial", na.action=na.exclude)

stepAIC(glm1, direction=c("both"))


# Which chose this: 
    glm(formula = WriteoffsDummy ~ log(Sales + 1 - min(na.omit(Sales))) + 
                Cash.Conversion.Cycle. + COGS.Margin + isCoffee + member_receivables_dummy, 
            family = "binomial", data = df.model, na.action = na.exclude)