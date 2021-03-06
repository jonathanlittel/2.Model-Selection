#--------------------------------------------------------
# PREPROCESS AND LOAD
#---------------

#load libraries 
  library(pROC)
  library(pscl)
  library(stargazer)
  library(caret)
  library(dplyr)
  options(scipen=99, digits=3)
# Run script to load and process data
  wd_mod <- paste0("C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP Modules/",
    "Risk Profile/PD Model/2.Model Selection", sep="")
  setwd(wd_mod)
# Load custom function for graphing
  source('# Prediction Plot Function.r')
  source('Preprocess data file.r') # Cleans and creates variables
# Create folds
  set.seed(1235) # 527
  folds <- createFolds(
    df.rap.inactive$WO,
    k = 5)

#--------------------------------------------------------
# CROSSVALIDATE AUC
#---------------

# Train a model on each of five folds
     metrics_new_mod <- data.frame(
       AUC=0,
       AIC=0)
  # 8/19/16: add neg_working_cap (previously called wc_sales_cat)
  # 7/26/16: remove coffee and wc_sales_cat and 'Financial.Strat.Quality'
  modelCols <- c('WO', 'tenor_years_min1',  'sales_concent_cat', 'Depth.of.Management',
    'gross_margin_cat', 'Financial.Strat.Quality', 'past_arrears', 
    # 'sales_growth_nom',
    # 'sales_growth_pct',
    # 'sales_growth_rank',
    # 'sales_growth_rank_sq', 
    # 'Financial.Flexibility',   # predictive but removed
    # 'coffee',                  # predictive but removed
    'neg_working_cap',              
    'sales_growth_pct_rank',     # note that NAs and high growth are replaced with the median - analysis & models suggested neg risk..
    # 'sales_growth_none',         # see above - gave negative coef
    # 'sales_growth_pct_rank_sq',
    'country_risk')  

  # # on 6/22/16 (with coffee, wc_sales_cat) 'Financial.Strat.Quality',
  # modelCols <- c('WO', 'tenor_years_min1', 'sales_concent_cat', 'Depth.of.Management',
  #   'gross_margin_cat',  'past_arrears', 'wc_sales_cat',  
  #   'Financial.Strat.Quality', 'coffee', 'country_risk')  

# df.rap.inactive$sales_growth_pct_rank_sq <- log(df.rap.inactive$sales_growth_pct_rank)^2
df.rap.inactive$sales_growth_pct_rank_sq <- log(df.rap.inactive$sales_growth_pct_rank)^2


  df.train.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelCols]
  cutoff <- 0.15
  for (i in 1:5) {
    f <- paste("Fold",i, sep="")
    fold_list <- get(f, folds)
    fold <- unlist(fold_list)
    df.trn <- df.train.model[-c(fold),]
    df.tst <- df.train.model[c(fold),]
    name <- paste('glm', i, sep="")
    glm <- glm(WO ~ ., data=df.trn, family="binomial", na.action=na.exclude)
    assign(name, glm)
    roc <- roc(df.tst$WO,predict(glm, df.tst, type="response"))
    met_row <- c(roc$auc, glm$aic)
    metrics_new_mod <- rbind(metrics_new_mod, met_row)
    pd <- predict(glm, df.tst, type='response')
    wo_cut <- as.factor(ifelse(pd>cutoff,"Writeoff","Non_Writeoff"))
    cm <- confusionMatrix(wo_cut, df.tst$WO, 
      positive="Writeoff")
    cm_name <- paste("cm", i, sep="")
    assign(cm_name, cm)
    # p <- plot_pred_type_distribution(df.tst, cutoff)
    # p_name <- paste("p", i, sep="")
    # assign(p_name, p)
  }
  metrics_new_mod <- metrics_new_mod[-1,]
  metrics_new_mod["average",] <- apply(metrics_new_mod, 2, mean)
  metrics_new_mod
  
# Train a model on each of five folds using the risk rating glm model
     metrics_rr <- data.frame(
       AUC=0,
       AIC=0)
     
  modelColsrr <- c('WO', 'RiskRating')
    df.train.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelColsrr]

  for (i in 1:5) {
    f <- paste("Fold",i, sep="")
    fold_list <- get(f, folds)
    fold <- unlist(fold_list)
    df.trn <- df.train.model[-c(fold),]
    df.tst <- df.train.model[c(fold),]
    name <- paste('glmRR', i, sep="")
    glm <- glm(WO ~ ., data=df.trn, family="binomial", na.action=na.exclude)
    assign(name, glm)
    roc <- roc(df.tst$WO,predict(glm, df.tst, type="response"))
    met_row <- c(roc$auc, glm$aic)
    metrics_rr <- rbind(metrics_rr, met_row)
    pd <- predict(glm, df.tst, type='response')
    wo_cut <- as.factor(ifelse(pd>cutoff,"Writeoff","Non_Writeoff"))
    cm <- confusionMatrix(wo_cut, df.tst$WO, 
      positive="Writeoff")
    cm_name <- paste("cmRR", i, sep="")
    assign(cm_name, cm)
  }
  metrics_rr <- metrics_rr[-1,]
  metrics_rr["average",] <- apply(metrics_rr, 2, mean)


#--------------------------------------------------------
# CROSSVALIDATE ON FULL DATASET
#--------
# Using caret to to CV on full dataset

  df.train.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelCols]

     tc <- trainControl("repeatedcv",
         number=5,
         repeats=10, 
         savePred=TRUE,
         classProbs = TRUE,
         summaryFunction = twoClassSummary)
   # train_control <- trainControl(method="repeatedcv", number=5, repeats=2)
     set.seed(1348)
     glm.mod.all <- train(WO ~ .  ,
       data = df.train.model,
       method='glm',
       trControl=tc,
       metric = "ROC",
       family='binomial') 
    # # train similar model just for stargazer output (caret not compatible)
     # glm.mod.all.stargazer.output <- glm(WO ~ ., 
     #   data=df.train.model, 
     #   family="binomial", 
     #   na.action=na.exclude)
  pd <- predict(glm.mod.all, df.rap.inactive, type="prob")[,2]
  roc_all <- roc(df.rap.inactive$WO, pd)
  df.rap.inactive$predWO_cut <- as.factor(ifelse(pd>cutoff,"Writeoff", "Non_Writeoff"))
  confusionMatrix(df.rap.inactive$predWO_cut, df.rap.inactive$WO)
  summary(glm.mod.all$finalModel)
  glm.mod.all$results$ROC  # print the cv AUC
  # confint(glm.mod.all$finalModel, level = 0.75)
  # confint(glm.mod.all$finalModel, level = 0.33)


  # model on all data
    df.rap.inactive$pd <- predict(glm.mod.all, df.rap.inactive, type="prob")[,2]
    pd <- predict(glm.mod.all, df.rap.inactive, type="prob")[,2]
    roc_all <- roc(df.rap.inactive$WO, pd)
    df.rap.inactive$predWO_cut <- as.factor(ifelse(pd>cutoff,"Writeoff", "Non_Writeoff"))
    confusionMatrix(df.rap.inactive$predWO_cut, df.rap.inactive$WO, 
      positive="Writeoff")

  # RR on all data  
    modelColsRR <- c('WO', 'RiskRating')
    df.train.model <- df.rap.inactive[,names(df.rap.inactive) %in% modelColsRR]

    glm.rr <- glm(WO ~ ., data=df.train.model, family="binomial", na.action=na.exclude)
    pd <- predict(glm.rr, df.rap.inactive, type="response")
    roc_rr <- roc(df.rap.inactive$WO, pd)
    predWO_cut <- as.factor(ifelse(pd>cutoff,"Writeoff", "Non_Writeoff"))
    confusionMatrix(predWO_cut, df.rap.inactive$WO, 
      positive="Writeoff")


#--------------------------------------------------------
# RUN ON TEST DATA SET
#--------

   df.train.model <- df.train[,names(df.train) %in% modelCols]

      tc <- trainControl("cv",
            number=5, 
            savePred=TRUE,
            classProbs = TRUE,
            summaryFunction = twoClassSummary)
   # train_control <- trainControl(method="repeatedcv", number=5, repeats=2)
      set.seed(1348)
      glm.mod.all.train <- train(WO ~ .   ,
         data = df.train.model,
         method='glm',
         trControl=tc,
         metric = "ROC",
         family='binomial') 

   pd <- predict(glm.mod.all.train, df.test, type="prob")[,2]
   roc_test<- roc(df.test$WO, pd)
   df.test$predWO_cut <- as.factor(ifelse(pd>cutoff,"Writeoff", "Non_Writeoff"))
   confusionMatrix(df.test$predWO_cut, df.test$WO)
   summary(glm.mod.all.train$pred)
   glm.mod.all.train$results$ROC  # print the cv AUC

   pd <- predict(glm.mod.all.train, df.train, type="prob")[,2]
   roc_train<- roc(df.train$WO, pd)


#--------------------------------------------------------
# RUN ON TEST DATA SET - OLD MODEL
#--------

   df.train.model.rr <- df.train[,names(df.train) %in% modelColsrr]
   df.train.model.rr <- df.train.model.rr[complete.cases(df.train.model.rr),]
      tc <- trainControl("cv",
            number=5, 
            savePred=TRUE,
            classProbs = TRUE,
            summaryFunction = twoClassSummary)
   # train_control <- trainControl(method="repeatedcv", number=5, repeats=2)
      set.seed(1348)
      glm.mod.all.train.rr <- train(WO ~ .   ,
         data = df.train.model.rr,
         method='glm',
         trControl=tc,
         metric = "ROC",
         family='binomial') 

  # recode one NA risk rating in test set
  df.test$RiskRating[is.na(df.test$RiskRating)] <- mean(df.test$RiskRating, na.rm = T)

   pd_rr_test <- predict(glm.mod.all.train.rr, df.test, type="prob")[,2]
   roc_test<- roc(df.test$WO, pd_rr_test)
   df.test$predWO_cut <- as.factor(ifelse(pd_rr_test>cutoff,"Writeoff", "Non_Writeoff"))
   confusionMatrix(df.test$predWO_cut, df.test$WO)
   summary(glm.mod.all.train.rr$pred)
   glm.mod.all.train.rr$results$ROC  # print the cv AUC

   pd_rr_train <- predict(glm.mod.all.train.rr, df.train, type="prob")[,2]
   roc_test_rr <- roc(df.test$WO, pd_rr_test)

#--------------------------------------------------------
# SAVE OUTPUT
#--------
  output_dir <- paste("C:/Box Sync/Risk Appetite - Provisioning Project/Working Folders for RAP",
  " Modules/Risk Profile/PD Model/2.Model Selection/outputs", sep = "")
  setwd(output_dir)
# Matrix with coefficients
  coefs <- data.frame((coef(glm1)))
  coefs[,2] <- data.frame(coef(glm2))
  coefs[,3] <- data.frame(coef(glm3))
  coefs[,4] <- data.frame(coef(glm4))
  coefs[,5] <- data.frame(coef(glm5))  
  coefs[,6] <- apply(coefs, 1, mean)  
  names(coefs) <- c('run1', 'run2', 'run3', 'run4', 'run5', 'mean')

  # Matrix with coefficients for rr
  coefs_rr <- data.frame((coef(glmRR1)))
  coefs_rr[,2] <- data.frame(coef(glmRR2))
  coefs_rr[,3] <- data.frame(coef(glmRR3))
  coefs_rr[,4] <- data.frame(coef(glmRR4))
  coefs_rr[,5] <- data.frame(coef(glmRR5))  
  coefs_rr[,6] <- apply(coefs_rr, 1, mean)  
  names(coefs_rr) <- c('run1', 'run2', 'run3', 'run4', 'run5', 'mean')

  df.rap$pd_rr <- 1 / (1 + exp(-(coefs_rr[1,6] + coefs_rr[2,6]*df.rap$RiskRating)))
  df.rap.inactive$pd_rr <-  predict(glm.rr, df.rap.inactive, type="response")
  df.rap$pd_rr <-  predict(glm.rr, df.rap, type="response")
  df.rap$pd <- predict(glm.mod.all, df.rap, type="prob")[,2]

# run file with upper/lower bounds
  setwd(wd_mod)
  source('# Upper and lower intervals and graphs.r')
  summary(df.rap$pd_lower)
    # # Predict confidence intervals for pd (moved to upper and lower file)
    # pr <- predict(glm.mod.all$finalModel, df.rap, type="response", se.fit = TRUE)  
    # df.rap$pd_lower <- pr$fit - qnorm(0.95) * pr$se.fit
    # df.rap$pd_upper <- pr$fit + qnorm(0.95) * pr$se.fit 

  df.rap$pd_lower <- dfp$lower
  df.rap$pd_upper <- dfp$upper
  df.rap$pd_lower[df.rap$pd_lower<0] <- 0
# predict with a tenor of 1 for all loans
  df.temp <- df.rap
  df.temp$tenor_years_min1 <- 1
  df.temp$pd_1 <- predict(glm.mod.all, df.temp, type="prob")[,2]
  df.temp$pd_n <- 1 - ( ( 1 - df.temp$pd_1) ^ df.rap$tenor_years_min1)

  df.rap$pd_one_year <- df.temp$pd_1
  df.rap$pd_multiple_years <- df.temp$pd_n

# Save each glm
  glm_five <- list(glm1, glm2, glm3, glm4, glm5)
  glm_five_rr <- list(glmRR1, glmRR2, glmRR3, glmRR4, glmRR5)
  saveRDS(glm_five, 'glms_pd_model_08.19.16.rds')
  saveRDS(glm_five, 'glms_rr_model_08.19.16.rds')
  saveRDS(glm.mod.all, 'glm_model_08.19.16.rds')

  c <- coef(glm.mod.all$finalModel)
  c_rr <- coef(glm.rr)
  # last output on 6/24/16:
  # df.out <- dplyr::select(df.rap, LoanID, Account.Name, RC.Opp.Number,
  #   Close.Date, Maturity.at.Origination, tenor_years_min1,
  #   Sales.Concentration.to.a.Single.Buyer, Depth.of.Management, Gross_Margin_range,
  #   coffee, past_arrears, Working.Capital, Sales, working_capital_to_sales,
  #   Financial.Strat.Quality, country_risk, sales_concent_cat, wc_sales_cat,
  #   gross_margin_cat,
  #   active,  pd,  pd_one_year, pd_multiple_years,
  #   # pd_lower, pd_upper,
  #   Max_Risk_Category,
  #   Risk_Category, WO)

  df.out <- dplyr::select(df.rap, LoanID, Account.Name, RC.Opp.Number,
    active,  
    Close.Date, Maturity.at.Origination, 
    Sales.Concentration.to.a.Single.Buyer, 
    Gross_Margin_range,
    coffee,  Working.Capital, Sales, working_capital_to_sales,
    Max_Risk_Category,
    Risk_Category, WO,
    sales_growth_pct,
    Sales,
    Depth.of.Management,
    Financial.Strat.Quality,
    past_arrears,
    country_risk,
    sales_concent_cat,
    sales_growth_pct_rank,
    gross_margin_cat,
    tenor_years_min1,
    neg_working_cap,
    pd,  pd_one_year, pd_multiple_years,
    pd_lower, pd_upper,
    ltgwc
    )

  sales_rank <- quantile(df.rap$sales_growth_pct, probs = seq(0, 1, by = 0.01), na.rm = TRUE) * 100
  write.csv(sales_rank, 'sales_growth_rank_table.csv')
  write.csv(df.out, 'pds_08.19.16_unadjusted.csv', row.names = FALSE)
  write.csv(c, 'pd_model_coefs.08.19.17.csv', row.names = FALSE)

  nice_output <- tbl_df(coef(glm.mod.all$finalModel))
  nice_output$coefficient <- names(coef(glm.mod.all$finalModel))
  model_out <- stargazer(t(nice_output), type = "html",
                    ci = TRUE)

  options(digits = 5)
  write("PD Model Coefficients Q2 2016", file = "pd_model_08.19.16.html", append = FALSE)  
  write(model_out, file = "pd_model_08.19.16.html", append = TRUE)
  file.show("pd_model_08.19.16.html")

exp(coef(glm.mod.all$finalModel))

rocs <- data.frame(roc_all$auc,
  roc_train$auc,
  roc_test$auc,
  roc_rr$auc,
  glm.mod.all.train$results$ROC,
  glm.mod.all$results$ROC)
# metrics_new_mod

write.csv(t(rocs), 'rocs_08.19.16.csv', row.names = FALSE)
write.csv(nice_output, 'coefficients_08.19.16.csv', row.names = FALSE)

# miscellany:
# #   require(MuMIn)
# #   glm_Final <- model.avg(glm_five)
#   p <- ggplot(df.rap, aes(x=pd, y=RiskRating))
#   p + geom_point() + geom_smooth()

#   df.rap <- arrange(df.rap, desc(WO))
#   p <- ggplot(df.rap, aes(x=pd, y=pd_rr, colour=WO))
#   p + geom_point(size=0.5) + geom_abline(color='purple', size=1) + geom_smooth() 
#   p + geom_point(aes(size=WO),alpha=.75) + geom_abline(color='purple', size=1) # aes(size=WO), 


#   # reorder the factor levels
#   df.rap$WO <- factor(df.rap$WO, levels = rev(levels(factor(df.rap$WO))))

#   r <- ggplot(df.rap, aes(pd, tenor_years_min1 )) + geom_jitter() # geom_smooth() + fac
#   r + facet_grid(. ~ WO) + geom_smooth()
#   r + facet_grid(WO ~ .)


# anova(glm.mod.all,test="Chisq")
# drop1(glm.mod.all,test="Chisq")
# confint(glm.mod.all$finalModel, level = 0.75)
# confint(glm.mod.all$finalModel, level = 0.33)

