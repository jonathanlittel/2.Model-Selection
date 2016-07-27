# Create custom function for categorized and graphing pd iction types
# Takes a data frame with column 'pd '


# Note that the pd ictor, 'WriteoffsDummy', must be a factor (possible put that into this formula)
# df.rap$WriteoffsDummy <- as.factor(df.rap$WriteoffsDummy) or
# df$WriteoffsDummy <- as.factor(df.WriteoffsDummy)
# may also want to add pd ictor as argument to custom function

# The input for this is a dataframe with a column for the pd iction (not the cut pd iction)
plot_pred_type_distribution <- function(df, threshold) {
  v <- rep(NA, nrow(df))
  v <- ifelse(df$pd  >= threshold & df$WO == 1, "TP", v)
  v <- ifelse(df$pd  >= threshold & df$WO == 0, "FP", v)
  v <- ifelse(df$pd  < threshold & df$WO == 1, "FN", v)
  v <- ifelse(df$pd  < threshold & df$WO == 0, "TN", v)
  
  df$pred_type <- as.numeric(v)
  
  ggplot(data=df, aes(x=WO, y=pd )) + 
    geom_jitter( alpha=0.6) + # aes(color=pred_type),
    geom_violin(fill=rgb(1,1,1,alpha=0.75), color=NA) + 
    geom_hline(yintercept=threshold, color="red", alpha=0.6) +
    scale_color_discrete(name = "type") +
    labs(title=sprintf("Threshold at %.2f", threshold))
}


# example:
# cutoff <- 0.2 
# df.rap$WriteoffsDummy <- as.factor(df.rap$WriteoffsDummy)
# df.rap$pd  <- pd ict(glm18c, df.rap, type="response")
# plot_pred_type_distribution(df.rap, cutoff)
