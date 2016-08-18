# Write outputs for 





# coefs



# scale table for sales growth rank
	# sales growth nom is the log of most recent sales minus the sales from the second most recent.
	# log( sales_most_recent - sales_second_most_recent ) 
	sales_rank <- quantile(df.rap$sales_growth_nom, probs = seq(0, 1, by = 0.01), na.rm = TRUE)
	summary(2^sales_rank)

	kad <- filter(df.rap, LoanID == 603)
	kad <- filter(df.rap, LoanID == 1260)
	kad <- kad[rep(1:nrow(kad), each=11),] # duplicate x rows
	kad$sales_growth_pct_rank    <- seq(0, 100, 10)
	kad$sales_growth_rank_sq <- seq(0, 100, 10)^2

	p1 <- predict(glm.mod.all$finalModel, kad, type = 'response')
	plot(kad$sales_growth_pct_rank, p1) 

	virmax <- filter(df.rap, LoanID == 1219)  # 1219 virmax  1659
	virmax <- virmax[rep(1:nrow(virmax), each=11),] # duplicate x rows
	virmax$sales_growth_pct_rank    <- seq(0, 100, 10)
	virmax$sales_growth_pct_rank_sq <- seq(0, 100, 10)^2
	virmax$sales_growth_none <- 1
	# virmax$sales_growth_pct_rank_sq <- log(seq(1, 100, 9.9))^2

	p2 <- predict(glm.mod.all$finalModel, virmax, type = 'response')
	plot(virmax$sales_growth_pct_rank, p2) 

	virmax <- filter(df.rap, LoanID == 1219)  # 1219 virmax  1659
	virmax <- virmax[rep(1:nrow(virmax), each=11),] # duplicate x rows
	virmax$sales_growth_rank    <- seq(0, 100, 10)
	# virmax$sales_growth_pct_rank    <- seq(0, 100, 10)	
	virmax$sales_growth_rank_sq <- seq(0, 100, 10)^2

	p2 <- predict(glm.mod.all$finalModel, virmax, type = 'response')
	plot(virmax$sales_growth_rank, p2) 
