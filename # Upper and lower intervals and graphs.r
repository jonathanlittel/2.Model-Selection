	library('reshape2')

	pr <- predict(glm.mod.all$finalModel, df.rap, type="response", se.fit = TRUE, level=.95)
	family <- family(glm.mod.all$finalModel) 

	# lower <- family$linkinv(pr$fit - qnorm(0.95) * pr$se.fit) 
	# upper <- family$linkinv(pr$fit + qnorm(0.95) * pr$se.fit) 

	ci <- 0.95 # 90% two tail 
	lower <- pr$fit - qnorm(ci) * pr$se.fit
	upper <- pr$fit + qnorm(ci) * pr$se.fit 

dfp <- as.data.frame(cbind(pr$fit, lower, upper))
   names(dfp)[1] <- 'pd'

   dfp <- dfp %>%
   	arrange(pd)

   dfp$n <- as.numeric(row.names(dfp))

# g <- ggplot(dfp, aes(x=n, y=pd)) + geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) # , position=V1

# g + 
#     geom_line(position=pd) +
#     geom_point(position=pd)


dfp_l <- melt(dfp, id="n")
h <- ggplot(dfp_l, aes(n, value)) + 
	geom_line(aes(colour=variable))  # aes(colour=variable)
	# geom_line(aes(y=lower, coulor = 'orange')) +
	# geom_line(aes(y=upper, coulor = 'grey'))

h