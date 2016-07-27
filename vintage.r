library(lubridate)
source('Preprocess data file.r')

# summarise wo by year
	if(class(df.rap$Close.Date)=='Date') 'ok' else df.rap$Close.Date <- as.Date(df.rap$Close.Date, '%Y-%m-%d')

	# df.rap$Close.Date <- as.Date(df.rap$Close.Date, '%m/%d/%Y')
	# df.rap$Close.Date <- as.Date(df.rap$Close.Date, '%Y-%m-%d')
	vintage.total <- df.rap %>%
			group_by(year(Close.Date)) %>%
			summarise(pd_avg=mean(pd, na.rm=TRUE), WO_avg=mean(as.numeric(WO)-1, na.rm=TRUE))

# summarise wo by year, and by active/inactive
	if(class(df.rap.inactive$Close.Date)=='Date') 'ok' else df.rap.inactive$Close.Date <- as.Date(df.rap$Close.Date, '%Y-%m-%d')
	vintage.inactive <- df.rap.inactive %>%
			group_by(year(Close.Date)) %>%
			summarise(pd_avg=mean(pd, na.rm=TRUE), WO_avg=mean(as.numeric(WO)-1, na.rm=TRUE))

# summarise wo by year, and by active/inactive
	df.rap.active <- filter(df.rap, active==1)

	vintage.active <- df.rap.active %>%
			group_by(year(Close.Date)) %>%
			summarise(pd_avg=mean(pd, na.rm=TRUE), WO_avg=mean(as.numeric(WO)-1, na.rm=TRUE))
vintage <- NULL
vintage <- cbind(vintage.total, vintage.inactive[,2:3])
vintage <- merge(vintage, vintage.active, by='year(Close.Date)', all.x=TRUE)
colnames(vintage) <- c('Origination Year', 'PD total', 'WO total', 'PD inactive', 'WO inactive',
		'PD active', 'WO active')


# by active/inactive with annualized pd
	# df.rap$Close.Date <- as.Date(df.rap$Close.Date, '%m/%d/%Y')
	vintage.x <- df.rap %>%
			group_by(year(Close.Date), active) %>%
			summarise(pd_avg=mean(pd, na.rm=TRUE), WO_avg=mean(as.numeric(WO)-1, na.rm=TRUE),
			 pd_ann = mean(pd/tenor_years_min1, na.rm=TRUE)) 

filter(vintage.x, active==0)