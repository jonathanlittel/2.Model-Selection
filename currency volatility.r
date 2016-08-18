# optional code to get past/future currency pair volatility
library(quantmod)

setwd(data_wd)
fx_code_map <- read.csv('country-code-to-currency-code-mapping.csv')

setdiff(levels(df.rap$Country), levels(fx_code_map$Country))

fx_code_map$Country <- gsub('Tanzania', "Tanzania, United Republic Of",  fx_code_map$Country)
fx_code_map$Country <- gsub('Congo Republic of the Democratic', "Congo, The Democratic Republic Of The", fx_code_map$Country)
fx_code_map$Country <- gsub('Ivory Coast', "Côte D'Ivoire", fx_code_map$Country)

setdiff(unique(df.rap$Country), unique(fx_code_map$Country))

# fix the mismatched codes  
currencies <- levels(df.rap$Currency.y)
currencies <- paste('USD/', currencies, sep = "")
for (c in 1:length(currencies)) {
	# getFX(currency[c], from = from[c], to = to[c]) # if you want to try getting separate ranges
	getFX(currencies[c], from = Sys.Date() - (5 * 364), to = Sys.Date())
	# ret <- log(lag(price)) - log(price)
 #    vol[c] <- sd(ret) * sqrt(250) * 100
}

get_fx_range <- function (currency, from = Sys.Date() - 30, to = Sys.Date() ) {
	currency_pair <- paste('USD', currency, sep = "")
	x <- get(currency_pair) # need to first run getFX for currency to create exchange rate object
	d1 <- gsub('-', '', as.character(from))
	d2 <- gsub('-', '', as.character(to))
	d_range <- paste(d1, d2, sep = "/")
	cut <- x[d_range]
	cut
}

get_vol <- function (x, periods = 250) {
	price <- x
	return <- log(lag(price)) - log(price)
	vol <- sd(return, na.rm = TRUE) * sqrt(periods)
	vol
}

# get_x_months(USDCOP, 'COP')
# df.rap$vol <- NA
df.rap$vol_past <- NA
df.rap$vol_future <- NA
window <- 120 
fx_rate_range <- NULL
for (loan in 1:nrow(df.rap)) {
	from_date <- max(df.rap$Close.Date[loan], (Sys.Date() - 5 * 365)) # since onanda only allows past five years
	# to_date   <- min()
	fx_rate_range <- get_fx_range(df.rap$Currency.y[loan], from = from_date, to = from_date + window)
	df.rap$vol_future[loan] <- get_vol(fx_rate_range, periods = length(fx_rate_range))
	from_date <- max(df.rap$Close.Date[loan], (Sys.Date() - (5 * 365) + window)) # since onanda only allows past five years
	fx_rate_range <- get_fx_range(df.rap$Currency.y[loan], from = from_date - window, to = from_date)
	df.rap$vol_past[loan] <- get_vol(fx_rate_range, periods = length(fx_rate_range))
}
