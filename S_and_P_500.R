# TODO: Add comment
# 
# Author: bao1827
###############################################################################
library(quantmod)

# download.file("http://www2.standardandpoors.com/spf/xls/index/sp500.xls", "/home/bao1827/workspace/Stocks/data/S_and_P.xls")

s_and_p.data <- read.csv("/home/bao1827/workspace/Stocks/data/S_and_P.csv")

sp_tickers <- as.character(s_and_p.data$Ticker)

UnionStockData <- function(next.ticker){
	getSymbols(next.ticker, src = 'yahoo', from = '2001-01-01', to = Sys.Date())
	new.data <- data.frame(ticker = next.ticker, eval(parse(text = next.ticker), .GlobalEnv ) )
	new.data$date <- index( eval(parse(text = next.ticker) ) )
	names(new.data) <- c("ticker", "open", "high", "low", "close", "volume", "adjusted", "date")
	
	stock.history <<- rbind(stock.history, new.data)
} 

stock.history <- data.frame(NULL)

n.stocks <- 15 # length(sp_tickers)
for(i in 1:n.stocks){
	
	print(sp_tickers[i])
	try( UnionStockData(sp_tickers[i]) )
	
}

#write.csv(stock.history, file = "/home/bao1827/workspace/Stocks/data/S_and_P_last10yrs.csv")


