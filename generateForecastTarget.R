# TODO: Add comment
# 
# Author: bao1827
###############################################################################

## using stock.history data frame from S_and_P_500.R ##

#There are usually 252 trading days in a year. 
nrows.data <- nrow(stock.history)
stock.history$lag_adj_151 <- c(rep(NA, times=151), stock.history$adjusted[1:(nrows.data-151)])
stock.history$lag_date <- as.Date(c(rep(-10000,times=151), stock.history$date[1:(nrows.data-151)]))
stock.history$lag_ticker <- c(rep(NA, times=151), as.character(stock.history$ticker[1:(nrows.data-151)]) )

#stock.history <- within(stock.history, check <- lead_ticker ==ticker)
rownames(stock.history) <- NULL
stock.history <- subset(stock.history, lag_ticker == ticker)
stock.history$lag_ticker <- NULL
stock.history$adj_price_up <- ifelse(stock.history$adjusted > stock.history$lag_adj_151, 1, 0)

write.csv(stock.history, file = "/home/bao1827/workspace/Stocks/data/S_and_P_V2.csv", row.names = FALSE)