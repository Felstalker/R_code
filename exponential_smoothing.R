#The ts – library in R contains the function HoltWinters(x,alpha,beta,gamma),
#which lets one perform the Holt–Winters procedure on a time series x. One can
#specify the three smoothing parameters with the options alpha, beta and gamma.

beer<-read.csv("C://devl//data//beer.csv",header=T,dec=",",sep=";")
beer<-ts(beer[,1],start=1956,freq=12)

estHoltWinters <- HoltWinters(beer)

estHoltWinters


predHoltWinters <- predict(estHoltWinters,n.ahead=24)

plot(beer, xlim = c(1956, 1962)) # make room for the predictions
lines(estHoltWinters$fitted[,"xhat"],col="red")
lines(predHoltWinters, col = "green")