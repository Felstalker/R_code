rm(list = ls())
raw.df <- read.csv("c:/devl/bank_colin/Weibull Distribution Data.csv")

survival.df <- data.frame()


for(j in 2:12){
  temp.df <- raw.df[,c(1,j)]
  temp.df$cohort <- j - 1
  names(temp.df) = c("time", "survival.rate", "cohort")
  temp.df <- temp.df[!is.na(temp.df$survival.rate),]
  survival.df <- rbind(survival.df, temp.df)
}

# trick to get Weibull parameters with linear regression.
survival.df$new.y <- log(-log(1-survival.df$survival.rate))
survival.df$new.x <- log(survival.df$time)

my.lm <- lm(new.y ~ new.x, data = subset(survival.df, survival.rate < 1))
k = coef(my.lm)[2]
lambda = exp(-coef(my.lm)[1]/k)

cat("initial k,", k, " initial lambda, ", lambda)
# k must be positive, so I'll set it to 1

my.nls <- nls(survival.rate~exp(-(time/lambda)^k), data = survival.df, 
              start = list(k = 1, lambda = 77.8))

summary(my.nls)

# just copying what I see in the summary
k =  1.78399
lambda = 64.66560

# extrapolation grid
extrap.df <- data.frame(time = c(1:120))
extrap.df$curve <- with(extrap.df, exp(-(time/lambda)^k))

# plotting
plot(survival.rate~time, data = survival.df, xlim = c(0,120), ylim = c(0,1), 
     cex = .5, main = "Extrapolated attrition rate using Weibull curve",
              sub = "Survival Rate = exp(-(time/lambda)^k), lambda = 64.67, k =  1.78")

lines(curve~time, data = extrap.df, col="blue", lwd = 2)

