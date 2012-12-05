
rm(list = ls(all = TRUE))

library(bayesm)

data(cheese)

retailers = unique(cheese$RETAILER)
nRetailers = length(retailers)
nRegressors = 3; #includes the intercept;
j = NULL
retailerData = NULL
lsCoeffs = matrix(c(rep(0,times = nRetailers*nRegressors)), 
						ncol = nRegressors)
varY = cbind(c(rep(0,times = nRetailers)))

nRetailers

for (i in 1:nRetailers){
	retailer = retailers[i]
	
	retailerSet = subset(cheese, cheese$RETAILER ==retailer )
	numberOfReps = dim(retailerSet)[1]
	j = c(rep(1,times=numberOfReps))
	
	#construct list entries
	y = log(retailerSet[,2])
	x = cbind(j, retailerSet$DISP, log(retailerSet$PRICE))
	
	retailerList = list(y = y,X = x) 	
	retailerData[[i]] = retailerList
	
	lsRegression = lm(y~x[,2] + x[,3])
	lsCoeffs[i,] = t(as.matrix(lsRegression$coeff))
	varY[i] = var(y)
}

Z = as.matrix(c(rep(1,times = nRetailers)))

A = as.matrix(.01)
Deltabar = matrix(c(rep(0,times = nRegressors)),nrow = 1) #Z has only intercept
ssq = varY
nu = nRegressors + 3;
nu.e = 3;
V = nu*diag(c(rep(.1, times = nRegressors)))

Prior = list(A = A, Deltabar = Deltabar, ssq = ssq, nu = nu, nu.e = nu.e, V=V)



dataInList = list(regdata = retailerData, Z = Z)

Mcmc = list(R = 50000, keep = 10)

myBayesianModel= rhierLinearModel(Data = dataInList, Prior = Prior, Mcmc = Mcmc)

