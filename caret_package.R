# TODO: Add comment
#   to alter: Preferences->StatET->Code Generation
# Author: OGOREKB
###############################################################################
setInternet2(TRUE)
options(width = 5000)

library(caret)
browseVignettes("caret")

data(mdrr) #creates the elements explored below
head(mdrrDescr)
mdrrClass

#near zero variance in the predictors?
nzv <- nearZeroVar(mdrrDescr)
filteredDescr <- mdrrDescr[, -nzv]
descrCor <- cor(filteredDescr)
highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
filteredDescr <- filteredDescr[,-highlyCorDescr]

set.seed(1)
inTrain <- sample(seq(along.with = mdrrClass), length(mdrrClass)/2)
trainDescr <- filteredDescr[inTrain,]
testDescr <- filteredDescr[-inTrain,]
trainMDRR <- mdrrClass[inTrain]
testMDRR <- mdrrClass[-inTrain]

fitControl <- trainControl(method = "LGOCV", #Leave Group Out Cross Validation
							 p = .75, #training proportion
							 number = 30, #Number of folds/#number of resampling iterations
							 returnResamp = "all",
					 		 verboseIter = FALSE
			  )
	
set.seed(2)

svmFit <- train(trainDescr, trainMDRR, 
		        method = "svmRadial",
				preProcess = c("center", "scale"),
				tuneLength = 4, # we have a grid of tuning parameter values for all p parameters. The default is 3^p.
				trControl = fitControl
		)
svmFit
# Notes:
# 1. Sigma is estimated from the Kernlab package, so only C applies to the grid
# 2. "Accuracy" is the overall agreement rate (remember, this is classification) averaged over cross-validation iterations.
# 3. If this were a numeric regression problem, we would see average RMSE and average R^2 (also known as Q^2).
# 4. The train function allows for a formula interface, provided the number of predictor variables is not so large

mbrrPredictions <- extractPrediction(list(svmFit), testX = testDescr, testY = testMDRR) #svmFit is already stored as a list, but it's class is not list unless you use the list function
# you could actually pass this function a number of fits to compare predictions #

mbrrTestPredictions <- mbrrPredictions[mbrrPredictions$dataType == "Test",]
# note the fields obs and pred in  head(mbrrTestPredictions )
with( mbrrTestPredictions, sensitivity(pred, obs) )
with( mbrrTestPredictions, specificity(pred, obs) )

with( mbrrTestPredictions, confusionMatrix(pred, obs) )  #Not just a confusion matrix, but an entire summary of the classification model results!

# The "No Information Rate" is just the largest proportion of observed classes. #

#### Reciever Operating Characteristic Curve ####
mbrrProbs <- extractProb(list(svmFit), testX = testDescr, testY = testMDRR)