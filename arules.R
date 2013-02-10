library(arules)
library(Matrix)
library(pmml)
library(RODBC)

## Helpful webpage ##
## http://rss.acs.unt.edu/Rdoc/library/arules/html/transactions-class.html ##

#### Example 1 ####
data("Epub")
Epub  #sparse incidence matrix format

summary(Epub) #is of type transactions, not ngCMatrix
getSlots("transactions")

# Let''s look at this data
names(Epub@transactionInfo) # a timestamp describes each transaction. This is also available in the transactionInfo() function.
Epub@data # the ngCMatrix from the package Matrix
names(Epub@itemInfo)
names(Epub@itemsetInfo) # just the sessions in here

# get the year off the timestamp #
year <- strftime(as.POSIXlt(transactionInfo(Epub)[["TimeStamp"]]), "%Y")

Epub2003 <- Epub[year == "2003"]

# let us look at the binary incidence matrix #
image(Epub2003)

# select transactions containing more than 20 items #
transactionInfo(Epub2003[size(Epub2003)>20])

# let us inspect the transactions:
inspect(Epub2003[1:5]) # Only now am I actually looking at "basket data"
as(Epub2003[1:5], "list")# prefer a list?
is.vector(as(Epub2003[1:5], "list")[["session_47b7"]]) #each transaction is a character vector

EpubList <- as(Epub2003, "list")

as(Epub2003[1:5], "data.frame")

# inverting the relationship... tidLists are the transactions associated with a product #
EpubTidLists <- as(Epub, "tidLists") # still in a sparse format
as(EpubTidLists[1:3], "list")


### This is an example where I can use Teradata Warehouse Miner...either the regular Association Rule analysis or the Sequence Analysis provided ###
### first, I need to coerse the transactions object into true basket data

#### Example 2 ####
EpubDataFrame <- data.frame(tid = NULL, item = NULL)

for (i in 1:length(EpubList)){
	for (j in 1:length(EpubList[[i]]))
	EpubDataFrame <- 	 rbind(EpubDataFrame, data.frame(tid = attributes(EpubList)$names[i], item =  EpubList[[i]][j]) )
		}
# now I can append transaction info by merging transactionInfo(Epub), but the sequence analysis in TWM needs the items to have a 
# time ordering with them.

CKS <- odbcConnect("cksdata", uid = "ogorekb", pwd = "Teradata34")
sqlSave(CKS, EpubDataFrame, "all_wkscratchpad_db.BAO_Epub")
odbcClose(CKS)


data("AdultUCI")
z = edit(AdultUCI)

## Remove two variables that will not be useful ##
AdultUCI[["fnlwgt"]] <- NULL    # I never knew I could delete a variable like this
AdultUCI[["education-num"]] <- NULL

## We must transform all remaining numeric variables into suitable categories ##
### Two cool functions here: 
##      1) cut: divides the range of x into intervals and codes the values in x according to which interval they fall.
##      2) ordered: part of the factor() function. Ensures that the factor levels are ordered.

AdultUCI[["age"]] <- ordered(
		cut(x = AdultUCI[["age"]], breaks = c(15,25,45,65,100)),
			labels = c("Young", "Middle-aged", "Senior", "Old")
		)
		
AdultUCI[["hours-per-week"]] <- ordered(
		cut(x = AdultUCI[["hours-per-week"]], breaks = c(0,25,40,60,168)),
		    labels = c("Part-time", "Full-time", "Over-time", "Workaholic")
		)		

medianCG <- median(AdultUCI[["capital-gain"]][AdultUCI[["capital-gain"]] > 0])
AdultUCI[["capital-gain"]] <- ordered(
		cut(x = AdultUCI[["capital-gain"]], breaks = c(-Inf, 0, medianCG, Inf)), 
				labels = c("None", "Low", "High")
			)
medianCL <- median(AdultUCI[["capital-loss"]][AdultUCI[["capital-loss"]] > 0])
AdultUCI[["capital-loss"]] <- ordered(
		cut(x = AdultUCI[["capital-loss"]], 
		breaks = c(-Inf, 0, medianCL, Inf)),
		labels = c("none", "low", "high")
		)
		
z = edit(AdultUCI)



## concept of data type coercion...I see paralells for us when we force data from 
## different source systems into CKS

Adult <- as(AdultUCI, "transactions")
WRITE(Adult, file = "C:/devl/output/Ad_trans.csv", sep = ",")

summary(Adult) # see the "most frequent items" output. Notice that an "item" is a concatination of a variable and one of it''s values.

str(Adult)  ## Adult is of type transactions. Notice that data, within adult, is of type ngCMatrix. The C stands for "compressed" format. Probably the n stands for "non-zero".
## the ngCMatrix appears to be THE data for the Matrix class in the arules documentation

# I read that I am not supposed to work with an ngCMatrix, rather I should work
# with the convenient itemMatrix class 

# enter in the concept of "support" with the itemFrequencyPlot
itemFrequencyPlot(Adult, support = 0.1, cex.names = 0.8) # think of how this could be used in a transactional view of markov processes

# GO TIME!! #
A.rules <- apriori(Adult, parameter = list(support = 0.01, confidence = 0.6))
# an important default is the maxlen parameter, which states how large the associations can be

summary(A.rules)
#over 80,000 rules! A suggestion is to look at only subsets of rules where the consequent,
# or right hand side (rhs) contains a particular "item" (or variable-value pair). In this case,
# we are only interested in a single set of consequents {income = small} and {income = large}.

rulesIncomeSmall <- subset(A.rules, subset = rhs %in% "income=small" & lift > 1.2)
rulesIncomeLarge <- subset(A.rules, subset = rhs %in% "income=large" & lift > 1.2)

inspect(head(SORT(rulesIncomeSmall, by = "confidence"), n = 3))
inspect(head(SORT(rulesIncomeLarge, by = "confidence"), n = 3))

rulesIncomeSmall # we see here that we have a set of items
is(rulesIncomeSmall, "rules") # we are looking at an object of class "rules"

getSlots("rules")
summary(head(rulesIncomeSmall)@lhs) #antecedents and consequents will also take matrix form,
# with itemset ids as rows and times as columns 

WRITE(rulesIncomeSmall, file = "C:/devl/output/rules.csv", sep = ",", col.names = NA)

saveXML(pmml(rulesIncomeSmall), file = "C:/devl/output/rules.xml")

#question: how to make AdultUCI into a dataset that can be used to consume the pmml
# the saved pmml file only specifies two categorical variables named transaction and item
# I assume I have to find a way to turn AdultUCI into true "basket data"

getSlots("transactions")
head(Adult@transactionInfo)

### This is the itemInfo "facade" to the Matrix class
Adult@itemInfo[1:5,] #it appears that the pmml "item" variable is the "labels" variable in this set

nrow(Adult@transactionInfo)

nrow(Adult@itemInfo)
ncol(Adult@itemInfo)


# Maybe the ngCMatrix is the place to look!!

testDat = rbind(
		c("t1", "Red"),
		c("t1", "Yellow"),
		c("t2", "Blue"),
		c("t3", "Red")
)

testDat <- data.frame(testDat)
names(testDat) = c("tid", "colorVar")

testDat.tr <- as(testDat, "transactions")
inspect(testDat.tr) #this is clearly NOT the way to read in basket data

tidList = list()
for ( i in 1:nlevels(testDat$tid)){
	  currentTid <- as.character(testDat[testDat$tid == levels(testDat$tid)[i], "colorVar"] ) 
	  tidList[[ levels(testDat$tid)[i] ]] <- currentTid
	}

testDat.tr.2 <- as(tidList, "transactions")
inspect(testDat.tr.2)
# This is more like it!! The coersion from a list to a transactions object is very natural.
# it seems that there is only a very specific notion of a data.frame -> transaction object coersion

testList <- as.list(testDat)

signature(from = "transactions", to = "matrix")

testDat.tr.2 <- new("transactions", transactionInfo = testDat$transaction, itemInfo = testDat$item)
#Class "itemMatrix" -- Sparse Binary Incidence Matrix to Represent Sets of Items, is what itemInfo is supposed to be



