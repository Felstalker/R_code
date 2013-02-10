library(mlogit)


mydata <- read.csv(url("http://www.ats.ucla.edu/stat/r/dae/mlogit.csv"))
attach(mydata)
names(mydata)

# http://www.ats.ucla.edu/stat/r/dae/mlogit.htm
# Our goal is to associate the brand choices with age and gender. 

table(brand)
table(female)

xtabs(~ female + brand)

mydata$brand<-as.factor(mydata$brand)

# Then we use the mlogit.data command to expand the data.  For each observation in our original dataset, we now have 3 observations--one for each value of brand. In this new dataset, the brand number appears under alt and whether or not the given brand was selected by the person in the brand variable as a TRUE or FALSE. 

mldata<-mlogit.data(mydata, varying=NULL, choice="brand", shape="wide")

#we can indicate alternative-specific and individual-specific variables. We are only interested in individual-specific variables, which are indicated after the | symbol.
mlogit.model<- mlogit(brand~1|female+age, data = mldata, reflevel="1")
summary(mlogit.model)

newdata <- data.frame(cbind(age = rep(24:38, 2), female = c(rep(0, 15), rep(1, 15))))
logit1 <- rep(0, 30)
logit2 <- -11.774655 + 0.523814*newdata$female + 0.368206*newdata$age
logit3 <- -22.721396 + 0.465941*newdata$female + 0.685908*newdata$age

logits <- cbind(logit1, logit2, logit3)
p.unscaled <- exp(logits)
#We can exponentiate these logit values and scale them, knowing that the probabilities of the three brands must sum to one
p <- cbind(newdata, (p.unscaled / rowSums(p.unscaled)))
colnames(p) <- c("age", "female", "pred.1", "pred.2", "pred.3")

