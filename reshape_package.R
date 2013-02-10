# TODO: Add comment
# 
# Author: OGOREKB
###############################################################################

library(reshape)



###MELTING####
data(smiths)

# See the distinction between the "id" variables and the "measured" variables.
# The one restriction is that all id variables must be of the same type
melted <- melt(smiths, id = c("subject", "time"))


# id variables encoded in column names...A1 and A2 are factors recorded at two time points
#if not specified, factor and integer-type columns become the id variables
trial <- data.frame( id = factor(1:4), A1 = c(1,2,1,2), A2 = c(2,1,2,1), B1 = c(3,3,3,3))

(trialm <- melt(trial))

#colshape() within the Reshape package: This function can be used to split up a column that has been pasted together. 

new.columns <- colsplit( trialm$variable, names = c("treatment", "time") )

( trialm <- cbind(trialm, new.columns) )

# The reshape package also gives you a renaming function, rename()

###CASTING####
cast(melted, time + subject ~ variable) # original
test <- cast(melted, ... ~ subject) # ... stands for all variables not in the casting equation, and guarantees no aggregation
	# The subject's values become column names
test$`John Smith` #Note the backticks

###Aggregation: When the combination of variables in the cast formula does not identify individual observations###
ffm <- melt(french_fries, id = 1:4, na.rm = TRUE)

#Most severe aggregation: reduction to a single number, described by the cast formula . ~ .
cast(ffm, . ~ . , length)
cast(ffm, treatment ~ ., length)
cast(ffm, .~ treatment, length)
cast(ffm, rep ~ treatment, length)
cast(ffm, treatment + rep ~ . , length)
cast(ffm, . ~ treatment + rep , length)

## High-dimensional arrays

Hd.Array <- cast(ffm, time ~ variable ~ treatment, mean)
dim(Hd.Array)

# The ~ operator is a type of "crossing" operator (like the Cartesian Product) because all combinations of the variables appear.
# The plus operator is NOT a crossing operator. Only the combinations that appear in the data will appear in the output.

### Lists ###
### The | operator creates lists. Multiple | operators creates lists with multiple levels of hierarchy ###
List <- cast(ffm, treatment ~ rep | variable,    mean)

###Tip: the list output is a useful input to lapply and sapply
cast.output <- cast(ffm, treatment ~ rep | time + variable, sum)
sapply(cast.output, length)