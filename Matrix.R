# TODO: Add comment
# 
# Author: OGOREKB
###############################################################################
library(Matrix)

matrix(10+ 1:28, 4, 7)

M <- Matrix(10 + 1:28, 4, 7)

#transpose
tM <- t(M) # is a generic function, but I can see that it is defined especially for type "Matrix" by using showMethods("t")

(M2 <-cBind(-1, M)) # parentheses to get things to print out in one step

# add zeros to sparsify
M2[, c(2, 4:6)] <- 0  # zero out four columns
M2[2, ] <- 0 # zero out second row
M2 <- rBind(0, M2, 0) # add two more zero rows
M2[1:2, 2] <- M2[3,4:5] <- NA  # add a few NAs to the mix

object.size(M2)

sparseM <- as(M2, "sparseMatrix")  # of class "dgCMatrix"
object.size(sparseM)   # wierd...it is actually a few byte more.

getSlots("dgCMatrix") # x is a numeric vector that holds the non-zero values
str(sparseM)

## I think I can recreate the matrix with just these columns ##
sparseM@Dim # matrix dimensions
sparseM@x   # non-zero entries
sparseM@i	# zero-based row indicies where elements are placed
sparseM@p   # zero-based index of elements to be used in each column
###################################################################

# using the new() constructor
rebuild <- new("dgCMatrix", i = sparseM@i, p = sparseM@p, Dim = as.integer(c(6,8)), x =sparseM@x)
identical(sparseM, rebuild)

# actually, the sparseMatrix() function is recommended
# notice that i is a 1-based array, and column array j could have been used instead of column pointer p
rebuild.2 = sparseMatrix(i = (sparseM@i + 1), p = sparseM@p, dims = as.integer(c(6,8)), x =sparseM@x)
identical(sparseM, rebuild.2)

identical(as(M2*M2, "sparseMatrix"), rebuild.2 * rebuild.2)

M2*M2
rebuild.2 * rebuild.2


sparseM # keeps NAs, but removes zeros
10 * sparseM

print(image(sparseM))
dev.off()


