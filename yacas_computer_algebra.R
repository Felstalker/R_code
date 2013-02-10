# TODO: Add comment
# 
# Author: OGOREKB
###############################################################################

# had to install yacas by using an older Windows executable #
# Then I copied files within "winrel" and pasted them into the R yacas folder "yacdir" #
library(Ryacas)

expr1 <- yacas(expression(Factor(x^2 - 1)))
Eval(expr1, list(x = 4))
class(expr1)

expr2 <- yacas("D(x)Sin(x)")
expr2
class(expr2)
names(expr2)
expr2$OMForm # Hmm, XML, huh?

# both expr1 and expr2 are "yacas" objects #

getSlots("yacas") #error. This is an S3 class.

xs <- Sym("xs") # convention to put an "s" at the end of the R object
class(x) #now we have the sym class which appears to extend character

x + 4

Integrate(sin(x), x)

expr <- Solve(xs^(-1)  == 1, xs)
expr2 <- Newton(xs^(-1)- 1, xs, .5, .0001)

