rm(list = ls())
library(qcc)
set.seed(123)

mu.a1b1 <- 30.5
mu.a1b2 <- 31
mu.a2b1 <- 40
mu.a2b2 <- 42

n.a1b1 <- 25  #5
n.a1b2 <- 25  #45
n.a2b1 <- 25  #30
n.a2b2 <- 25  #20

n <- n.a1b1 + n.a2b1 + n.a1b2 + n.a2b2

test.data <- data.frame(group = 
                        c(rep("a1b1", n.a1b1), 
                          rep("a1b2", n.a1b2),
                          rep("a2b1", n.a2b1),
                          rep("a2b2", n.a2b2)),
                        y = 
                        c(rnorm(n.a1b1, mean=mu.a1b1), 
                        rnorm(n.a1b2, mean=mu.a1b2),
                        rnorm(n.a2b1, mean=mu.a2b1),
                        rnorm(n.a2b2, mean=mu.a2b2))
)
test.data$y <- round(test.data$y, 1)

my.qcc <- qcc(test.data$y, type = "xbar.one", add.stats=FALSE)

cat("Mean of y ", mean(test.data$y),  "Variance of y ", var(test.data$y))
cat("Sums of Squares of y ", var(test.data$y)*(n-1))
    
boxplot(y~group, data = test.data, main = "one way layout")

test.data$a <- factor(substr(test.data$group, 1,2))
test.data$b <- factor(substr(test.data$group, 3,4))

interaction.plot(test.data$a,  test.data$b,test.data$y)

tbl <- table(test.data$a, test.data$b)
tbl
plot(tbl)

# SAS-type options for a Type III analysis
options(contrasts=c("contr.sum","contr.poly")) 

anova <- aov(y ~ a + b + a:b, data = test.data)
results <- drop1(anova, scope = ~., test = "F")

results
#http://stats.stackexchange.com/questions/23197/type-iii-sum-of-squares-from-sas-and-r

2685.31 + 52.42 + 16.48 + 81.38

1396.32 + 23.58 + 13.74 + 80.70


