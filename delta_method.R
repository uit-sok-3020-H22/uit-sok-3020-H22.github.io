

#' deltamethod()

#' In this R code, I have shown how to use 
#' the deltamethod() function from the car package 
#' to construct an interval estimate for the expected value 
#' of the dependent variable and elasticity. Also, I have 
#' shown how to perform hypothesis testing using the 
#' deltamethod() 
#' 
rm(list=ls())

library(mosaic)

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/food.def")

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/food.rdata"))

head(food)
#' Estimating the parameters of the simple linear regression model in Chapter 2
names(food)

m1 <- lm(food_exp~income, data=food)
m1

#' A more general output from the model is given by
summary(m1)

#' The fitted regression
plotModel(m1) 

# interval estimation 
f <- makeFun(m1)
f(20, interval="confidence", level=0.95)



# Hypothesis testing 

# E(food_exp | income=20) = b1+b2*20 

# H0: b1+b2*20 <= 250, H1: b1+b2*20 > 250 

# Rewrite: H0: b1+b2*20 -250 <= 0, H1: b1+b2*20 -250 > 0 

# Use deltaMethod in car package (The delta method "car" package)
#install.packages("car")
library(car)
?deltaMethod

deltaMethod(m1, "Intercept+20*income-250") 

# more convenient than names, use parameter names
deltaMethod(m1, "b1+20*b2-250", parameterNames= paste("b", 1:2, sep="")) 

dmt <- deltaMethod(m1, "b1+20*b2-250", parameterNames= paste("b", 1:2, sep="")) 
names(dmt)
# t-value
dmt$Estimate/dmt$SE

# critical t, alphe=0.05
qt(0.95, 38)  #df = N-2 degree of freedom 

# compare 
dmt$Estimate/dmt$SE > qt(0.95, 38)
#conclusion: reject H0


#  Estimate the elasticity of expected food exp.
els <- function(x,y){coef(m1)[2]*x/y}

# The sample mean of income and food_exp
mx <- mean(food$income)
my <- mean(food$food_exp)

els(mx, my)

# A 95% CI
deltaMethod(m1, 
            "b2*mx/my", 
            parameterNames=paste("b", 1:2, sep=""),
            level=0.95)

# Hypothesis testing 
# Test the null hypothesis that the elasticity, evaluated
# at the sample mean of income and food_exp is one against the alt. that the 
# elast is not one.
#  H0:mx/my=1 vs H1:mx/my=1
# Rewrite: H0:mx/my-1 vs H1:mx/my-1

deltaMethod(m1, 
            "b2*mx/my-1", 
            parameterNames=paste("b", 1:2, sep=""),
            level=0.95)


dm_t <- deltaMethod(m1, 
                   "b2*mx/my-1", 
                   parameterNames=paste("b", 1:2, sep=""),
                   level=0.95) 
# t-value
dm_t$Estimate/dm_t$SE

# critical t, alphe=0.05
qt(0.95, 38)  #df = N-2 degree of freedom 

# compare 
dm_t$Estimate/dm_t$SE > qt(0.95, 38)
#conclusion: Fail to reject H0




