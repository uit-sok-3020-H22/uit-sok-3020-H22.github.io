


# Chapter - 05 



rm(list=ls())

require(pacman)
p_load(rockchalk, mosaic)

#' A sample of hamburger franchises in 75 cities from Big Andy's Burger Barn.
#' 
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/andy.def")

#' sales = S    Monthly sales revenue ($1000s)
#' price = P    A price index ($) for all products sold in a given month.
#' advert = A   Expenditure on advertising ($1000s)

#' Read the data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/andy.rdata"))

head(andy)

#' Multiple regression model, Example 5.2
names(andy)

m1 <- lm(sales ~ price + advert, data = andy)
m1

coef(m1)

coef(m1)[1]
coef(m1)[2]
coef(m1)[3]

#General output 
summary(m1)


#Interpretation of the coefficients, intercept, etc.

#Interpretation of R2 , residual standard error, df, etc



# Interval estimation: 

#  1. confidence interval on the coefficients of the regression  
confint(m1)
confint(m1, level = 0.90)


# 2. Interval estimation for a linear combination of coefficients 
# Find the 95% interval estimate of -0.4*price + 0.8*advert   ---- linear combinatio of two paramters 
library(car)   # remember whenever you see a linear combination of parameters of a regression model
deltaMethod(m1, "-0.4*price +0.8*advert", level = 0.9)


# 3.  Prediction: prediction of the mean (called interval estimation) and prediction of individual values 
#' A CI reports the interval estimate for the mean value of y  for a given value of x

# predict the expected sale when price=5.5, and advert = 1.2
E(sales/price, advert)=118.91 -7.91*price + 1.86*advert

#E(sales/price =5.5, advert = 1.2) = 
118.9136 -7.91*5.5 + 1.86*1.2

# Alternatively 
f <- makeFun(m1)

f(5.5, 1.2)

f(price = 5.5, advert = 1.2)


# prediction with confidence and prediction intervals 

f(price = 5.5, advert = 1.2, interval = "confidence", level = 0.95)

f(price = 5.5, advert = 1.2, interval = "prediction", level = 0.95)


#plot prediction interval 

plotSlopes(m1, plotx = "price", interval="prediction", col="red") # in the dir of price
plotSlopes(m1, plotx = "advert", interval="prediction", col="red") # in the dir of advert



# Hypothesis testing 
###########################################

#  a).  Testing the significance of a single coefficient


# Two tail test 
#-----------------------------
 
# H0: beta_k =0 against H1: beta_k ! =0   

# For example H0: beta_2 =0 against H1: beta_2 ! =0
        
# calculate t-static and compare with t_c , (remember to adjust the degree of freedom)

# Alternatively 
summary(m1)   # just look at the p-values (Two tail test)


#H0: beta_3 =0 against H1: beta_3 ! =0

summary(m1)  # just look at the p-values


# More example on two sided test 

 # Test  #H0: beta_3 (advertising)= 1 against H1: beta_3  !=0

summary(glht(m1, linfct = c("advert =0"))) #Two sided test


# One tail test 
################################
# Test the hypothesis 
#H0: beta_2 >= 0 against H1: beta_2  < 0

# Just use the glht() function from the multcomp package
library(multcomp)
summary(glht(m1, linfct = c("price >=0")))   # One sides test 


# b) Hypothesis testing for a linear combination of coefficients 

# Test Ho:-0.2*B2 - 0.5*B3 <=0 vs H1:-0.2*B2 - 0.5*B3 > 0

summary(glht(m1, linfct = c("-0.2*price-0.5*advert <=0"))) 


# Polynomial equations or Nonlinear Relationships 
############################################################3
#Estimate: sales ~ price + advert + advert^2

# Two ways to estimate:

# Directly, without creating a new variable advert^2

m2 <- lm(sales ~price + advert + I(advert^2), data = andy)
summary(m2)

# Create a variable advert^2 

andy %>% mutate(advert2 = advert^2)

andy <-andy %>% mutate(advert2 = advert^2)

m3 <- lm(sales ~ price + advert + advert2, data = andy)
summary(m3)

