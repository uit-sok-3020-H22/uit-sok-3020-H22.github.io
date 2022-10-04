

#' Chapter 6

rm(list=ls())

library(mosaic)

#' A sample of hamburger franchises in 75 cities from Big Andy's Burger Barn.

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/andy.def")

#' sales = S    Monthly sales revenue ($1000s)
#' price = P    A price index in $ for all products sold in a given month.
#' advert = A   Expenditure on advertising ($1000s)

#' Read the data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/andy.rdata"))
names(andy)


#' Example 6.1
#' Testing joint hypotheses, the F-test
#' Equation 6.2 Unrestricted Model, all variables
unrestricted <- lm(sales~price+advert+I(advert^2), data=andy) 
summary(unrestricted) 


#' Equation 6.3(?) Restricted model, remove advertising
#' The restriction is that, H0: b3 = b4 = 0, 5% level of significance
restricted <- lm(sales~price, data=andy) # restricted model, only price
summary(restricted)


anova(restricted,unrestricted) 

#' We reject H0, and conclude that advertising contribute in 
#' explaining the variance of sales
#' 

## Alternatively, without estimating the restricted model 

library(car)
linearHypothesis(unrestricted, c("advert=0","I(advert^2) = 0")) 




#' Testing the overall significance of a model : it is basically testing the 
#' relevance of all the included explanatory variables.  
#' 


#' 6.1.1 Testing the significance of the model (compare with
#'  model that only has an intercept)

onlyinterc <- lm(sales~1, data=andy) # that is our restricted model, equation 6.7
summary(onlyinterc)  

anova(onlyinterc,unrestricted) # F-test, equation 6.8

summary(unrestricted) # Same F-test as above
#' So comparing the two models by estimating both, 
#' is the same as the F-test in the summary of the 
#' unrestricted model

#' Same result using the linearHypothesis test 
linearHypothesis(unrestricted, c("price=0","advert=0","I(advert^2) = 0")) 

#################################


# More general F-test
#######################################

#' Example 6.5 Testing optimal advertising
#'  ---------------------------------------------
#' Is optimal advertising equal to $1900? p.267
#' ----------------------------------------------

#' H0: b3 + 2 b4 1.9 = 1, H1: b3 + 2 b4 1.9 != 1
#' Note that 2 x 1.9 = 3.8


#' Do the test directly on the unrestricted model!

#
linearHypothesis(unrestricted, "advert + 3.8*I(advert^2) = 1") # car


#' Joint Hypothesis, Example 6.7  (Complex Hypothesis, on page 268)
#' J=2 Complex hypotheses

#' H0: b3 + 3.8 b4 = 1,  b1 + 6 b2 + 1.9 b3 + 3.61 b4 = 80
#' 
#' H0: optimal level of advertising is $1900 and a price of $6 and
#'      advertising of $1900 will give sales of $ 80000



#' There are two ways of doing this.

#' One is writing the joint hypothesis as a matrix. 
#' Alternatively, We can do the same thing using variable names 

#' Using variable names, 
linearHypothesis(unrestricted, c("advert+3.8*I(advert^2)=1",
                                 "(Intercept)+6*price+1.9*advert+3.61*I(advert^2)=80"))
#' At a 5% level the H0 is rejected



#' Testing Nonlinear Hypothesis
#######################################

#' Optimal level of advertising, Example 6.8 A nonlinear hypothesis

#  Test The hypothesis: 
#' Ho: (1-beta_3)/2*beta_4 = 1.9,
#' H1: (1-beta_3)/2*beta_4 != 1.9   (rewrite it to have 0 on the RHS)

###### We cannot use F-test here, because it is not a linear hypothesis
m1 <- lm(sales~price+advert+I(advert^2), data=andy) 
# or 
andy <- andy %>% mutate(advert2 = advert^2)
m2 <- lm(sales~price+advert+advert2, data=andy) # Unrestricted model


deltaMethod(m2, "((1-advert)/(2*advert2))-1.9") # Non-linear combination of two parameters

#' Value under H0 (0) is inside 95%, hence we keep H0




#############################################################
#' 6.2 The use of non sample information
#' we have the beer data 
#######################################################


rm(list=ls())

# Obs:  30 annual observations from a single household
# 
# 1. q = litres of beer consumed (quantity demanded)
# 2. pb = Price of beer ($)
# 3. pl = price of other liquor ($)
# 4. pr = price of remaining goods and services (an index)
# 5. i = income ($)

#' Read the data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/beer.rdata"))
names(beer)

summary(beer)

#' To estimate the demand relationship, we need to assume about the functional form,
#' Assume, for this case, that the log-log functional form is appropriate. 

#' Unrestricted model eq. 6.17, log-log model
unrestricted <- lm(log(q)~log(pb)+log(pl)+log(pr)+log(i), data=beer)
summary(unrestricted)


# Economic theory says the sum of the coefficeint of the prices and income should sum to zero.
#' we can test whether this restriction hold in the data, we could do that even
#' without estimating the restricted model 

#'  F-test on the restriction on the Unrestricted model
linearHypothesis(unrestricted, "log(pb)+log(pl)+log(pr)+log(i) = 0") # car function

# Hence, we keep H0. 


#' Restricted model 6.20, under H0: b2+b3+b4+b5=0 
#'solve for pr, i.e., b4 = - b2 - b3 - b5 , and substitute into the unrestricted model 
restricted <- lm(log(q)~log(pb/pr)+log(pl/pr)+log(i/pr), data=beer) # the restricted model
summary(restricted)

c <- coef(restricted) ; c  # coeff. of that model (restricted model)
-c[2]-c[3]-c[4] # use the restriction under H0 to recover the removed b4 coefficient for pr


#' Using the deltaMethod from `car` on the Restriced model to recover
#' the coefficient on b4 and its standard error

library(car)
deltaMethod(restricted, "-b2-b3-b4", parameterNames= paste("b", 1:4, sep="")) # notice b4 here

dmt <- car::deltaMethod(restricted, "-b2-b3-b4", parameterNames= paste("b", 1:4, sep="")) 
names(dmt)

tvalue <- dmt$Estimate/dmt$SE
tvalue

#' p-value
2*pt(tvalue, df=df.residual(restricted), lower.tail=F)



###################################################################################

rm(list=ls())
library(mosaic)

#' Read the data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/rice5.rdata"))
names(rice5)
head(rice5)

#' Unrestricted model (using 1994 data)
mydata <-  filter(rice5, year==1994)

fit <- lm(log(prod) ~ log(area) + log(labor) + log(fert),data = mydata)
summary(fit)  #imprecise estimates, look at the coeffs of ln(AREA) and ln(LABOR)

confint(fit)  # And, the 95% interval estimates are very wide, and 
# because the coeffs of ln(AREA) and ln(LABOR) are not significantly different from zero,
# their interval estimates include a negative range

library(car)
vif(fit)  #And, high variance inflation factors points to collinearity as at issue for the imprecise estimates


#' Non-sample information
