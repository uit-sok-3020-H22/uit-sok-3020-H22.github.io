

#' Chapter 7 - Using Indicator Variables
#' 
#' 
#' Indicator variables are used to account for qualitative factors 
#' (e.g.,gender, location, ) in econometrics models. 
#' They are often called dummy, binary, or dichotomous variables
#' because they take just two values, usually 1 or 0, to indicate 
#' whether a condition is true or false.


library(mosaic)

#' Data definition
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/utown.def")
#' Load data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/utown.rdata"))
#' Table 7.1
head(utown)

# price	house price, in $1000
# sqft		square feet of living area, in 100's


fit1 <- lm(price ~ sqft, data = utown)
summary(fit1)

#' Interpretation: 
#' b2 is the value of an additional square foot of living area, and 
#' b1 is the value of the land 


#Intercept indicator variables
####################################

#' Now let us add the effect of location on the house price 
#' by including intercept dummy.
#' The effect of the inclusion of an indicator variable D into 
#' the regression model is best seen by examining the regression function, 
#' E(Price|sqft), in the two locations.
fit2 <- lm(price ~ sqft+utown, data = utown)
summary(fit2)   

#' The coeff of utown capture a location premium, 
#' the difference in house price due to houses being 
#' located in the desirable neighborhood (close to the univ.).
#' We expect the price to be higher in a desirable location (close to the university), and 
#'  thus we anticipated that coeff. of utown will be positive and significant.


f <- makeFun(fit2)
#' The intercept, the expected price, when all explanatory 
#' variables are zero.
#' E(price|utown=0,sqft=0)
f(utown=0,sqft=0)

#' The location premium for lots near the university,
#'  calculated as a difference between expected values
f(utown=1,sqft=0)-f(utown=0,sqft=0)

confint(fit2)


#Slop-indicator variables
##########################################
#' Instead of assuming the effect of location on house 
#' price causes a change in the intercept of our model, 
#' let us assume that the change is in the slope of the relationship. 
#' We can allow for the a change in a slope by including in the
#'  model an additional explanatory variable that is equal 
#'  to the product of an indicator variable and 
#' a continuous variable. 

fit3 <- lm(price ~ sqft+I(sqft*utown), data = utown)
summary(fit3)

#' The coef of the interaction variable captures the interaction 
#' effects of location and size on house price.


#' If we assume that house location affects both the 
#' intercept and the slope, then both effects can be 
#' incorporated into a single model 
fit4 <- lm(price ~ sqft+utown+I(sqft*utown), data = utown)
summary(fit4)


#' Now let us include other variables into our model 
#' 
# age		house age, in years
# pool		=1 if house has pool
# fplace	=1 if house has fireplace

fit <- lm(price~utown+sqft+I(utown*sqft)+age+pool+fplace, data = utown)
summary(fit)

#' Note that pool and fplace are intercept dummy variables.
#' By introducing these variables we are asking whether, and by
#'  how much, these features change house price. 
#'  Because these variables stand alone, and are not
#' interacted with sqft or age, we are assuming that they affect 
#' the regression intercept, but not the slope.

f <- makeFun(fit)

#' The intercept, the expected price, when all explanatory 
#' variables are zero.
#' E(price|utown=0,sqft=0,age=0,pool=0,fplace=0)
f(utown=0,sqft=0,age=0,pool=0,fplace=0)

#' The location premium for lots near the university,
#'  calculated as a difference between expected values
f(utown=1,sqft=0,age=0,pool=0,fplace=0)-f(utown=0,sqft=0,age=0,pool=0,fplace=0)
#' aka. the coefficient on utown
coef(fit)[2]

#' The change in expected price per additional square foot is
(coef(fit)[3]+coef(fit)[4])*10    # 1unit(100sqft)= [coef(fit)[3]+coef(fit)]*$1000 =>1sqft= $89.1
#' $ for houses near the university and
coef(fit)[3]*10
#' $ for houses in other areas.

#' Houses depreciate by 
coef(fit)[5]*1000
#' $ per year

#' A pool increases the value of a home by $
coef(fit)[6]*1000

#' A fireplace increases the value by $
coef(fit)[7]*1000

#' A house near the university, of 25000 square feet, 
#' being 10 years old,
#' with no pool and fireplace is sold for $
f(utown=1,sqft=250,age=10,pool=0,fplace=0)*1000 #sqft=25 should be 250

# --------------------------------------------------



#'  Example 7.2 The Effects of Race and Sex on Wage
#####################################################

#' The model is:
#'wage= b1 + b2 educ + b3 black + b4 female + b5 black*female +e

# Notice here black is a dummy variable, and also female 
#Now we have interaction of the dummies in our model.

#' When multiple dummy variables are present, and especially when there are
#' interactions b/n indicator variables,it is important for proper 
#' interpretation to write out the regression function, E(wage|educ),for 
#' each indicator variable combination. 

rm(list=ls())

library(mosaic)

#' Data definition
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/cps5_small.def")

#' Load data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/cps5_small.rdata"))
head(cps5_small)
noquote(names(cps5_small))

cps5_small <- cps5_small %>% mutate(black.female=black*female)

fit <- lm(wage ~ educ+black+female+black.female, data = cps5_small)
#' Table 7.3
summary(fit)

#' Holding the effect of education (educ) constant, a black male earn
coef(fit)[3]
#' per hour less than a white male, white females earn
coef(fit)[4]
#' per hour less than a white male, and black females earn
coef(fit)[3]+coef(fit)[4]+coef(fit)[5]
#' per hour less than a white male.

#'Suppose we are asked to test the joint significance of 
#'all the qualitative factors. 
#'
#'How do we test the hypothesis that neither a person's race nor sex
#'affects wage?
#'We do it by testing the joint hypothesis test:

#' 3-joint hypothesis, from chapter 6.7
#' H0: b3 = 0, and
#' H0: b4 = 0, and
#' H0: b5 = 0

library(car)

Hypothesis <- matrix(c(0,0,1,0,0,
                       0,0,0,1,0,
                       0,0,0,0,1), 3, 5, byrow=TRUE)
RHS <- c(0,0,0)

colnames(Hypothesis) <- c("b1", "b2", "b3", "b4", "b5")
rownames(Hypothesis) <- c("eqtn 1", "eqtn 2", "eqtn 3")
Hypothesis

linearHypothesis(fit, Hypothesis, rhs=RHS)
#' Thus we conclude that a workers race and/or sex affect 
#' the wage equation (jointly)

#' Alternatively 
linearHypothesis(fit, c("black=0", "female=0", "black.female=0"))



#' Example 7.5 Indicator Variables in Log-Linear Models

fit <- lm(log(wage)~educ+female, data=cps5_small)
summary(fit)

#' What is the interpretation of the parameter?
#' 
#' Female is an intercept dummy variable, creating a parallel shift of the 
#' log-linear relationship when Female=1.
#' The depe. variable is in log(wage), and have an effect in interpretation. 

#' Approximate calculation:
#' There is a 17.78% differential b/n
#' male and female wages.This is quick and simple.

#' Exact calculation;
#' The percentage difference b/n wages of female and males is 
#'  16.29%
100*(exp(coef(fit)[3])-1)




