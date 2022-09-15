rm(list=ls())

library(mosaic)

#' Data set on food expenditure and weekly income from a random sample of 40 households.
#' Data definition file: <http://www.principlesofeconometrics.com/poe5/data/def/food.def>
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/food.def")

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/food.rdata"))

head(food)
str(food)

#' Estimating the parameters of the simple linear regression model
fit <- lm(food_exp~income, data=food)
summary(fit)


#A 95% confidence interval is calculated ad follows:
confint(fit, level = 0.95)



# Hypothesis testing 

#' Example 3.2, p.123, here we specify a one-tail right-test,
#'  H0: b2=0, H1: b2>0

library(multcomp)
summary(glht(fit, linfct = c("income = 0"))) #' specify the H0 using linfct=

#compare t-calculated with the critical
summary(glht(fit, linfct = c("income = 0")))$test$tstat > qt(0.95, 38)
# conclusion: Reject H0

#' Example 3.3, p. 124, here we specify a one-tail test,
#'  H0: b2<=5.5, H1: b2>5.5
summary(glht(fit, linfct = c("income <= 5.5"))) 

# compare t-calculated with the critical
summary(glht(fit, linfct = c("income <= 5.5")))$test$tstat > qt(0.99, 38)
# conclusion: keep H0

#' Example 3.4, p.125, here we specify a one-tail test,
#'  H0: b2>=15, H1: b2<15
summary(glht(fit, linfct = c("income >= 15"))) 
# calculated t larger than critical?
summary(glht(fit, linfct = c("income >= 15")))$test$tstat < qt(0.05, 38)
# conclusion: reject H0

#' Example 3.5, p.125, two-tail test, here we specify
#'  a two-tail test, H0: b2=7.5, H1: b2!=7.5
summary(glht(fit, linfct = c("income = 7.5")))
# compare t and t_c
summary(glht(fit, linfct = c("income = 7.5")))$test$tstat > abs(qt(c(0.025,0.975), 38))
#Conclusion: do not reject H0

# Example 3.6
summary(glht(fit, linfct = c("income = 0")))

summary(glht(fit, linfct = c("income = 0")))$test$tstat > abs(qt(c(0.025,0.975), 38))


summary(fit)
# The 100(1-alpha)% confidence interval, any value of H0: b2=c inside
#this interval, we keep H0.
confint(fit, level = 0.95)



##############################################
# The p-value rule:
# if p-value <= alpha (level of significance) reject H0
# often used, more often misused
##############################################

#' Example 3.3, p. 124, here we specify a one-tail test,
#'  H0: b2<=5.5, H1: b2>5.5
summary(glht(fit, linfct = c("income <= 5.5"))) 


#' Example 3.4, p.125, here we specify a one-tail test,
#'  H0: b2>=15, H1: b2<15
summary(glht(fit, linfct = c("income >= 15"))) 


#' Example 3.5, p.125, two-tail test, here we specify
#'  a two-tail test, H0: b2=7.5, H1: b2!=7.5
summary(glht(fit, linfct = c("income = 7.5")))


# Example 3.6
summary(glht(fit, linfct = c("income = 0")))
#alternative approach 
linearHypothesis(fit, "income = 0") # two-tail test
# t-value, with one df, the t^2 = F


###################################################################
###################################################################
####################################################################

#' Example 3.7 : Estimating Expected Food Expenditure.
#'  
# Predict Expected Values from the regression model
#' Prediction at income=20

# Base R
predict.lm(fit,newdata = data.frame(income=20))

f <- makeFun(fit)
f(income=20)
f(20)

#'  Example 3.8: An Interval Estimates of Expected
#' Food Expenditure,  p. 131
f(20, interval="confidence")
f(20, interval="confidence", level=0.9)

library(pacman)
p_load(rockchalk)

predictOMatic(fit)
predictOMatic(fit, predVals = list(income=20))
predictOMatic(fit, predVals = list(income=20), interval="confidence")
predictOMatic(fit, predVals = list(income=20), interval="prediction")

plotSlopes(fit, plotx = "income", interval = "conf")
plotSlopes(fit, plotx = "income", interval = "conf", opacity = 80, col="red")


# Example 3.9, Linear combination of two parameters, p. 132

# E(food_exp | income=20) = b1+b2*20 
# H0: b1+b2*20 <= 250, H1: b1+b2*20 > 250 

# Rewrite: H0: b1+b2*20 -250 <= 0, H1: b1+b2*20 -250 > 0 

# Use deltaMethod in car package (The delta method "car" package)
#install.packages("car")
library(car)
deltaMethod(fit, "Intercept+20*income-250") 

# more convenient than names, use parameter names
deltaMethod(fit, "b1+20*b2-250", parameterNames= paste("b", 1:2, sep="")) 

dmt <- deltaMethod(fit, "b1+20*b2-250", parameterNames= paste("b", 1:2, sep="")) 
# t-value
dmt$Estimate/dmt$SE
# critical t, alphe=0.05
qt(0.95, 38)

# compare 
dmt$Estimate/dmt$SE > qt(0.95, 38)
#conclusion: reject H0

##########################################################

#' Trick to use glht, with hypothesis involving (Intercept), 
#' create own intercept, call it "one"
head(model.matrix(fit))
food <- food %>% mutate(one=1)
head(food)

# remove default (Intercept) with 0, replace it with one
fit2 <- lm(food_exp~0+one+income, data=food)
summary(fit) # original model
summary(fit2) # identical, but (Intercept) replaced with variable the variable we called one

head(model.matrix(fit2))

# now we can use glht, one sided test, specify H0
summary(glht(fit2, linfct = c("one+20*income <= 250")))

# is by default a two-tail test, divide p-value/2 to get 1-tail p-value
linearHypothesis(fit, "(Intercept)+20*income -250 = 0")

lht <- linearHypothesis(fit, "(Intercept)+20*income -250 = 0")
lht$`Pr(>F)`[2]/2
