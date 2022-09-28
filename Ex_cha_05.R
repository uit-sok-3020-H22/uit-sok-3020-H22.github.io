# Chapter - 05 



rm(list=ls())

library(mosaic)

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
