

#' Some Exercises from chapter o4

rm(list=ls())
suppressPackageStartupMessages(library(mosaic))

#' browseURL("http://www.principlesofeconometrics.com/poe5/data/def/newbroiler.def")
#' 
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/newbroiler.rdata"))
head(newbroiler)


#'  Units of the variables:
#'  
#'   p - price (no unit), just price index
#'
#'   q - per capita consumption (pound)



#'  Model 1: lin-lin model 

m1 <-  lm(q ~ p, data = newbroiler)

coef(m1)
coef(m1)[2]

#' more general output 
summary(m1)

#' Interprate the parameters:
#' 
#' Intercept: 
#' 
#' 57.956: The expected level of per capita consumption when price is zero.
#' 
#' Slope: 
#'  
#' -18.403: A 1 unit increase in price leads to (approximately ) 18.40 pound decrease 
#'          in the level of expected/average/ per capita consumption q.
 
#' Interprate R^2:
#' 
#'  71.05% of variation in per capita consumption is explained by the variation in price.
#'                : the remaining 29% is the unexplained part 
 

plotModel(m1)

#' confidence interval of the parameter 
confint(m1)

#'  Prediction: Predict the E(q|p) at a given value of p, p= 0.98
#'  
#' E(q|p = 0.98)
f <- makeFun(m1)
f(0.98)
f(p = 0.98)
f(p = 0.98, interval = "confidence", level = 0.95)




#' Model 2: lin-log


m2 <- lm(q ~ log(p), data = newbroiler)
summary(m2)

#' Interprate the parameters: 
#' 
coef(m2)[2]/100

#' -0.319078 : a 1% increase in price, leads to (approximately) 0.319 pound decrease in the level of per capita consumption q
#'
#' Interprate R^2: 
#' 
#'     81.38% of variation in per capita consumption is explained by the variation in log price. 


#' confidence interval of the parameter 
#' 
confint(m2)

#'  Prediction: Predict the E(q|p) at a given value of p, p= 0.98
#'  
#' E(q|p = 0.98)

#' Manually:
coef(m2)[1]-(coef(m2)[2]*log(0.98))

#' more simply 
f <- makeFun(m2)
f(0.98)
f(p = 0.98)
f(p = 0.98, interval = "confidence", level = 0.95)





#' Model 3: log- linear model 



m3 <- lm(log(q) ~ p, data = newbroiler)
summary(m3)

#' Interpretation the parameters:  
100*coef(m3)[2] 

#' a 1 unit increase in price leads to (approximately) a 66.4 %  decrease in q.

#' Prediction in the log -linear model:
#' 
#' E(q|p) at a given value of p, p= 0.98 (mean of p)
#' 
#' E(q|p = 0.98)

#' Manually 
coef(m3)[1]+coef(m3)[2]*0.98
exp(coef(m3)[1]+coef(m3)[2]*0.98)

yn <- function(x){exp(coef(m3)[1]+coef(m3)[2]*x)}  # not corrected 
yn(0.98) # not corrected 

#' Alternatively 
y <- makeFun(m3) 
y(0.98)  # predict q for the uncorrected depe.var.

#' However, there is one problem here. what is that?
#' Since the dep. var. is in log form, we have log transformed error /residual terms. 
#' That  means e = log(q) - b1 -p
#' We have to adjust that. How? Look at your book on page (175 -178 

s2 <- deviance(m3)/m3$df.residual   # The variance in the model, sigma squared 

yc <- function(x){exp(coef(m3)[1]+coef(m3)[2]*x+s2/2)}  # corrected 
yc(0.98)  # corrected 

#' Alternatively, using the mosaic::makeFun() 
y(0.98)*exp(s2/2) # must adjust 

#' Remember: the mosaic::makeFun() function predict the dependent variable in levels but w/o se correction 
           # we have to adjsut



#' we have estimated three different models. 
#' Now let us compare the three models in terms of R^2 values, i.e., which model has the highest R^2 value. 
#' The best model is the one with the highest R^2 value.  

#' R^2 from model 1
summary(m1)$r.squared
#' R^2 from model 2
summary(m2)$r.squared

#' R^2 from model 3 
summary(m3)$r.squared   
#' But this is explained variation in the log dep. variable. 
#' That means we have the explained variation in log form of the dep.variable 
#' Change to explained variation in dep. variable in levels. How?


#' A generalized R^2 measure (page 176): comparing two models, y = and log(y)=
cor(newbroiler$q, yc(newbroiler$p))
cor(newbroiler$q, yc(newbroiler$p))^2

#' Alternatively 
cor(newbroiler$q, y(newbroiler$p)*exp(s2/2))^2

