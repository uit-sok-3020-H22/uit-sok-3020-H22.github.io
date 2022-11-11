
rm(list=ls())

library(mosaic)
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/rice5.def")

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/rice5.rdata"))


# firm -	Firm number  ( 1 to 44)
# year - Year = 1993 to 1994
# prod - Rice production (tonnes)
# area - Area planted to rice (hectares)
# labor -	Hired + family labor (person days)
# fert -	Fertilizer applied (kilograms)

fit <- lm(log(prod) ~ log(area) +log(labor)+ log(fert),data = rice5)
summary(fit)

# log(area)
round(coef(fit)[2],2)

round(coef(fit)[2]*5,2)

# log(labor)
round(coef(fit)[3],2)
round(coef(fit)[3]*-3,2)

# Section 4

# 90% CI, for a 10% increase in labor 
confint(fit,level = 0.9)[3,]
10*round(confint(fit,level = 0.9)[3,],3)

# section 5

summary(fit)$r.squared
round(summary(fit)$r.squared,3)
# or 
library(broom)
stat <- glance(fit)
round(stat$r.squared,3)

# section 6

#' A generalized $R^{2}$ measure
f=makeFun(fit)
cor(f(rice5$area,rice5$labor,rice5$fert), rice5$prod)^2

# With the correction 
s2 <- deviance(fit)/fit$df.residual

cor(f(rice5$area,rice5$labor,rice5$fert)*exp(s2/2), rice5$prod)^2

#--------------------------------------------------------
#' Predictions in the log-linear model
#' yc <- function(x1,x2,x3) {exp(coef(fit)[1]+coef(fit)[2]*log(x1) +
#'           coef(fit)[3]*log(x2) +coef(fit)[4]*log(x3)+s2/2)} # corrected
#' yc=yc(rice5$area,rice5$labor,rice5$fert)

#' cor(rice5$prod,yc)
#' cor(rice5$prod,yc)^2
# round it 
#' round(cor(rice5$prod,yc)^2,3)
#' summary(fit)$r.squared
#'----------------------------------------------------------------

# The correction factor is:
exp(s2/2)
#round to 3 digits
round(exp(s2/2),3)

# Section 7

# 90% prediction interval 
f=makeFun(fit)
f(area=2,labor=150,fert=200, interval = "prediction")
# corrected for the log-normal
f(area=2,labor=150,fert=200, interval = "prediction")*exp(s2/2)
#rounding 
round(f(area=2,labor=150,fert=200, interval = "prediction")*exp(s2/2),2)


# section 8

fit
library(car)
my=mean(rice5$prod)
mx=mean(rice5$area)
m=my/mx
round(deltaMethod(fit, "m*b2", parameterNames=paste("b", 1:4, sep=""),level=0.95),2)


# Section 9

# H0:b2 >=0.5 vs H1: b2 < 0.5

rice5 <- mutate(rice5,
                lnprod=log(prod),
                lnarea=log(area),
                lnlabor=log(labor),
                lnfert=log(fert))

fit2 = lm(lnprod ~ lnarea +lnlabor + lnfert, data = rice5)
summary(fit2)
summary(fit)

library(multcomp)

summary(glht(fit2, linfct = c("lnarea >=0.5"))) # one tail test 
a=summary(glht(fit2, linfct = c("lnarea >=0.5"))) # one tail test 

# the calculated test value 
a$test$tstat

#The critical test value
qt(0.95,length(rice5$prod)-4)


#' Section 10

#H0: constant return to Scale (CRTS)
summary(glht(fit2, linfct = c("lnarea+lnlabor+lnfert=1")))

#We keep the restriction by looking at the p-value 

# Re-estimate the model with CRTS imposed on it

r.m1 = lm(log(prod/fert) ~ log(area/fert) +log(labor/fert),data = rice5)
summary(r.m1)

#' Using the deltaMethod from car on the Restricted model to recover
#' the coefficient on b4 and its standard error:

deltaMethod(r.m1, "1-b2-b3", parameterNames = paste("b",1:3,sep = ""))


#------------------------------------------------------------------
# OR

# Directly with the restriction specified 

g <- fitModel(log(prod) ~ b0 +b1*log(area)+b2*log(labor)+(1-b1-b2)*log(fert),data=rice5)

summary(g)

c=coef(g)
c

1-c[2]-c[3] # recover the removed coefficient


test = summary(g)
varcovg = test$cov.unscaled*test$sigma^2 #vcov
D <- c(0,1,1)

lambda.se <- sqrt(t(D) %*% D) # std. Error of the removed 
# coefficeint from var-cov matrix
lambda.se
#-----------------------------------------------------------------



# The time series part 

#' Section 12

rm(list = ls())
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/shiller.rdata"))
head(shiller)

#' Create a time series object of the variables
p=ts(shiller$price, start=c(1871,1), end=c(2015,9), frequency=12)
div=ts(shiller$div,start=c(1871,1), end=c(2015,9), frequency=12)

#' plot the series to see how they are trending
plot(p)
plot(div)

#' Unit root in levels
#' Check stationarity of the variables with only drift terms.
library(urca)
summary(ur.df(p, type = "drift",  lags = 12, selectlags = "BIC"))
summary(ur.df(div, type = "drift", lags = 12, selectlags = "BIC"))


#' Section 13

library(dynlm)
summary(dynlm(d(p) ~ L(p)+d(L(p,1))))
summary(dynlm(d(div) ~ L(div)+d(L(div,1:4))))


#' Section 14 

#' First difference
summary(ur.df(diff(p), type = "none",  lags = 0))
summary(ur.df(diff(div), type ="none", lags = 0))


#' Section 15

#'Cointegration test
model_step1=dynlm(div~p)
summary(model_step1)

#' Extract the residual, and estimate the residuals using OLS 
res=resid(model_step1)
model_resd=dynlm(d(res)~0+L(res)+L(d(res),1))
summary (model_resd)

#' Section 17 and 19
#' Ignore these sections, they appear due mistake 


#' Section 19 

#' Error correction model
Ecm=dynlm(diff(div)~L(res,1)+L(diff(div),1:3)+diff(p)+L(diff(p),1:3))
summary(Ecm)

#' Our estimates results show that the error correction coefficient is equal to 
 round(coef(Ecm)[2],2) # and it is significant at the 5%. The negative sign of 
 #the error correction coefficient indicates the presence of error correction (or adjustment) 
 #' towards the cointegrating relationship between p and div. The estimated value of the coefficient 
 #' of the error correction term indicates that the monthly adjustment of div_t 1% of the deviation of div_t-1.
 #' This is a slow rate of adjustment. The value of estimated coefficient of the current level of 
 #' pt is  0.0335791 and it is significant at 5% level. The value of the R^2 is 0.3457. 



#' Section 20

#Autocorrelation test
#Lagrnage multiplier(Lm)/Breusch-Godfrey test for seral correlation
# H0: No autocorrelation
library(lmtest)
bgtest(ECM) 
bgtest(ECM, order=2)
bgtest(ECM, order=3)



