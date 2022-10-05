rm(list=ls())
library(mosaic)

#' Read the data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/rice5.rdata"))
names(rice5)
head(rice5)

#' Unrestricted model (using 1994 data)
fit <- lm(log(prod) ~ log(area) + log(labor) + log(fert), data = filter(rice5, year==1994))
summary(fit)  #imprecise estimates, look at the coeffs of ln(AREA) and ln(LABOR)

confint(fit)  # And, the 95% interval estimates are very wide, and 
# because the coeffs of ln(AREA) and ln(LABOR) are not significantly different from zero,
# their interval estimates include a negative range

library(car)
vif(fit)  #And, high variance inflation factors points to collinearity as at issue for the imprecise estimates

cor(cbind(rice5$area, rice5$fert,rice5$labor))

#' Solution to mitigate multicllinearity:
#'  1) Use non-sample information 
#'  2). Use more data

#' Non-sample information

#' Constant returns (restriction) to scale 1994 data
#' H0: b2 + b3 + b4 = 1, solve for area
#' 

linearHypothesis(fit, "log(area) + log(labor) + log(fert)=1")

# solve for area 

fit2 <- lm(log(prod/area)~log(labor/area)+log(fert/area), data = filter(rice5, year==1994))
summary(fit2)

vif(fit2)

#' Recovering area parameter in constant returns model, from restriction under H0
#--------------------------------------------
c <- coef(fit2);c
1-c[2]-c[3]

# using the deltaMethod
deltaMethod(fit2, "1-b2-b3", parameterNames= paste("b", 1:3, sep="")) 

#' Estimate this model directly by using fitModel() function
g <- fitModel(log(prod) ~ b1 + (1-b3-b4)*log(area) + b3*log(labor) + b4*log(fert), data = filter(rice5, year==1994))
summary(g)
summary(fit2)


#' Data from 1993 and 1994
fit3 <- lm(log(prod) ~ log(area) + log(labor) + log(fert), data = filter(rice5, year %in% c(1993,1994)))
summary(fit3)

confint(fit3)

vif(fit3)

#' Constant returns to scale 1993 and 1994
fit4 <- lm(log(prod/area)~log(labor/area)+log(fert/area), data = filter(rice5, year %in% c(1993,1994)))
summary(fit4)

vif(fit4)

#' Recovering area parameter in constant returns model
deltaMethod(fit4, "1-b2-b3", parameterNames= paste("b", 1:3, sep="")) 

