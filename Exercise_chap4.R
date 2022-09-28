
# 4.16

rm(list=ls())

library(mosaic)
library(tidyverse)

#' The data defenition file: 
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/newbroiler.def")

# Load the data:
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/newbroiler.rdata"))
head(newbroiler)
tail(newbroiler)


#' The estimated reciprocal model
#'  Q = a_1 + a_2*(1/p) + e 
fit1 <- lm(q~I(1/p), data=newbroiler)
summary(fit1)

plotModel(fit1)

#or 
newbroiler %>% ggplot(aes(x=p, y=q)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE, formula = y~I(1/x)) 

#' The reciprocal model fits the data relatively well.
#' There is some tendency to overestimate quantity in 
#' the middle range of prices and
#' underestimate quantity at the low and high extreme prices. 


#' It is also possible to create a new variable that is 
#' the reciprocal of price,and use this in a regression.



#' b) compute the elasticity of per capita consumption w.r.t. real price

#' Finding the derivative:
D(expression(1/p), "p")

D(expression(b1+b2*1/p), "p")

f1 <- makeFun(fit1)

#' Elasticity
e <- function(p) {coef(fit1)[2]*-(1/p^2)*p/f1(p)}

median(~p, data=newbroiler) # median p
f1(median(~p, data=newbroiler)) # q at median p

e(median(~p, data=newbroiler)) # elasticity at median p


curve(e(x), 0.9,3) # Higher prices, more elastic


##########################################
# log-log model 
fit_log_log <- lm(log(q) ~log(p), data = newbroiler)
coef(fit_log_log)
coef(fit_log_log)[2]

#'The elasticity found using the log-log model is -1.121 , 
#'a similar, but slightly smaller absolute value than that
#' for the reciprocal model

e(mean(~p, data=newbroiler)) # elasticity at median p
########################################

#' c.  linear-log model 

fit2 <- lm(q~log(p), data=newbroiler)
summary(fit2)

plotModel(fit2)

#' Like the reciprocal model, this linear-log model tends to 
#' under predict for low and high prices and
#' over predict for mid-range prices. 
#' Also, its fit appears slightly worse than that of
#' the reciprocal model. 



#' d). compue the elasticity 
 
#' Finding the derivative:
D(expression(log(p)), "p")

D(expression(b1+b2*log(p)), "p")

f2 <- makeFun(fit2)

#' Elasticity
e2 <- function(p) {coef(fit2)[2]*(1/p)*p/f2(p)}

median(~p, data=newbroiler) # median p
f2(median(~p, data=newbroiler)) # q at median p
e2(median(~p, data=newbroiler)) # elasticity at median p


#'compare the elasticities:
#' The linear-log model yields a more inealastic 
#' elasticity (in absolute value) than the other model.

curve(e2(x), 0.9,3, col="red") # Linear-log model
curve(e(x), 0.9,3, add = TRUE) # Reciprocal model



#' e. Log-linear function 

fit3 <- lm(log(q)~p, data=newbroiler)
summary(fit3)

f3 <- makeFun(fit3)

#plotModel(fit3) doesnot work here 
xyplot(q~p, data=newbroiler) 
plotFun(f3(p) ~ p, add=TRUE, col="darkred", lwd=2)

#' The model fits well at low prices but over predicts 
#' in the middle range, and under predicts at higher prices. 


#' f. compute the elasticity 
e3 <- function(p) {coef(fit3)[2]*p}

median(~p, data=newbroiler) # median p

f3(median(~p, data=newbroiler)) # q at median p

e3(median(~p, data=newbroiler)) # elasticity at median p

#' The elasticity from this model is lower than for the other models.


#' Plot the elasticities from all all 3 models
curve(e2(x), 0.9,3, col="red", xlab = "price", ylab = "elasticity") # Linear-log model
curve(e(x), 0.9,3, add = TRUE) # Reciprocal model
curve(e3(x), 0.9,3, col="blue", add = TRUE) # Log-lin model
legend("bottomleft", inset=.05, title="Models", #legend: topright
       c("Linear-Log","Reciprocal","Log-Lin"), fill=c("red","black","blue"), horiz=TRUE)

#' The generalized R^2 is:
cor(f3(newbroiler$p),newbroiler$q)^2


# g.  Evalute the suitability of the alternative model


# Plot the residuals 

# Reciprocal model
newbroiler$resid_rec <- resid(fit1)
newbroiler %>% ggplot(aes(x=1/p,y=resid_rec))+geom_point() + labs(title = "Reciprocal Model Residual") 
#'The residual scatter shows a definite spray 
#'pattern, suggesting heteroskedastic errors. 

#log-log 
newbroiler$resid_log_log <- resid(fit_log_log)
newbroiler %>% ggplot(aes(x=log(p),y=resid_log_log))+geom_point() + labs(title = "Log-Log Model Residual") 

#linear_log 
newbroiler$resid_lin_log <- resid(fit2)
newbroiler %>% ggplot(aes(x=log(p),y=resid_lin_log))+geom_point() + labs(title = "Lin-Log Model Residual") 
#'The residual scatter shows a definite spray 
#' pattern, suggesting heteroskedastic errors. 


#'log-linear 
newbroiler$resid_log_lin <- resid(fit3)
newbroiler %>% ggplot(aes(x=p,y=resid_lin_log))+geom_point() + labs(title = "Log-lin Model Residual") 
#' The residual scatter shows a U-shape, suggesting a missed
#'  quadratic or cubic component, or at least a misspecification. 

#All residuals on the same plot 
library(gridExtra)
Recip <-newbroiler %>% ggplot(aes(x=1/p,y=resid_rec))+geom_point() + labs(title = "Reciprocal Model Residual") 
log_log<- newbroiler %>% ggplot(aes(x=log(p),y=resid_log_log))+geom_point() + labs(title = "Log-Log Model Residual") 
lin_log <- newbroiler %>% ggplot(aes(x=log(p),y=resid_lin_log))+geom_point() + labs(title = "Lin-Log Model Residual") 
log_lin <-newbroiler %>% ggplot(aes(x=p,y=resid_lin_log))+geom_point() + labs(title = "Log-lin Model Residual") 


grid.arrange(Recip,log_log,lin_log,log_lin, ncol=2)


#' Given the above diagnostics, the log-log model seems to be
#'  the best choice, based on a less-well defined residual plot. 


library(tseries)
jarque.bera.test(resid(fit1)) #Reciprocal Model
jarque.bera.test(resid(fit_log_log)) #Log-Log Model
jarque.bera.test(resid(fit2)) #Lin-Log Model
jarque.bera.test(resid(fit3)) #Log-lin Model


########################################################
##################################################################
######################################################################

#' 4.17 

rm(list=ls())

library(mosaic)

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/tvdata.def")


#' Read the data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/tvdata.rdata"))
head(tvdata)
tail(tvdata)

#' a). plot the rate of tv ownership against per-capita expenditure

#' Plot the relationship
tvdata %>% ggplot(aes(x=spend_uk, y=rate_uk)) + geom_point()

#' A possible match is the linear-log model, 
#' shown in Figure 4.5(f), p. 162.


#' b). Estimate lin-log model 

fit <- lm(rate_uk ~ log(spend_uk), data = tvdata)
summary(fit)

plotModel(fit)

#' or,using the ggplot function 
f=makeFun(fit)
tvdata %>% ggplot(aes(x=spend_uk,y=rate_uk))+geom_point()+
   stat_function(fun = f,col="red")


#' The log-linear model fits the data fairly well, except at the very low 
#' and very high data points.

#' c). What is the interpretation of the intercept?

#' If spend_uk = 1, then ln(spend_uk) = 0, then E(rate_uk | spend_uk = 1) = b1  



#' d. Estimate lin-log model with correction 

fit2 <- lm(rate_uk ~ log(spend_uk-280), data = tvdata)
summary(fit2)

plotModel(fit2)

#' The adjustment improves the fit in the lower and upper ends of the model.


# ----------------------------

#' We can estimate the optimal correction factor (g), using starting values from OLS model,
#' using a nonlinear least squares procedure as:
fit3 <- fitModel(rate_uk ~ a + b*log(spend_uk - g), data = tvdata, start=list(a=coef(fit)[1], b=coef(fit)[2], g=280))
summary(fit3)

?fitModel

#' plot fitted values from the non-linear model
f <- makeFun(fit3)
xyplot(rate_uk ~ spend_uk, data = tvdata)
plotFun(f(spend_uk)~spend_uk, add=TRUE)

spend_uk <- tvdata$spend_uk
rate_uk <- tvdata$rate_uk

f(spend_uk) # predicted rate_uk

#' Generalized R^2
cor(f(spend_uk),rate_uk)^2
#' This is the best model so far

# ----------------------------


#' e.  Estimate log-reciprocal model 

#' log-reciprocal model
fit4 <- lm(log(rate_uk) ~ I(1/spend_uk), data = tvdata)
summary(fit4)

#plotModel(fit4) #' not working, dep. var is in log form

g <- makeFun(fit4)

xyplot(rate_uk ~ spend_uk, data = tvdata)
plotFun(g(spend_uk)~spend_uk, add=TRUE)

# or using ggplot 
tvdata %>% ggplot(aes(x=spend_uk,y=rate_uk))+geom_point()+
  stat_function(fun = g)

#' For these data the log-reciprocal model is almost a straight line, 
#' missing the curvature of the data.


#' f. Explain the failure of the model in e)

#' The problem is that in the reciprocal the value of the explanatory 
#' variable x, here spend_uk,becomes large, 
#' which makes 1/spend_uk very small.


#' g. Estimate log-reciprocal model with correction 

#' log-reciprocal model with correction
fit5 <- lm(log(rate_uk) ~ I(1/(spend_uk-280)), data = tvdata)
summary(fit5)

h <- makeFun(fit5)

xyplot(rate_uk ~ spend_uk, data = tvdata)
plotFun(h(spend_uk)~spend_uk, add=TRUE)

#' For comparison plot the fitted values from part (e).
plotFun(g(spend_uk)~spend_uk, add=TRUE)



#' ---------------------------------------------------------------------------------------------
#' We can estimate the optimal correction factor (g), using starting values from OLS model,
#' using a nonlinear least squares procedure as:
fit6 <- fitModel(log(rate_uk) ~ a + b*(1/(spend_uk - g)), data = tvdata, start=list(a=coef(fit5)[1], b=coef(fit5)[2], g=280))
summary(fit6)

i <- makeFun(fit6)
i(spend_uk) # predicted rate_uk

#' Generalized R^2
cor(i(spend_uk),rate_uk)^2

#' h). (repeat above with ireland)


# Exercise 4.21

rm(list=ls())

#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/malawi_small.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/malawi_small.rdata"))

# malawi_small.def

# a)
# plot of exchange rate
library(quantmod)
getFX("USD/MWK")
plot(USDMWK)

# Malawi pop size
# source: https://fred.stlouisfed.org/series/POPTOTMWA647NWDB
getSymbols('POPTOTMWA647NWDB', src='FRED')
plot(POPTOTMWA647NWDB)

# economy
# https://en.wikipedia.org/wiki/Economy_of_Malawi


# b) lin-log model
fit <- lm(pfood ~ log(totexp), data = malawi_small)
summary(fit)

# We estimate b2 < 0, so that as total expenditure rises the share devoted to food declines.
# A 95% interval estimate of b2 is:
confint(fit)[2,]

# The interval estimate is relatively narrow due to the small standard error of the coefficient estimate.
# We have estimated b2 relatively precisely.
# The model does not fit the data very well, explaining only 11.8% of the variation in the proportion of
# total expenditure spent on food about its mean.
# However, such low numbers are typical of microeconomic data regressions on household.
# The low goodness-of-fit does not by itself indicate a problem.

# c)
# elasticity
e <- function(totexp) {
  (coef(fit)[1]+coef(fit)[2]*(log(totexp)+1))/(coef(fit)[1]+coef(fit)[2]*(log(totexp)))
}

favstats(~totexp, data = malawi_small)
# 5 percentile
qdata(~totexp, p=0.05,data = malawi_small)
# 75 percentile
qdata(~totexp, p=0.75,data = malawi_small)

# elasticity
e(qdata(~totexp, p=0.05,data = malawi_small)[2])
e(qdata(~totexp, p=0.75,data = malawi_small)[2])

# plot
curve(e(x), 0.3,120)

library(car)
# se
deltaMethod(fit, "(b1+b2*(log(1.5)+1))/(b1+b2*log(1.5))", parameterNames= paste("b", 1:2, sep=""))
deltaMethod(fit, "(b1+b2*(log(10)+1))/(b1+b2*log(10))", parameterNames= paste("b", 1:2, sep=""))

# As we see this is not a constant elasticity function, see plot.
# At the 5th percentile of total expenditure the estimated elasticity is 0.85 and at the 75th percentile it is 0.79.
# Elasticity has fallen as total expenditure has increased.

# d)
# add all fits to the data in the model
fitted <- augment(fit)
fitted

fitted %>% ggplot(aes(.resid)) + geom_histogram()
fitted %>% ggplot(aes(x=log.totexp., y=.resid)) + geom_point()

# The histogram shows a not quite bell-shaped distribution. The residual plot shows no strong "spray" or other pattern.

tseries::jarque.bera.test(fitted$.resid)
# The null hypothesis for the Jarque-Bera test is that the regression errors are normally distributed. 
# The calculated JB test statistic value is 25.192.
# Thus, we reject the null hypothesis that the regression errors are normally distributed at the 1% level.

# e)
# calculate food expenditures
malawi_small <- malawi_small %>% mutate(food = pfood*totexp)

fit1 <- lm(log(food) ~ log(totexp), data = malawi_small)
summary(fit1)

# The log-log model is a constant elasticity function.
# We estimate the elasticity of food expenditures with respect to total expenditure to be 0.75

# A 95% interval estimate is
confint(fit1)[2,]

# f)
fitted1 <- augment(fit1)
fitted1

fitted1 %>% ggplot(aes(.resid)) + geom_histogram()
fitted1 %>% ggplot(aes(x=log.totexp., y=.resid)) + geom_point()

# The histogram of the residuals from the log-log model shows a strong negative skew.
# The scatter diagram shows a v-pattern, with less variation in the residuals at lower levels of total expenditure
# and more variation at higher levels. Assumption SR3, homoskedasticity, may be violated in this model.
# The value of the Jarque-Bera statistic is
tseries::jarque.bera.test(fitted1$.resid)
# We reject the normality of the regression errors.

# g) lin-log model
fit2 <- lm(food ~ log(totexp), data = malawi_small)
summary(fit2)

# The linear-log model is shown in Figure 4.5(f), p. 162.
# When the regression coefficient is positive the relationship increases at a decreasing rate,
# which is very plausible for food expenditure models.

# 50 percentile
qdata(~totexp, p=0.5,data = malawi_small)
# 75 percentile
qdata(~totexp, p=0.75,data = malawi_small)

# elasticity and se
deltaMethod(fit2, "b2/(b1+b2*log(5.7))", parameterNames= paste("b", 1:2, sep=""))
deltaMethod(fit2, "b2/(b1+b2*log(10))", parameterNames= paste("b", 1:2, sep=""))

# The linear log model does not have constant elasticity. As expenditure on food increases, the elasticity falls.

# h)
fitted2 <- augment(fit2)
fitted2

fitted2 %>% ggplot(aes(.resid)) + geom_histogram(bins=100)
fitted2 %>% ggplot(aes(x=log.totexp., y=.resid)) + geom_point()

# The histogram of the residuals from the linear-log model is very skewed.
# Measure Skewness and Kurtosis
library(moments)
skewness(fitted2$.resid)
kurtosis(fitted2$.resid)

# The value of the Jarque-Bera statistic is
tseries::jarque.bera.test(fitted2$.resid)
# We reject the normality of the model random errors.

# i)
# The model in b) lin-log model
fit
# predicted
f <- makeFun(fit)
# predicted food
f(malawi_small$totexp)*malawi_small$totexp
# correlation with food
cor(malawi_small$food,f(malawi_small$totexp)*malawi_small$totexp)

# The model in e) log-log model
fit1
# predicted
f1 <- makeFun(fit1)
# predicted food
f1(malawi_small$totexp)
# correlation with food
cor(malawi_small$food,f1(malawi_small$totexp))

# The model in g) lin-log model
fit2
# predicted
f2 <- makeFun(fit2)
# predicted food
f2(malawi_small$totexp)
# correlation with food
cor(malawi_small$food,f2(malawi_small$totexp))

# A generalized R2 is obtained by squaring the correlation coefficients.
cor(malawi_small$food,f(malawi_small$totexp)*malawi_small$totexp)^2
cor(malawi_small$food,f1(malawi_small$totexp))^2
cor(malawi_small$food,f2(malawi_small$totexp))^2

# The first and second models fit the data about equally well,
# while the linear-log model does not fit the data as well.
# The first model has the most bell-shaped residual histogram and shows little if any violation of SR3.
# Its results are intuitively plausible with the proportion of total expenditure on food and
# the elasticity for food expenditure falling as total expenditure rises.
# The elasticity does not appear constant based on the results from model 1.
# Therefore, the first of the models seems the best choice.


#' 4.23

rm(list=ls())

#' The data defenition file: 
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/malawi_small.def")

#' Load the data:
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/malawi_small.rdata"))

names(malawi_small)
#' no quote 
noquote(names(malawi_small))

#' Model 1: Budget share ptelephone, Lin-Log 
fit1 <- lm(ptelephone~log(totexp), data = malawi_small)
summary(fit1)

favstats(~ptelephone, data = malawi_small)
count(~ptelephone==0, data = malawi_small)

favstats(~totexp, data = malawi_small)
count(~totexp==0, data = malawi_small)  # no zero values for this var.

#' Model 2: Expenditure share ptelephone, Log-Log
fit_2 <- lm(log(ptelephone*totexp)~log(totexp), data =malawi_small)
#' Oppss. This is because the variable ptelephone contain zero vales and we are working log-log model

#' Remove observations with zero ptelephone
fit2 <- lm(log(ptelephone*totexp)~log(totexp), data = filter(malawi_small, ptelephone>0))

#' Model 3: Budget share pclothes, Lin-Log 
favstats(~pclothes, data = malawi_small)
count(~pclothes==0, data = malawi_small)

fit3 <- lm(pclothes~log(totexp), data = malawi_small)
summary(fit3)

#' Model 4: Expenditure share pclothes, Log-Log
#' Remove observations with zero pclothes
fit4 <- lm(log(pclothes*totexp)~log(totexp), data = filter(malawi_small, pclothes>0))

#' Model 5: Budget share pfuel, Lin-Log 

favstats(~pfuel, data = malawi_small)
count(~pfuel==0, data = malawi_small)

fit5 <- lm(pfuel~log(totexp), data = malawi_small)

#' Model 6: Expenditure share pfuel, Log-Log
#' Remove observations with zero pfuel
fit6 <- lm(log(pfuel*totexp)~log(totexp), data = filter(malawi_small, pfuel>0))

library(stargazer)
stargazer(fit1, fit2, fit3, fit4, type="text", intercept.bottom = FALSE)
stargazer(fit5, fit6, type="text", intercept.bottom = FALSE)

#'--------------------------------------------------------------------------- the follwoing aren't easy to understand----
# Lets make the models for all categories
library(tidyverse)
library(broom)
malawi.long <- malawi_small %>% gather(key = "variable", value = "value", -totexp)
head(malawi.long)

malawi.long %>% group_by(variable) %>% do(tidy(lm(value~log(totexp), data=.))) # broom::tidy

b1.coef <- malawi.long %>% group_by(variable) %>% do(tidy(lm(value~log(totexp), data=.))) %>% 
  filter(term=="(Intercept)") %>% select(variable,estimate) %>% rename(b1=estimate)
b2.coef <- malawi.long %>% group_by(variable) %>% do(tidy(lm(value~log(totexp), data=.))) %>% 
  filter(term=="log(totexp)") %>% select(variable,estimate) %>% rename(b2=estimate)

b1.coef
b2.coef

b2.coef %>% mutate(one.percent.one.unit=b2/100) # A 1% change in x leads to a unit change in y

coefs <- left_join(b1.coef, b2.coef, means, by="variable")
coefs

#' Elasticities lin-log model
e.lin.log <- function(x) {1+(coefs$b2/(coefs$b1+coefs$b2*log(x)))}

quantile(~totexp|variable, probs=0.25, data = malawi.long)

e.lin.log(3.5) #' Q1=3.5

quantile(~totexp|variable, probs=0.75, data = malawi.long)

e.lin.log(10) # Q3=10

median(~totexp|variable, data = malawi.long)

e.lin.log(5.7)

# Expenditure elasticities: log-log
e.log.log <- malawi.long %>% group_by(variable) %>% 
  do(tidy(lm(log(value*totexp)~log(totexp), data=filter(.,value>0)))) %>% 
  filter(term=="log(totexp)") %>% select(variable,estimate) %>% rename(e.log.log=estimate)
e.log.log

median(~totexp|variable, data = malawi.long)
e.lin.log(5.7)

cbind(e.log.log$variable,e.log.log$e.log.log,e.lin.log(5.7))
# lin log model have more elastic expenditure elasticities


# ------------------------------------------------------------------------------------------------------



#  4.28)

rm(list=ls())

library(mosaic)
library(broom)

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/wa_wheat.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/wa_wheat.rdata"))

names(wa_wheat)

dframe <- wa_wheat %>% select(northampton, time) %>% rename(yield=northampton) 
head(dframe)


mod1_lin <- lm(yield ~ time, data = dframe)
summary(mod1_lin)

# some statistics from the estimated model
dframe %>% do(glance(lm(yield ~ time, data = .)))

plotModel(mod1_lin)
mplot(mod2_lin)

# More plots.... 
hist(resid(mod1_lin))

augment(mod1_lin)
resid.mod1_lin <- augment(mod1_lin)
resid.mod1_lin %>% ggplot(aes(x=time,y=.resid))+geom_point()


# The  lin-log model 
mod2_lin_log <- lm(yield ~ log(time), data = dframe)
summary(mod2_lin_log)

plotModel(mod2_lin_log)
#mplot(mod2_lin_log)
dframe %>% do(glance(lm(yield ~ log(time), data = .)))

mod3_sq <- lm(yield ~ I(time^2), data = dframe)
summary(mod3_sq)

plotModel(mod3_sq)
#mplot(mod3_sq)
dframe %>% do(glance(lm(yield ~ I(time^2), data = .)))

mod4_log_lin <- lm(log(yield) ~ time, data = dframe)
summary(mod4_log_lin)
#plotModel(mod4_log_lin)
f4 <- makeFun(mod4_log_lin)
xyplot(yield ~ time, data = dframe)
plotFun(f4(time) ~ time, add=TRUE, col="red")

#mplot(mod4_log_lin)
dframe %>% do(glance(lm(log(yield) ~ time, data = .)))
library(gridExtra)
mplot(mod4_log_lin, which = 1:7, multiplot = TRUE, ncol = 2)

#' Jarque Bera test H0: Residuals are Normal
jarque.bera.test(resid(mod1_lin))
jarque.bera.test(resid(mod2_lin_log))
jarque.bera.test(resid(mod3_sq))
jarque.bera.test(resid(mod4_log_lin))

gf_histogram(~resid(mod1_lin))
gf_histogram(~resid(mod2_lin_log))
gf_histogram(~resid(mod3_sq))
gf_histogram(~resid(mod4_log_lin))

#' Comparing the quadratic and log-linear fitted curves we see that both capture the shape of the relationship.
#' The quadratic model has a higher R^2 and the residual plot does not show as much of a dip in the center region.
#' Thus, we choose the quadratic model as our preferred specification.

# linear model
dframe %>% do(tidy(lm(yield ~ time, data = .))) %>% filter(term=="time") %>% select(term,estimate)
# linear-log model, dy/dx = beta*1/x
dframe %>% do(tidy(lm(yield ~ log(time), data = .))) %>% filter(term=="log(time)") %>% select(term,estimate)
# squared model, dy/dx = 2*beta*time
dframe %>% do(tidy(lm(yield ~ I(time^2), data = .))) %>% filter(term=="I(time^2)") %>% 
  select(term,estimate) %>% summarise(2*estimate)
# log-lin model, dy/dx = beta*time
dframe %>% do(tidy(lm(log(yield) ~ time, data = .))) %>% filter(term=="time") %>% select(term,estimate)

# linear model
# Studentized residual threshold = 2
dframe %>% do(augment(lm(yield ~ time, data = .))) %>% arrange(-.std.resid)
dframe %>% do(augment(lm(yield ~ time, data = .))) %>% arrange(.std.resid)

mod3 <- lm(yield ~ I(time^2), data = filter(dframe, time <= 47))
f <- makeFun(mod3)
f(48, interval="prediction")
dframe[48,]


