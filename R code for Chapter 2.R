#' R. Carter Hill, William E. Griffiths and Guay C. Lim,  
#' Principles of Econometrics, Fifth Edition, Wiley, 2018.
rm(list=ls())

library(mosaic)

#' Data set on food expenditure and weekly income from a random sample of 40 households.
#' Data definition file: <http://www.principlesofeconometrics.com/poe5/data/def/food.def>
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/food.def")

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/food.rdata"))

head(food, 3)
tail(food, 4)

#' The summary command reports some descriptive statistics (Table 2.1, p. 59)
summary(food)

#' To be able to work with the variable names directly we use the *$* assignment
#' These commands replicate the descriptive statistics 
food$food_exp
mean(food$income)   # calculates the mean
median(food$income) # calculates the median
max(food$income)    # maximum value
min(food$income)    # minimum value
sd(food$income)     # calculates the standard deviation

favstats(~income, data=food) # one sweep
stats <- favstats(~income, data=food)
stats$n
stats$mean
stats$median
favstats(~income, data=food)$mean
favstats(~income, data=food)$sd

str(stats)

#' Figure 2.6
xyplot(food_exp ~ income, data=food)

#mPlot(food) # use "cog" wheel in plot to change setup

food %>% ggplot(aes(x = income, y = food_exp)) +
  geom_point(col="red") + labs(title = "Test")

#' Estimating the parameters of the simple linear regression model in Chapter 2
names(food)
lm(food_exp ~ income, data = food)
favstats(~income, data=food)
#' Interpretation: Intercept, when income(x) is equal to zero,
#' the average food expenditure is $ 83.42
#' income: when income increase by $100 (1 unit), the average 
#' food expenditure increase by $ 10.21
#' income: when income decrease by $100 (1 unit), the average 
#' food expenditure decrease by $ 10.21

#' assign regression model to an object called m1
m1 <- lm(food_exp~income, data=food)
names(m1)

#' Figure 2.8. The fitted regression
plotModel(m1) 

#' Manually, base R
plot(food_exp ~ income, data=food, main = "Title here...")
abline(m1, col="red", lwd=2)

#' Estimated model (similar, buit not identical to Figure 2.9)
#' <http://jakeruss.com/cheatsheets/stargazer.html>
library(stargazer)
stargazer(m1, type="text", intercept.bottom=FALSE)

#' A more general output from the model is given by
summary(m1)
summary(lm(food_exp~income, data=food))

#' The OLS parameter estimates
m1
coef(m1) # vector
coef(m1)[1] 
coef(m1)[2]

#' using the name of the vector
coef(m1)["income"]
coef(m1)["(Intercept)"]

#' Save the coeficients as Values for later use
b1 <- coef(m1)[1]
b1
b2 <- coef(m1)[2]
b2

#' Calculate the Income elasticity at mean values, p. 64
income.elast.mean <- b2*mean(food$income)/mean(food$food_exp)
income.elast.mean
#' Interpretation: When income increase by 1%, food expenditure increase by 0.7%.  
#' Interpretation: When income increase by 1%, food expenditure increase by `r income.elast.mean`.  
#' Interpretation: When income increase by 1%, food expenditure increase by `r round(income.elast.mean*1,2)`.  

#' When income decrease by 1%, food expenditure decrease by:
round(income.elast.mean*-1, 2)

#' When income increase by 10%, food expenditure increase by:
round(income.elast.mean*10, 2)

#' When income decrease by 5%, food expenditure decrease by:
round(income.elast.mean*-5, 2)

#' st.error of elasticity using the delta method (Chapter 5)
library(car)
mx <- mean(food$income)
my <- mean(food$food_exp)

deltaMethod(m1, "b2*mx/my", parameterNames=paste("b", 1:2, sep=""))

#' This creates the Income elasticity (at every observation) as a variable
income.elast <- b2*food$income/food$food_exp
plot(income.elast)
abline(h=mean(income.elast))

#' Note that the Income elasticity calculated at mean x and y values, and the
#' average of the Income elasticity at all observations differ a bit.
mean(income.elast)
income.elast.mean

#' Prediction at income=20
f <- makeFun(m1)
f(income=20)
f(mean(food$income))
f(mean(food$income), interval="confidence")
f(20, interval="confidence", level=0.9)
f(20)
f(0:40)
f(food$income) # predicted values on the regression line

plot(food$income,f(food$income)) # these are the data points on the regression line

#' Elasticity at different points anlong the regression line
e <- function(income) {b2*income/f(income)}
e(mean(food$income)) # at mean
# Making a plot of the elasticity
curve(e(x), 0,40, xlab = "income", main = "Food expenditure elasticity at different levels of income")

#' Repeated sampling (mosaic::do) or bootstrapping
#' Illustrating 2.4.4. the variance of b1 and b2
resample(food)
#' Bootstrap a 100 regressions
table2_2 <- do(100)*lm(food_exp~income, data=resample(food))
table2_2

#' Plot the data first,
#' then add each of the MC regression lines, brute force approach
plot(food$income, food$food_exp, col="blue", pch=19, main = "Bootstrap parameter variance")
for(i in 1:length(table2_2$.index)) {
  curve(table2_2[i,1]+table2_2[i,2]*x, 0, 40, add = TRUE, col='#FF000088')
}

#' Model residuals
names(m1)
resid(m1)

#' Residual calculated "manually" as difference between observed and predicted
cbind(food$food_exp-f(food$income), resid(m1))
all.equal(food$food_exp-f(food$income), resid(m1), tolerance=1/1e9)

length(food$food_exp-f(food$income))
length(resid(m1))

#' Sums of squared residuals
deviance(m1)
sum(resid(m1)^2)

#' The models subsequent sums of squares and ANOVA table
anova(m1)

#' Sums of squares due to regression
sum((f(food$income)-mean(food$food_exp))^2)

#' Sums of squares total
sum((f(food$income)-mean(food$food_exp))^2)+sum(resid(m1)^2)

sum((food$food_exp-mean(food$food_exp))^2)

#' The variance in the model 
deviance(m1)/m1$df.residual

#' The variance/covariance matrix 
vcov(m1)

#' The residual standard error (sqare root of the variance)
summary(m1)
sqrt(deviance(m1)/m1$df.residual) # df is N-2


#' -------------------------------
#' Other economic models, 2.3.2
#' Example estimate an "iron" model
# iron	iron ore price in US$ per tonne.
# xrate	Australian $ exchange rate in U.S.$.
rm(list=ls())
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/iron.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/iron.rdata"))

ironmodel <- lm(iron ~ xrate, data = iron)
summary(ironmodel)

#' elasticity
coef(ironmodel)[2]*mean(iron$xrate)/mean(iron$iron)

#browseURL("https://www.quandl.com/data/FRED/DCOILBRENTEU-Crude-Oil-Prices-Brent-Europe")

#' -------------------------------
#' Example 2.6
rm(list=ls())

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/br.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/br.rdata"))

head(br)

#' Estimate the regression model
fit <- lm(price~I(sqft^2), data=br)
summary(fit)

with(br, plot(sqft,price))
br <- br %>% mutate(sqft2 = sqft^2)
lm(price~sqft2, data=br)

library(broom)
tidy(fit)
glance(fit)
augment(fit)

fit_stats <- tidy(fit)
fit_stats$estimate
fit_stats$estimate[2] # slope

plotModel(fit)
f <- makeFun(fit)
f(2000)
f(6000)

slope <- function(sqft) {2*coef(fit)[2]*sqft}
slope(c(2000,4000,6000))

library(latticeExtra)
obj1 <- xyplot(price ~ sqft, data=br, pch=20, alpha=.2, xlim = c(0,8000))
obj2 <- plotFun(slope(sqft) ~ sqft, col="red", xlim = c(0,8000))
# A plot of the data with price on the left axis, and a red line representing the slope on the right axis
doubleYScale(obj1, obj2, add.ylab2 = TRUE)

elasticity <- function(sqft) {slope(sqft)*sqft/f(sqft)}
obj3 <- plotFun(elasticity(sqft) ~ sqft, col="red", xlim = c(0,8000))
# A plot of the data with price on the left axis, and a red line representing the elasticity on the right axis
doubleYScale(obj1, obj3, add.ylab2 = TRUE)
stats <- favstats(~sqft, data = br)
ladd(panel.abline(v=c(stats$Q1,stats$Q3),  lwd=3, alpha=.4 ) ) # vertical line at Q1 and Q4
elasticity(c(stats$Q1,stats$median,stats$Q3))

elasticity(c(2000,4000,6000))
# corresponding prices
f(c(2000,4000,6000))

#' -------------------------------
#' Example 2.7 a log-linear function
rm(list = ls())

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/br.rdata"))
head(br)

#' Figure 2.16
histogram(~price, width=10000, data=br)
histogram(~log(price), width=0.1, data=br)

#' Estimate regression model
fit <- lm(log(price)~sqft, data=br)
summary(fit)

with(br, plot(sqft,price)) # original data in levels
with(br, plot(sqft,log(price))) # log transformed price and sqft in levels

br <- br %>% mutate(logprice = log(price))
lm(logprice~sqft, data=br)

tidy(fit)
glance(fit)
augment(fit)

#plotModel(fit) does not work with transformed y models
xyplot(price ~ sqft, data=br, pch=20, alpha=.2, xlim = c(0,8000))
f <- makeFun(fit)
plotFun(f(sqft) ~ sqft, col="red", xlim = c(0,8000), add=TRUE)
g <- function(sqft) {exp(coef(fit)[1]+coef(fit)[2]*sqft)} # manually using the exponential function exp()
plotFun(g(sqft) ~ sqft, col="green", xlim = c(0,8000), add=TRUE)

#' Note this is a plot where log(price) is the scale of the y-axis!
library(rockchalk)
plotCurves(fit, plotx = "sqft")

#' Slope as a function of price
slope <- function(price) {coef(fit)[2]*price}
slope(c(100000,500000))

#' Slope as a function of predicted price, i.e. a function of sqft
slope2 <- function(sqft) {coef(fit)[2]*f(sqft)}

#' At what sqft is the price equal to 100000?
s1 <- findZeros(f(sqft)-100000~sqft) ; s1
f(s1)
#' At what sqft is the price equal to 500000?
s2 <- findZeros(f(sqft)-500000~sqft) ; s2
f(s2)

slope(c(100000,500000)) # same as above
slope2(c(s1$sqft,s2$sqft))

#' A plot of the data with price on the left axis, and a red line representing the slope
#' as a function of predicted price, i.e. a function of sqft
obj1 <- xyplot(price ~ sqft, data=br, pch=20, alpha=.2, xlim = c(0,8000))
obj2 <- plotFun(slope2(sqft) ~ sqft, col="red", xlim = c(0,8000))
doubleYScale(obj1, obj2, add.ylab2 = TRUE)

#' Fitted line and slope as a function of predicted price, i.e. a function of sqft
obj3 <- plotFun(f(sqft) ~ sqft, col="blue", xlim = c(0,8000))
doubleYScale(obj2, obj3, add.ylab2 = TRUE)

#' Elasticity as a function of sqft
elasticity <- function(sqft) {coef(fit)[2]*sqft}
elasticity(c(2000,4000))

#' A plot of the data with price on the left axis, and a red line representing the elasticity on the right axis
obj4 <- plotFun(elasticity(sqft) ~ sqft, col="red", xlim = c(0,8000))
doubleYScale(obj1, obj4, add.ylab2 = TRUE)

#' A plot of the blue fitted price on the left axis, and a red line representing the elasticity on the right axis
doubleYScale(obj3, obj4, add.ylab2 = TRUE)

#' 100 square foot increase will increase the price by:
100*100*coef(fit)[2]
#' percent (elasticity, equation 2.28).
#' Note: first 100 is percent second 100 is unit


#' ---------------------------------------------------------
#' 2.9 Regression with indicator variables

rm(list = ls())

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/utown.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/utown.rdata"))

# price	house price, in $1000
# sqft		square feet of living area, in 100's
# age		house age, in years
# utown	=1 if close to university
# pool		=1 if house has pool
# fplace	=1 if house has fireplace

head(utown)
histogram(~price|utown, data = utown)
histogram(~price|as.factor(utown), layout=(c(1,2)), data = utown)

utown %>% ggplot(aes(x=price, fill=as.factor(utown))) + geom_density(alpha=0.25)

favstats(~price|utown, data=utown)
mean(~price|utown, data=utown)

t.test(price~utown, data=utown, var.equal=TRUE)
ttest <- tidy(t.test(price~utown, data=utown, var.equal=TRUE))
ttest

#library(HH)
#NTplot(t.test(price~utown, data=utown, var.equal=TRUE))

# ANOVA
summary(aov(price~utown, data=utown))
ttest$statistic^2

#' Regression approach
mean(~price, data=utown) # mean independent of group, i.e, utown 0 or 1, or grand mean
lm(price~1, data=utown) # mean independent of group, i.e, utown 0 or 1, or grand mean

#' Mean per group
mean(~price|utown, data = utown)
diffmean(~price|utown, data = utown)
diffmean(price~utown, data = utown)

means <- mean(~price|utown, data = utown)
means

#' Note that sqft is not in this model, I just use it to get a scatterplot!
xyplot(price ~ sqft, groups = utown, data = utown, auto.key=TRUE)
ladd(panel.abline(h=c(means[1],means[2]), lwd=3, alpha=.4, col=c("blue","red")))

#' Regression model with an indicator variable identifying the groups
fit <- lm(price~utown, data=utown)
fit
summary(fit)

#' Golden OaKS, utown==0
coef(fit)[1]
mean(~price, data=filter(utown, utown==0))

#' University Town, utown==1
coef(fit)[2] # is the difference between the two means!
#' calculate the difference between the two means
mean(~price, data=filter(utown, utown==1))-mean(~price, data=filter(utown, utown==0))
coef(fit)[1]+coef(fit)[2]
mean(~price, data=filter(utown, utown==1))

#' Reverse the coding of utown
utown <- utown %>% mutate(REVutown = ifelse(utown==1,0,1))

with(utown, table(utown,REVutown))

fit
REVfit <- lm(price~REVutown, data=utown)
REVfit

summary(fit)
summary(REVfit)