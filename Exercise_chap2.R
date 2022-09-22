#' Do exercise: 2.17, 2.18, 2.19 2.21



##################################################

#' 2.17

rm(list=ls())

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/collegetown.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/collegetown.rdata"))

head(collegetown)

#' a)
collegetown %>% ggplot(aes(x=sqft, y=price)) + geom_point() + xlim(c(0,100)) + labs(title = "House price vs house size")

#' b)
fit1 <- lm(price ~ sqft, data = collegetown)
summary(fit1)

#' Interpretation of the coefficients:
#' The Slope implies: A 1 unit (i.e., 100sqft) increase of living area will increase
#' the expected home price by 13.40294 unit ($1000)= $ 13, 402.94, holding all else constant. 
#' The intercept implies: A house with zero square feet (i.e., sqft =0) has an expected price 
#' of -11.4236 unit ($1000)= $ -115,423.60. This is has no meaning because, in the sample, 
#' there are no houses that have zero square feet. 

library(mosaic)
favstats(~sqft, data = collegetown)   #there is no data values with a house size near zero. 


# use ggplot 
collegetown %>% ggplot(aes(x=sqft, y=price)) + geom_point()+ geom_smooth(method = "lm", se = FALSE) 

#' c)
fit2 <- lm(price ~ I(sqft^2), data = collegetown)
summary(fit2)


slope <- function(sqft) {2*coef(fit2)[2]*sqft}
#' 2000 sqft is 20 when the units are in 100s sqft
1000*slope(20)
#' an additional 100 sqft for a 2000 sqft home will add  1000*slope(20) dollars.

#' d)
collegetown %>% ggplot(aes(x=sqft, y=price)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE, formula = y~I(x^2)) + xlim(c(0,100))

# intercept of tangent at 2000 sqft
f <- makeFun(fit2)
f(20)-slope(20)*20

# ggplot
tangent <- function(x) {f(20)-slope(20)*20 + slope(20)*x}

collegetown %>% ggplot(aes(x=sqft, y=price)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE, formula = y~I(x^2)) + xlim(c(0,100)) +
  stat_function(fun=tangent, col="red")

#' or using the base R
with(collegetown, plot(sqft,price, xlim = c(0,100)))
curve(f(x), 5,80, add=TRUE, col="blue", lwd=2)
curve(f(20)-slope(20)*20+slope(20)*x, 5,80, col="red", add=TRUE, lwd=2) # add tangent
segments(20,0,20,f(20), lwd=2, col="green")

#' e)
elasticity <- function(x) {slope(x)*x/f(x)}
elasticity(20)

#' f)
library(broom)

tidy(fit1)
glance(fit1)
augment(fit1)

lin.mod <- augment(fit1)

lin.mod %>% ggplot(aes(x=sqft, y=.resid)) + geom_point() +
  ggtitle("Residuals from the linear model") +
  xlab("sqft") + ylab("residuals")

plot1 <- lin.mod %>% ggplot(aes(x=sqft, y=.resid)) + geom_point() +
  ggtitle("Residuals from the linear model") +
  xlab("sqft") + ylab("residuals")

augment(fit2)
quad.mod <- augment(fit2)

plot2 <- quad.mod %>% ggplot(aes(x=sqrt(`I(sqft^2)`), y= price - .fitted)) + geom_point() +
  ggtitle("Residuals from the quadratic model") +
  xlab("sqft") + ylab("residuals")

library(gridExtra)
grid.arrange(plot1,plot2, ncol=2)

#' g) compare the SSE from the two models 
glance(fit1)$deviance
glance(fit2)$deviance

anova(fit1)
anova(fit2)


##########################################################################################

#' 2.18

rm(list=ls())

library(mosaic)

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/collegetown.def")

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/collegetown.rdata"))

#' a)

collegetown %>% ggplot(aes(x=price)) + geom_histogram() +
ggtitle("Histogram of price")

collegetown %>% ggplot(aes(x=log(price))) + geom_histogram() +
  ggtitle("Histogram of log(price)") # a bit normal than price, by taking the log of the price, we normalized it. 


plot1 <- collegetown %>% ggplot(aes(x=price)) + geom_histogram() +
  ggtitle("Histogram of price")

plot2 <- collegetown %>% ggplot(aes(x=log(price))) + geom_histogram() +
  ggtitle("Histogram of log(price)")

library(gridExtra)
grid.arrange(plot1,plot2, ncol=2)

#' The distribution of PRICE is skewed with a long tail to the right. 
#' The distribution of log(price) is more symmetrical.

#Test normality using the Jarque–Bera test (JB) test from the "moment" package"

#install.packages("moments")
require(moments)
# H0: Normality
jarque.test(collegetown$price)

jarque.test(log(collegetown$price))


#' b) log-linear model

fit <- lm(log(price) ~ sqft, data = collegetown)
summary(fit)

# a 100 square foot increase (1 unit) in house size increases predicted price by approximately
100*coef(fit)[2] # percent

#' The estimated intercept tells us that  
#' the predicted price of a zero square foot house is 
exp(coef(fit)[1]) *1000 # multiply by 1000 b/c the unit of sale price is thousands dollar

#' This estimate has little meaning because in the sample there 
#' are no houses with zero square feet of living area. You can see this by running ..
favstats(~sqft, data=collegetown)

f <- makeFun(fit)  # this function gives exp(coef(fit)[1]+coef(fit)[2]*sqft)

# Graph the fitted price against sqft, ggplot
collegetown %>% ggplot(aes(x=sqft, y=price)) + geom_point() +
  xlim(c(0,100)) + stat_function(fun=f, col="red")

slope <- function(x) {coef(fit)[2]*f(x)}

#' For a 2000 square foot house the predicted price is: 
 f(20)

#' The estimated slope is
slope(20)

#' in units
f(20)*1000
slope(20)*1000
round(slope(20)*1000,1)

#' The predicted price of a house with 2000 square feet of living area is $166,460.10.
#' We estimate that 100 square foot size increase for a house with 2000 square feet of 
#' living area will increase price by $6,000, holding all else fixed.


# ggplot including the tangent line for the curve 
tangent <- function(x) {f(20)-slope(20)*20 + slope(20)*x}

collegetown %>% ggplot(aes(x=sqft, y=price)) + geom_point() +
  xlim(c(0,100)) + stat_function(fun=f, col="red") +
  stat_function(fun=tangent, col="blue")

# c) residuals
library(broom)
log.mod <- augment(fit)
log.mod
log.mod %>% ggplot(aes(x=sqft, y =`log(price)`-.fitted )) + geom_point() +  # .resid
  ggtitle("Residuals from the log-linear model") +
  xlab("sqft") + ylab("residuals")

#' d) two-regressions

favstats(~price|close, data = collegetown)
favstats(~sqft|close, data = collegetown)

#' The summary statistics show that there are 189 houses close to LSU and 311 
#' houses not close to LSU in the sample. The mean house price is $10,000 larger 
#' for homes close to LSU, and the homes close to LSU are slightly smaller, by 
#' about 100 square feet. The range of the data is smaller for the homes close 
#' to LSU, and the standard deviation for those homes is half the standard 
#' deviation of homes not close to LSU.


#  what about uisng the filter function 
closer_U <- collegetown %>% filter(close ==1)
Not_closer_U <- collegetown %>% filter(close ==0)

favstats(~price, data = closer_U)


#' e)
fit.0 <- lm(log(price) ~ sqft, data = filter(collegetown, close==0))
coef(fit.0)

fit.1 <- lm(log(price) ~ sqft, data = filter(collegetown, close==1))
coef(fit.1)

#' For homes close to LSU we estimate that an additional 100 square feet of 
#' living space will increase predicted price by about 2.69% and for homes not 
#' close to LSU about 4.02%.
 
#' Together, and more outputs  
collegetown %>% group_by(close) %>% do(tidy(lm(log(price) ~ sqft, data = .)))
#collegetown %>% group_by(close) %>% do(glance(lm(log(price) ~ sqft, data = .)))

#' plots of the regression model 
f0 <- makeFun(fit.0)
f1 <- makeFun(fit.1)

#' or , only the means 
mean_p <- data.frame(price=mean(~price|close, data = collegetown), close=c(0,1))
mean_p

mean_sqft <- data.frame(sqft=mean(~sqft|close, data = collegetown), close=c(0,1))
mean_sqft

collegetown %>% ggplot(aes(x=sqft, y=price, color=as.factor(close))) + geom_point() +
  geom_hline(aes(yintercept = price, color=factor(close)), mean_p) +
  geom_vline(aes(xintercept = sqft, color=factor(close)), mean_sqft) +
  stat_function(fun=f0, col="#F8766D") +
  stat_function(fun=f1, col="#00BFC4") + xlim(c(0,100)) + ylim(c(0,1500))


#' f) 
#' Assumption SR1 implies that the data are drawn from the same population. 
#' So the question is, are homes close to LSU and homes not close to LSU in
#' the same population? Based on our limited sample, and using just a simple, 
#' one variable, regression model it is difficult to be very specific.
#' The estimated regression coefficients for the sub-samples are different, 
#' the question we will be able to address later is “Are they significantly
#'  different.” Just looking at the magnitudes is not a statistical test.



##########################################################################################

#' 2.19


rm(list=ls())
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(broom))


browseURL("http://www.principlesofeconometrics.com/poe5/data/def/stockton5_small.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/stockton5_small.rdata"))

names(stockton5_small)

#' a)
stockton5_small <- stockton5_small %>% mutate(sprice=sprice/1000)

stockton5_small %>% ggplot(aes(x=livarea, y=sprice)) + geom_point()

# b)
fit <- lm(sprice ~ livarea, data = stockton5_small)
summary(fit)

stockton5_small %>% ggplot(aes(x=livarea, y=sprice)) + geom_point() +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE)


# c)
fit_Q <- lm(sprice ~ I(livarea^2), data = stockton5_small)
summary(fit_Q)

stockton5_small %>% ggplot(aes(x=livarea, y=sprice)) + geom_point() +
  geom_smooth(method = lm, formula = y ~ I(x^2), se = FALSE)

slope <- function(livarea) {2*coef(fit_Q)[2]*livarea}
# 1500 square feet, units of 100, 1500/100=15
1000*slope(15)


# d)
stockton5_small %>%  ggplot(aes(x=livarea, y=sprice)) + geom_point() +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, color="blue") +
  geom_smooth(method = lm, formula = y ~ I(x^2), se = FALSE, color="red")

deviance(fit) < deviance(fit_Q)



###################################################################################################

#' 2.21


rm(list=ls())

library(mosaic)


#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/stockton5.def")
#load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/stockton5.rdata"))
#str(stockton5)

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/stockton5_small.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/stockton5_small.rdata"))

stockton5 <- stockton5_small %>% mutate(price=sprice/1000)

head(stockton5)

#' a.
fit <- lm(price~age, data = stockton5)
summary(fit)

#'We estimate that a house that is new, AGE = 0, will have expected 
#'price $152,614.40. We estimate that each additional year of age will 
#'reduce expected price by $981.20, other things held constant. 
#'The expected selling price for a 30-year-old house is: 

f <- makeFun(fit)
f(30)
f(30)*1000

#' b.
plotModel(fit)
#plotModel(fit, system="g")

#' The data show an inverse relationship between house prices and age.
#' The data on newer houses is not as close to the fitted regression 
#' line as the data for older homes.

#' c.
fit2 <- lm(log(price)~age, data = stockton5)
summary(fit2)

#' We estimate that each additional year of age reduces 
#' expected price by about 0.75%, holding all else constant.  

#' d.
g <- makeFun(fit2)

gf_point(price ~ age, data = stockton5) %>% gf_fun(g(age) ~ age, col="red", lwd=2)

#' or 
xyplot(price ~ age, data = stockton5, pch=20, alpha=.2, xlim = c(0,100))
plotFun(g(age) ~ age, col="red", lwd=2, xlim = c(0,100), add=TRUE)

#' plotModel(fit) does not work with transformed y models

#' The fitted log-linear model is not too much different than
#'  the fitted linear relationship.


#' e.

#' The expected selling price of a house that is 30 years old is 

g(30)*1000

coef(fit2)
exp(coef(fit2)[1]+coef(fit2)[2]*30)

#' This is about $13,000 less than the prediction based on the linear relationship.


#' f.

#' linear model
sum((stockton5$price-f(stockton5$age))^2)
sum((stockton5$price-predict(fit))^2)
#' log linear model
sum((stockton5$price-g(stockton5$age))^2)

#' Based on the plots and visual fit of the estimated regression
#'lines it is difficult to choose between the two models. 
#'The sum of squared differences between the data and fitted 
#'values is smaller for the estimated linear relationship, 
#'by a small margin. This is one way to measure how well a 
#'model fits the data. In this case, based on fit alone, 
#'we might choose the linear relationship rather than the log-linear relationship.



