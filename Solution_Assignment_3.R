


# 3.22


rm(list=ls())

library(mosaic)


#The data definition file: http://www.principlesofeconometrics.com/poe5/data/def/collegetown.def

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/collegetown.rdata"))
head(collegetown)

str(collegetown)

fit <- lm(price ~ sqft, data = collegetown)
fit

# the means 
msqft <- mean(~sqft, data=collegetown)
mprice <- mean(~price, data=collegetown)

#Elasticity evaluated at the mean of the variables 
coef(fit)[2]*msqft/mprice

# a) 95% CI for elasticity
library(car)
deltaMethod(fit, "b2*msqft/mprice", parameterNames= paste("b", 1:2, sep=""))

# Interpretation of the CI: 
#' We estimate that a 1% increase in the total interior living 
#' space of a house will increase expected price by 1.37% to 1.56%  


#' b) H0: Test if elasticity = 1

deltaMethod(fit, "b2*msqft/mprice-1", parameterNames= paste("b", 1:2, sep=""))

#' Zero (the value under H0) is not a part of the 95% CI so we reject
#' the null hypothesis that the elasticity is 1
#' and accept the alternative that it is not.

#' c) H0: Marginal effect of 100 sqft <= 13 (13*1000=13000)
library(multcom)
summary(glht(fit, linfct = c("sqft <= 13"))) 

#Keep H0, we are unable to conclude that a 100 square foot increase (=1 sqft) in living area will increase price
#by more than $13000 (=13 price).

#d) 95% CI for a 2000 sqft house

deltaMethod(fit, "b1+b2*20", parameterNames= paste("b", 1:2, sep=""))

#' We estimate, with 95% confidence, that the average price of 
#' a home of 2000 square feet of interior living space is 
#' between $141,549.60 and $163.720.60.

# e)

collegetown %>%
  filter(sqft==20)

collegetown %>%
  filter(sqft==20) %>%
  summarise(sample_price_2000sqft=mean(price)) %>% head()


#' In the sample there are 3 houses with 2000 square feet. 
#' They sold for $138,000, $169,000 and $183,000. 
#' The average is 163.3333 which is inside the interval estimate.
#'  The interval estimate in (d) was for the expected, or population 
#'  average, price for homes with 2000 square feet of living area.
#'   The average of the 3 observed house prices is not a population average,
#'    but we should not be surprised by the outcome.


#-------------------------------------------------------------

# 3.24  

rm(list=ls())

#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/fair5.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/fair5.rdata"))

head(fair5)

# a)
fit <- lm(vote ~ growth, data = fair5)
summary(fit)

confint(fit)
# We estimate with 95% confidence that a 1% increase in economic growth during the first three quarters of an election year
# will increase the percentage vote in favor of the democratic candidate by between
confint(fit)[2,1] # % and
confint(fit)[2,2] # % if there is a Democratic incumbent.
# The direction of the effect reverses if there is a Republican incumbent.

# b)
f <- makeFun(fit)
# predicted vote | growth=4
f(4)
# If the growth rate is 4 percent, that is good news for a Democratic incumbent and the estimated average vote
# in favor of the incumbent is
f(4) # %.

# An interval estimate is obtained by:
f(4, interval="confidence", level=0.95)
f(4, interval="confidence", level=0.99)
# While the 95% interval suggests a Democratic victory,
# the 99% interval suggests that there is still a chance for a Republican victory.

# c)
library(multcomp)
qt(0.99,fit$df.residual) # critical t
summary(glht(fit, linfct = c("growth <= 0"))) # multcomp::glht, one sided test,  specify the H0 using linfct=
# The calculated value of t is 5.929 which is greater than the critical value.
# We reject the null hypothesis and conclude that growth increases the expected vote share for an incumbent Democrat.
# The p-value is:
test <- summary(glht(fit, linfct = c("growth <= 0")))
test$test$pvalues[1]

# d)
fit2 <- lm(vote ~ inflat, data = fair5)
summary(fit2)
# same as from summary(fit2)
summary(glht(fit2, linfct = c("inflat = 0"))) # multcomp::glht, one sided test,  specify the H0 using linfct=
# This is a two-tail test. For a 1% test the right tail critical value is the 0.995 percentile of the t distribution,
qt(0.995, fit2$df.residual)
# and the left tail critical value its negative this value.
# The calculated value of t is 0.702. It falls between the two critical values in the non-rejection region.
# We fail to reject the null hypothesis and cannot conclude that inflat has a significant effect on the expected vote.
# The p value is:
test <- summary(glht(fit2, linfct = c("inflat = 0")))
test$test$pvalues[1]

# -----------------------------------------------------



# 3.28  

rm(list=ls())

#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/motel.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/motel.rdata"))


motel <- motel %>% mutate(relprice2 = 100*relprice)

favstats(~relprice2, data = motel)
favstats(relprice2~repair, data = motel)

# The relative price of the motel is lower than their competitors over the entire period,
# but significantly lower during repair
motel %>% ggplot(aes(x=time, y=relprice2, color=as.factor(repair), group=1)) + geom_line() + geom_point() +
  geom_hline(yintercept=mean(~relprice2, data = motel)) +
  geom_hline(yintercept=mean(~relprice2, data = filter(motel, repair==1)), color="darkgreen") +
  geom_hline(yintercept=mean(~relprice2, data = filter(motel, repair==0)), color="red")

repair <- lm(relprice2 ~ repair, data = motel)
summary(repair)

# b)
fit <- lm(motel_pct ~ relprice2, data = motel)
summary(fit)

# We estimate that each additional 1% increase in the relative price the average motel occupancy rate falls by
coef(fit)[2] # %. 

# A 95% confidence interval is
confint(fit)[2,]
# This interval is entirely less than zero, so we are 95% confident that there is an inverse relationship. 

# c)
f <- makeFun(fit)
# The 90% interval estimate is
f(80, interval="confidence", level=0.9)
# The interval estimate is about 14% wide which does not pin down the expected occupancy rate very well.
f(80, interval="confidence", level=0.9)[3]-f(80, interval="confidence", level=0.9)[2]

# d) Use 5% level of significance
summary(glht(fit, linfct = c("relprice2 = 0"))) # multcomp::glht, one sided test,  specify the H0 using linfct=
# We do reject the null hypothesis of no relationship and conclude that there is a statistically significant inverse relationship.

# e) Use 5% level of significance
summary(glht(fit, linfct = c("relprice2 = -1"))) # multcomp::glht, one sided test,  specify the H0 using linfct=
qt(0.025,fit$df.residual)
qt(0.975,fit$df.residual)

# This value is in the non-rejection region, so we cannot reject the null hypothesis
# that for each percent higher for the relative price that the motel in question charges,
# it loses one percent of its occupancy rate. The test p-value is 0.708

#' -----------------------------------------------------


