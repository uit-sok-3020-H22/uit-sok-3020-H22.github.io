
# Exercises: 5.18 , 5.22, 5.23

#' 5.18

rm(list=ls())
library(mosaic)

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/london5.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/london5.rdata"))
head(london5)

# a). wfood = b1 +b2*ln(totexp)+ef
#     wcloth = a1 + a2*ln(totexp)+ec
#' A commodity is regarded as luxury if the coeff of ln(totexp) is 
#' positive and a necessity if it is negative. 
# what signs do you expect for b2 and a2?

#Ans: Since both food and clothing are necessities rather than luxuries.
#we would expect both b2 and a1 to have negative signs. 

# b).
london5$nk

london5_1 <- london5 %>% filter(nk==1)

fit_food <- lm(wfood ~ log(totexp), data =london5_1 )
summary(fit_food)

fit_cloth <- lm(wcloth ~ log(totexp), data =london5_1 )
summary(fit_cloth)

#' The coefficient of ln(totexp)  in the equation for food has
#'  the expected negative sign, but the corresponding coefficient 
#'  in the clothing equation is positive, contrary to expectations. 

#' The p-values indicate that the estimates are significantly 
#' different from zero at all conventional significance levels. 

# c). Hypothesis test, 
#'  H0: beta >=0 vs  H1: beta < 0, alpha=1%
#( Note that the summary function gives you a two-tail test )

#' We would set up the hypotheses in this way if we wished to 
#' establish that food was a necessity. 

library(FSAmisc)
hoCoef(fit_food, term = 2, bo = 0, alt = c("less")) 
#conclusion: reject H0

# Alternatively, 
london5_1 <- london5_1 %>% mutate(ltotexp = log(totexp))
fit1_food <- lm(wfood ~ ltotexp, data =london5_1 )
summary(fit1_food)

#check with the above model 
summary(fit_food)

# now we can use glht function for the hypothesis testing
library(multcomp)
summary(glht(fit1_food,linfct = c("ltotexp>=0")))

#Alternatively 
#library(car)
#deltaMethod(fit1_food, "b2", parameterNames= paste("b", 1:2, sep=""))


# d). Hypothesis test, H0: alpha >=0, H1: alpha <0

hoCoef(fit_cloth, term = 2, bo = 0, alt = c("less")) 
# conclusion: we fail to reject Ho 

#Alternatively 
fit1_cloth <- lm(wcloth ~ ltotexp, data =london5_1 )
summary(glht(fit1_cloth,linfct = c("ltotexp>=0")))


# e)

head(london5)

london5_2 <- london5 %>% filter(nk==2)

fit_food2 <- lm(wfood ~ log(totexp), data =london5_2 )
summary(fit_food2)

fit_cloth2 <- lm(wcloth ~ log(totexp), data =london5_2 )
summary(fit_cloth2)


# 95% interval estimates 

#  1 child HHs
confint(fit_food)
confint(fit_cloth)

#2-child HHs
confint(fit_food2)
confint(fit_cloth2)


#' For both food and clothing, the interval estimates overlap
#' suggesting the coefficients could be the same, particularly 
#' for clothing. A simple inspection of interval estimates is not 
#' definitive, however.


# f) use all observations to estimate

# wfood = a1 +a2 ln(totexp)+a3 nk + a4 nk*ln(totexp) +e
# wcloth = a1 +a2 ln(totexp)+a3 nk + a4 nk*ln(totexp) +e

head(london5)
fit_food3 <- lm(wfood ~ log(totexp) + nk+ nk*log(totexp), data =london5 )
summary(fit_food3)

fit_cloth3 <- lm(wcloth ~ log(totexp) + nk + nk*log(totexp), data =london5 )
summary(fit_cloth3)

# A 95% significance level 

#' For both commodities, to test whether the coefficient of totexp 
#' is the same for both one-child and two-child households,
#' we test whether the coefficient of NK*totexp is significantly 
#' different from zero.

#For Food, H0: gamma_4 =0 vs H1: gmma_4 = ! 0 

# look at the t-static and p-value from the summary of the model
# since we have a two tail test 
#conclusion: reject H0

#For clothing, H0: gamma_4 =0 vs H1: gamma_4 = ! 0 
#conclusion: fail to reject H0
#' i.e., There is no evidence to suggest that, for the clothing 
#' equation, one- and two-child households have different 
#' coefficients for log(totexp).
#' 


# g). mathematical derivation, go by yourself





#' 5.22

rm(list=ls())

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/toody5.def")

# toody5.def
# 
# y  trend rain year t
# 
# Obs:   48 annual observations for 1950-1997
# 
# y = wheat yield in tonnes per hectare for the Toodyay 
#     Shire of Western Australia in year t
# trend= is a trend term designed to capture technological change with 
#        1950=0, 1951=0.1, 1952=0.2, 1953=0.3, ..., 1960=1, ..., 1997=4.7
# rain = is a total rainfall over the growing season (May to october)
#       measured in decimeters.
# t		a second trend term 1950=1, 1951=2,..., 1997=48

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/toody5.rdata"))
head(toody5)

#' y_t =b1 +b2*Trend + b3*rain + b4*rain^2 +b5*(rain*Trend) +et 

library(mosaic)

#'  a.

#'Directly 
fit1 <- lm(y ~ trend + rain + I(rain^2)+rain*trend, data = toody5)
summary(fit1)
library(stargazer)
stargazer(fit1, type="text", intercept.bottom=FALSE)

# Alternatively 
#' Generate new variables first:
toody5 <- toody5 %>% mutate(rain2 = rain^2, rain_trend = rain*trend)
fit <- lm(y ~ trend + rain + rain2 + rain_trend, data = toody5)
summary(fit)


#' b.

#' Extra rainfall will increase expected yield if the marginal effect 
#' of rainfall is positive.

#' The marginal effect is: d(y)/d(rain) = b3 + 2 b4 rain + b5 trend
#' For 1974, find the marginal effect of rain
head(toody5)
toody5 %>% filter(dateid01=="1974-01-01")

library(multcomp)
#' One sided test, inserts values from 1974
#' H0: b3 + 2 b4 rain + b5 trend <= 0, H1: b3 + 2 b4 rain + b5 trend > 0
summary(glht(fit, linfct = c("rain + 2*4.576*rain2 + 2.4*rain_trend <= 0"))) # H0
#' We fail to reject H0. 
#' There is insufficient evidence to show that extra rainfall
#' will increase expected yield in 1974.

#' c.

toody5 %>% filter(dateid01=="1960-01-01")
#' In 1960 when trend=1, 
#' E(y|rain) = b1 + b2 + b3 rain + b4 rain2 + b5 rain

toody5 %>% filter(dateid01=="1995-01-01")
#' In 1995 when trend=4.5, 
#' E(y|rain) = b1 + 4.5 b2 + b3 rain + b4 rain2 + 4.5 b5 rain
#' The expected improvement (IMP) in wheat yield over this period 
#' (1960 to 1995, trend increases from 1 to 4.5,
#' or by 3.5) from technological change is:
#' IMP = E(d(y)|rain) = 3.5 b2 + 3.5 b5 rain
#' The estimated expected improvement when rain is 3.8355 (median) is
# 3.5*b2 + 3.5*b5*rain
summary(glht(fit, linfct = c("3.5*trend+13.42425*rain_trend = 0"))) # H0

#' A 95% interval estimate for the expected improvement in 
#' yield from technological change is 
library(car)
deltaMethod(fit, "3.5*b2+13.42425*b5", parameterNames= paste("b", 1:5, sep="")) 

#' d.

#' Estimate the equation:rain = a1 +a2*trend +e to test a decline in rain fall over time 

#' The estimated trend for rainfall is
fit2 <- lm(rain ~ trend, data = toody5)
summary(fit2)

summary(glht(fit2, linfct = c("trend >= 0"))) # H0
#' We reject H0 and conclude that mean rainfall is declining over time

plotModel(lm(rain ~ trend, data = toody5))

#' e.
f <- makeFun(fit2)
toody5 %>% filter(dateid01=="1960-01-01")
#' E(rain|trend=1) in 1960:
f(trend=1)

toody5 %>% filter(dateid01=="1995-01-01")
#' E(rain|trend=1) in 1995:
f(trend=4.5)

#' The differenc in mean is:
f(trend=1)-f(trend=4.5)
#' Or expressed as a hypothesis
summary(glht(fit2, linfct = c("3.5*trend = 0"))) # H0

#' f.

#' If there has been no technological change from 1960 to 1995, i.e., keeping trend constant (=1)
#' then expected yield in both these years, conditional on rainfall, is
#' E(y|rain) = b1 + b2 1  + b3 rain + b4 rain2 + b5 rain 1
#' The change in mean yield attributable to climate change is
#' d E(y) = E(y| rain = 3.707899) - E(y| rain = 4.240694), this equals 
#' d E(y) = (b3+b5) (3.707899 - 4.240694) + b4 (3.707899^2 - 4.240694^2)
3.707899 - 4.240694
3.707899^2 - 4.240694^2
#' A 95% interval estimate for the expected yield due to climate change is 
car::deltaMethod(fit, "(b3+b5)*-0.532795 + b4*-4.234971", parameterNames= paste("b", 1:5, sep="")) 
#' We estimate the decline in yield attributable to climate change to be -0.08885963 tons per hectare.
#' It is a significant decline at a 5% level of significance.

#' g.

#' If E(rain,1995) = E(rain,1960) = 4.240694, then the trend in yield is given by
#' E(y|trend) = b1 + b2 trend + 4.240694 b3 + 4.240694^2 b4 + 4.240694 b5 trend
#' The change in mean yield attributable to technological change is
#' d E(y,trend) = E(y|trend = 4.5) - E(y|trend = 1)
#' the estimate is
car::deltaMethod(fit, "3.5*b2 + 4.240694*3.5*b5", parameterNames= paste("b", 1:5, sep="")) 
#' We estimate the increase in yield attributable to technological change is 0.995412 tons per hectare
#' It is a significant at a 5% level of significance.

#' h.

#' The estimated improvement in mean wheat yield from technological change was greater for
#' the median rainfall of 3.8355dm than it was for the higher 1960 rainfall of 4.2407dm
#' The two yield estimates are 1.127073 and 0.995412, respectively.
#' The likely cause is that, in 1995, the yield-maximizing rainfall is less than 4.2407. 











