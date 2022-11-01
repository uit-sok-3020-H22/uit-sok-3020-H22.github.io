

#' 6.18

rm(list=ls())

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/rice5.def")

# rice5.def
# 
# firm  year  prod  area  labor  fert
# 
# Obs:   a panel with 44 firms over 2 years (1993-1994)
# total observations = 88
# 
# firm	Firm number  ( 1 to 44)
# year	Year = 1993 to 1994
# prod	Rice production (tonnes)
# area	Area planted to rice (hectares)
# labor	Hired + family labor (person days)
# fert	Fertilizer applied (kilograms)

library(mosaic)
library(broom)
library(car)

#' Read the data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/rice5.rdata"))

#' Filter out 1994
rice94 <- rice5 %>% filter(year==1994)

#' Estimate model
fit <- lm(log(prod) ~ log(area) + log(labor) + log(fert), data = rice94)

#' a. i. & ii. 
summary(fit)
confint(fit)

tidy(fit)
glance(fit)
vif(fit)

#' iii. Joint Hypothesis, 6.7

#' Ho: b2 = b3 = 0 
joint.hyp <- c("log(area)=0","log(labor)=0")
linearHypothesis(fit, joint.hyp)

#' The t-test indicate that when b2 and b3 are tested separately,
#' we are unable to reject these null hypotheses at a 5% significance level.
#' However, when b2 and b3 are tested jointly, we reject the null hypothesis at a 1% significance level.
#' These results suggest ln(AREA) and ln(LABOR) are important variables,
#' but they are very collinear, as the VIF values show.
#' The correlation between these two variables is
cor(log(area) ~ log(labor), data = rice94)

#' b. Math covered in textbook

#' c.

fit2 <- lm(log(prod/area) ~ log(labor/area) + log(fert/area), data = rice94)
summary(fit2)

#' Using the deltaMethod from `car` on the Restriced model to recover the coefficient on b2 (in the original model)
#' and its standard error
deltaMethod(fit2, "1-b2-b3", parameterNames= paste("b", 1:3, sep="")) 

#' Alternatively
rice94 <- rice94 %>% mutate(ln_prod_area=log(prod/area),
                            ln_labor_area=log(labor/area),
                            ln_fert_area=log(fert/area))

fit2.alt <- lm(ln_prod_area ~ ln_labor_area + ln_fert_area, data = rice94)
linearHypothesis(fit2.alt, "1 - ln_labor_area - ln_fert_area = 0")

#' d.

#' Use all data
#' Estimate model without constant-returns-to-scale 

fit.wo.crs <- lm(log(prod) ~ log(area) + log(labor) + log(fert), data = rice5)
fit.with.crs <- lm(log(prod/area) ~ log(labor/area) + log(fert/area), data = rice5)

summary(fit.wo.crs)
summary(fit.with.crs)

confint(fit.wo.crs)
confint(fit.with.crs)
deltaMethod(fit.with.crs, "1-b2-b3", parameterNames= paste("b", 1:3, sep="")) 

#' The major change in the estimates from imposition of constant returns to scale
#' is the increase in the elasticity of production with respect to labor which
#' has increased from 0.39967 to 0.48824.
#' The interval estimate has shifted in a corresponding way.
#' There has been a slight decrease in the point estimate and a shift to the left
#' of the interval estimate for the elasticity of production with respect to fertilizer.
#' The standard errors for these two elasticities have decreased slightly
#' after imposing constant returns to scale. There has been little change in the results for AREA

vif(fit.wo.crs)
vif(fit.with.crs)

#' Multicollinearity is no longer a problem when imposing CRS. 






#' 6.27

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/cps5_small.def")

#' Read the data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/cps5_small.rdata"))

head(cps5_small)

# a). 
library(mosaic)
m1 <- lm(log(wage) ~educ + exper, data = filter(cps5_small,educ >7))
summary(m1)

#' Joint hypothesis testing 
#' The hypothesis for the claim about the marginal effects
#' of education and experience are:
#' H0: b2 =0.112, b3=0.008 vs H1: b2!=0.112,or b3 !=0.008
library(car)
linearHypothesis(m1, c("educ = 0.112","exper=0.008"))

#' Conclusion: since the p = 0.678 greater than alpha=5%, we keep H0

#' b).

#' RESET Test
library(lmtest)

#' Augmenting the equation with the squares of the predictions 
resettest(m1, power=2, type="fitted")
#'Since the p-value (0.01139) is less that alpha = 5%,
#'we conclude that the squares of the predictions have 
#'significant explanatory power and hence the model is inadequate.

#' Augmenting the equation with the squares and cubes of the predictions, 
resettest(m1, power=2:3, type="fitted")

#' Again here, we conclude that the squares and cubes of the 
#' predictions have significant explanatory power and hence 
#' the model is inadequate.


#' c).
cps5_small_new <- cps5_small %>% mutate(educ2 = educ^2,
                                        exper2 = exper^2,
                                        educ_exper = educ*exper)
m2 <- lm(log(wage) ~ educ + exper + educ2 + exper2+ educ_exper, data = filter(cps5_small_new,educ >7))
summary(m2)


#' Look the solution guide how the marginal effects of
#' education and experience are derived. 


#' i) b2 +20*b4+5*b6 =0.112
#'    b3 + 10*b5 + 10*b6 = 0.008 

linearHypothesis(m2, c("educ + 20*educ2 + 5*educ_exper = 0.112", 
                       "exper + 10*exper2 + 10*educ_exper = 0.008"))


#' ii). b2 + 28*b4 + 24*b6 =0.112
#'      b3 +48*b5+14*b6 =0.008 

linearHypothesis(m2, c("educ + 28*educ2 + 24*educ_exper = 0.112", 
                       "exper + 48*exper2 + 14*educ_exper = 0.008"))

#' iii). b2 + 36*b4 + 40*b6 =0.112
#'       b3 +80*b5+18*b6 =0.008 

linearHypothesis(m2, c("educ + 36*educ2 + 40*educ_exper = 0.112", 
                       "exper + 80*exper2 + 18*educ_exper = 0.008"))


#' d). RESET Test

#' Augmenting the equation with the squares of the predictions 
resettest(m2, power=2, type="fitted")
#'Since the p-value (0.693) is greater than alpha = 5%,
#'we conclude that the squares of the predictions do not have 
#'significant explanatory power: the test does not suggest the model
#'is inadequate.

#' Augmenting the equation with the squares and cubes of the predictions, 
resettest(m2, power=2:3, type="fitted")

#' Here, we concluded that the squares and cubes of the 
#' predictions do not have significant explanatory power; the test does not 
#' suggest the model is inadequate.


# e). 
#' The claim is not true for all levels of education and experience. 
#' The medians for EDUC and EXPER are 14 and 23, respectively, 
#' values that approximately correspond to those tested in part (ii). 
#' Thus, the claim is reasonable for those at the median levels of 
#' education and experience, but it does not hold for values of EDUC
#'  and EXPER which are much greater or less than the medians.




# 7. 17

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/bweight_small.def")

#' Read the data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/bweight_small.rdata"))

head(bweight_small)

library(mosaic)

#' a). 

t.test(bweight ~mbsmoke, data = bweight_small)

#' Since the p-value =7.409e-11 is less than alpha = 5%, 
#' we reject H0 that the birthweights of smoking and nonsmoking 
#' mothers are the same. 

#' b).

m1 <- lm(bweight ~ mbsmoke, data = bweight_small)
summary(m1)

#' It is not wise to consider this regression
#' a basis for estimating an average treatment effect.
#'  It would be a consistent estimator only if
#' women were randomly assigned to smoking and non-smoking groups.

library(multcomp)
# H0: b2 >=0 vs H1: b2 <0
summary(glht(m1, linfct = c("mbsmoke >=0")))

#' The one tail critical value is 
qt(0.05,length(bweight_small$mbsmoke))
#' We conclude that women who smoke have significantly 
#' lower birthweight babies, with the estimated reduction being 288 grams, 
#' or about 10.16 ounces. 

# c).
names(bweight_small)

m2 <- lm(bweight ~ mbsmoke + mmarried + mage + prenatal1 + fbaby , data = bweight_small)
summary(m2)
#' The coefficient on the indicator that the
#' mother smokes increases in magnitude to -229.9757, with t = -5.13.
#' This is a difference of 58.2089 grams, or about 2 ounces.
#' Of the other variables only MMARREID is significant. It has a positive
#' sign suggesting that mothers who are married have higher birthweight babies. 
#' This is plausible.

# The joint F-test of these control variables:
library(car)
linearHypothesis(m2, c("mmarried=0","mage=0","prenatal1=0", "fbaby=0"))

#' The p-value is less than alpha=5%.
#' So collectively we can say at least one of the 
#' coefficients helps explain the outcome.


#' d). 

# Regression for mbsmoke =1
bweight_smoke <- lm(bweight ~  mmarried + mage + prenatal1 + fbaby , data = filter(bweight_small,mbsmoke ==1))
summary(bweight_smoke)

# Regression for mbsmoke =0 
bweight_not_smoke <- lm(bweight ~  mmarried + mage + prenatal1 + fbaby , data = filter(bweight_small,mbsmoke ==0))
summary(bweight_not_smoke)


# Testing the equivalence of two regressions, chow test

#' Create interaction variables between all the explanatory variables and the indicator variable mbsmoke
bweight_small <- bweight_small %>% mutate(mmarried.mbsmoke = mbsmoke*mmarried,
                                          mage.mbsmoke = mbsmoke*mage,
                                          prenatal1.mbsmoke = mbsmoke*prenatal1,
                                          fbaby.mbsmoke = mbsmoke*fbaby)
fit <- lm(bweight ~  mmarried + mage + prenatal1 + fbaby + mbsmoke + mmarried.mbsmoke + mage.mbsmoke + prenatal1.mbsmoke + fbaby.mbsmoke , data = bweight_small)
summary(fit)

joint.hyp <- c("mbsmoke=0",
               "mmarried.mbsmoke=0",
               "mage.mbsmoke=0", 
               "prenatal1.mbsmoke=0",
               "fbaby.mbsmoke=0")
library(car)
linearHypothesis(fit, joint.hyp)

# Critical value 
qf(0.95,5,1190)

#' Thus, we reject the null hypothesis. And conclude that there is 
#' some difference in the coefficients of the two equations 


































