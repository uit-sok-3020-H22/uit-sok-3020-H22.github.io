rm(list=ls())

# Exercise 8.19

#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/malawi_small.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/malawi_small.rdata"))

# malawi_small.def
# 
# totexp pclothes pfood pfoodaway pfuel phealthcare prent pschool ptelephone pvice
# 
# Obs: 1200
# 
# This is a subset of malawi.dat
# 
# The data in malawi.dat are extracted from the World Bank Survey: Malawi - Schooling, Income,
# and Health Risk Impact Evaluation Household Survey 2007-2008, Round I, file = SIHR1_PI_S5_public,
# produced by Sarah Baird, George Washington University, Craig McIntosh, University of California,
# San Diego, and Berk Ozler, World Bank.
# 
# Any errors are the responsibility of the authors of this textbook.
# 
# variable  	label
# ----------------------------------------------------------------------------------------
# totexp          Total household expenditure last month, in thousands of Malawian Kwacha
# pclothes        Proportion of last month's totexp spent on clothes
# pfood           Proportion of last month's totexp spent on food consumed at home
# pfoodaway       Proportion of last month's totexp spent on food consumed outside home
# pfuel           Proportion of last month's totexp spent on fuel
# phealthcare     Proportion of last month's totexp spent on healthcare
# prent           Proportion of last month's totexp spent on housing rent
# pschool         Proportion of last month's totexp spent on school expense
# ptelephone      Proportion of last month's totexp spent on telephone
# pvice           Proportion of last month's totexp spent on tobacco and beer

library(mosaic)

# a. ---------------------------------------

malawi <- malawi_small %>% filter(ptelephone>0) %>% 
  mutate(ln_telephone=log(ptelephone*totexp),
         ln_totexp=log(totexp))

ggplot(malawi, aes(x=ln_totexp, y=ln_telephone)) + geom_point() + geom_smooth(method = lm, se=TRUE, color="red") +
  xlab("log(Total household expenditure last month, in thousands of Malawian Kwacha)") +
  ylab("log(Totexp household expenditure on telephone last month)") +
  ggtitle("Test")

#' There is a positive relationship between the log of telephone expenditure and the log of total household expenditure.

# Alternatively 
fit <- lm(ln_telephone~ln_totexp, data = malawi)
summary(fit)

plotModel(fit) + xlab("log(Total household expenditure last month, in thousands of Malawian Kwacha)") +
  ylab("log(Totexp household expenditure on telephone last month)")



# b. ---------------------------------------

#' This is a log-log model ln(TELEPHONE) = b1 + b2 ln(TOTEXP) + e the coefficient on b2 is the elasticity. 
coef(fit)[2]
#' Because this value is larger then one, we would classify it as a luxury good.
#' The 95% interval estimate is
confint(fit)[2,]
#' which does include some values less than one so that we could not reject the null hypothesis that
#' the elasticity is 1 using a 2-tail test at the 5% level of significance.

library(car)
linearHypothesis(fit, "ln_totexp = 1")

# c. ---------------------------------------

#' Detecting Heteroscedasticity
library(lmtest)

#' The H0 is homoscedastic errors
bptest(fit, studentize = FALSE) # B-P or LM test
ncvTest(fit) # package car, same test

#' Chritical chi square value
qchisq(0.05, df=1, ncp = 0, lower.tail = FALSE, log.p = FALSE)

#' Plot of the p-value of the heteroscedasticity test
test <- ncvTest(fit)
test$ChiSquare
xpchisq(test$ChiSquare,df=1)

# White test
# second order
bptest(lm(ln_telephone~ln_totexp+I(ln_totexp^2), data = malawi))

#' We fail to reject the null hypothesis of homoskedasticity at the 5% level in all tests.

# second and third order 
bptest(lm(ln_telephone~ln_totexp+I(ln_totexp^2)+I(ln_totexp^3), data = malawi))

# d. ---------------------------------------

fit2 <- lm(ptelephone~log(totexp), data = malawi)
summary(fit2)

library(multcomp)
#' One sided test
summary(glht(lm(ptelephone~ln_totexp, data = malawi), linfct = c("ln_totexp <= 0"))) # H0: b2 <= 0
# Two sided test
linearHypothesis(lm(ptelephone~ln_totexp, data = malawi), "ln_totexp = 0") # divide p-value in 2 to get 1 tail p-value

#' We conclude that there is a positive relationship between the proportion of telephone
#' expenditures and total expenditures

# e. ---------------------------------------

#' Elasticity at sample mean is:

f <- makeFun(fit2)
e <- function(x) {(coef(fit2)[1]+coef(fit2)[2]*(log(x)+1))/f(x)}
median(~totexp, data = malawi)
e(median(~totexp, data = malawi))

#' Using the deltaMethod from `car` package to find se(e) and a 95% interval
deltaMethod(fit2, "(b1+b2*(log(12)+1))/(b1+b2*log(12))", parameterNames= paste("b", 1:2, sep="")) 
#' The point estimate of the elasticity is slightly larger and the interval estimate shifted rightward.
coef(fit)[2];confint(fit)[2,]

# f. ---------------------------------------

#' The H0 is homoscedastic errors
bptest(fit2, studentize = FALSE) # B-P or LM test

#' At a 5% level we fail to reject homoskedasticity

# White test
# second order
bptest(lm(ptelephone~ln_totexp+I(ln_totexp^2), data = malawi))

#' At a 5% level we fail to reject homoskedasticity

# g. ---------------------------------------

library(broom)
# If we save the broom::augment() as data
summary(fit2)
fit.metrics <- augment(fit2)

resid(fit2)
# Estimating the variance function, p. 382
fit.metrics <- mutate(fit.metrics, e2=log(.resid^2))

fit2.var <- lm(e2~log.totexp., data = fit.metrics)
summary(fit2.var)

# Calculating the correction h() function
sigma2=exp(predict(fit2.var))

# Then do Weighted Least Squares model, eqtn. 8.21
fit2.feasible.gls <- lm(ptelephone~ln_totexp, weights=I(1/sigma2), data=malawi)
summary(fit2.feasible.gls)

#' One sided test
summary(glht(fit2.feasible.gls, linfct = c("ln_totexp <= 0"))) # H0: b2 <= 0
#' We fail to conclude that there is a positive relation between the proportion of expenditures
#' and the log of total expenditures.

#' While not required, the elasticity based on the FGLS estimates is
deltaMethod(fit2.feasible.gls, "(b1+b2*(log(12)+1))/(b1+b2*log(12))", parameterNames= paste("b", 1:2, sep="")) 

# h. ---------------------------------------

library(sandwich)

# These are the White HCE corrected standard errors, POE5 p. 374
sqrt(diag(vcovHC(fit2, type = c("HC1")))) 

# OLS estimates and White HCE standard errors
coeftest(fit2, vcov=vcovHC, type = c("HC1") )

# i. ---------------------------------------

#' Using a log-log model, or the proportion-log model, the estimated elasticity of phone
#' expenditures with respect to total expenditures is close to one.
#' Heteroscedasticity does not seem to be a problem in the proportion-log model, and
#' correcting for for it made parameter estimates more imprecise.

