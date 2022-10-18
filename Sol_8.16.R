

# Assignment: 

# 8.16

rm(list = ls())
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/vacation.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/vacation.rdata"))

head(vacation)

#' a)

fit.1 <- lm(miles ~ income + age + kids, data = vacation)
summary(fit.1)


#95% CI
confint(fit.1) #very wide interval 

confint(fit.1)[4,]
# b).

library(mosaic)
library(broom)

augment(fit.1)
fit.metrics <- augment(fit.1)

# Residual vs income 
fit.metrics %>% ggplot(aes(x=income,y = .resid))+geom_point()+
  ggtitle("Residuals vs income")
# Residual vs age 
fit.metrics %>%ggplot(aes(x=age,y = .resid))+geom_point()+
  ggtitle("Resdidual vs age")
#' In the plot of the residuals against income, variation of the residuals increases as income
#' increases, but the same effect is not apparent in the plot of the residuals against age. In this
#' latter case there is no apparent relationship between the magnitude of the residuals and age.
#' Thus, the graphs suggest that the error variance depends on income, but not age.

#' c).

dim(vacation)

vacation.sort <- vacation %>% arrange(income) # for decreasing -income, dplyr

vacation.part1 <- vacation.sort[1:90,]
vacation.part2 <- vacation.sort[111:200,]

dim(vacation.part1)
dim(vacation.part2)

fit.part1 <- lm(miles ~ income + age + kids, data = vacation.part1)
fit.part2 <- lm(miles ~ income + age + kids, data = vacation.part2)

summary(fit.part1)
summary(fit.part2)

#'H0: sigma1^2 = sigma2^2 
#'H1: Sigma2^2 > Sigma1^2
#'where sigma1 and sigma2 are variances for low and high income HHs.

#'The value of the test statistic is:

sigma1 <- deviance(fit.part1)/fit.part1$df.residual
sigma2 <- deviance(fit.part2)/fit.part2$df.residual

F = sigma2/sigma1
F

#The 5% critical value: F(N1-df1,N2-df2)  N1=N2=90, df1=df2=4
qf(0.95,86,86)

#' Thus, we reject H0 and conclude that the error variance depends on income



#' d).

fit.2 <- lm(miles ~ income + age + kids, data = vacation)
summary(fit.2)

library(sandwich)
fit.2 %>% vcovHC(type = c("HC1")) %>% diag() %>% sqrt()

library(lmtest)

#' OLS estimates and White HCE standard errors 
coeftest(fit.2,vcov = vcovHC,type = c("HC1"))

#'We find that the robust standard errors are larger for all the
#'coefficients except the constant term.
#'The coefficients are all significant at the 1% level. 

#' Now we can create a CI using car::Confint()
library(car)
Confint(fit.2,vcov.=hccm(fit.2,type = c("hc1")))
#OLS
confint(fit.2)

#'The 95% interval estimate for the effect of another child is [-139.323, -24.32986], 
#'which is of course slightly wider than using the conventional OLS standard errors.
confint(fit.2)

#Note here, if we use the following, it will give you same CI as that of OLS
confint(fit.2,vcov.=hccm(fit.2,type = c("hc1")))


# e).

fit.gls <- lm(miles ~ income + age + kids, weights = I(1/income^2),data = vacation)
summary(fit.gls)

#Ols 
summary(fit.2)

#' 95% CI

#' Using conventional GLS
confint(fit.gls)

#' Using the robust GLS
Confint(fit.gls,vcov.=hccm(fit.gls,type = c("hc1")))




