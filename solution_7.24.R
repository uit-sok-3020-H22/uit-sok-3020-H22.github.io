


#' 7.24  /Difference-in-Difference Regressions


rm(list=ls())

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/means.def")

#' Read the data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/means.rdata"))

#' lnprice = the log of average home price
#' lnunits = the log of the number of housing units 
#' izlaw =1, cities with inclusionary policy, 
#' izlaw=0, cities without the policy, 
#' 
#' The treatment variable(izlaw) is an indicator variable, izlaw =1 if obs. or 
#' cities with inclisionary policy, and zero if not. 
#' The time indicator variable is d=1 ,for year 2000


#' a.

fit.price <- lm(lnprice ~ d + izlaw + izlaw_d, data = means)
summary(fit.price)
#' The treatment effect is estimated by the 
#' coefficient of D × IZLAW, or the variable IZLAW_D.

#' Here, we estimate that the result 
#' of the law was to increase prices by about 33.3%, or
#'  (using the exact calculation), it will be 
100*(exp(coef(fit.price)[4])-1)
#' %,, and
#' this effect is statistically significant at the 5% level (t = 2.355).

fit.units <- lm(lnunits ~ d + izlaw + izlaw_d, data = means)
summary(fit.units)
#' For the LNUNITS equation the effect carries a negative sign,
#' which is opposite the direction we expect,
#' but the coefficient is not statistically different from zero, 
#' so that its sign should not be interpreted.

#' To summarize, these models suggest that the policy effect is 
#' to increase prices, but not to increase the number of housing units,
#' contrary to the intention of the policy.


#' b. To the regression in (a) add lmedhhinc = log(median household income )

fit.price.2 <- lm(lnprice ~ d + izlaw + izlaw_d + lmedhhinc, data = means)
summary(fit.price.2)

# Holding other variables constant,we estimate that a 1% increase in the 
# households' median income,increases the price of housing by 1.3 percent.

#' This effect is statistically significant with a t-value of 34.357.
#' The inclusion of this control variable reduces the magnitude of the
#'  estimated treatment effect to approximately
100*(exp(coef(fit.price.2)[4])-1)
#' %. The treatment effect is statistically significant at the 1% level.

fit.units.2 <- lm(lnunits ~ d + izlaw + izlaw_d + lmedhhinc, data = means)
summary(fit.units.2)
#' In the LNUNITS equation the median income variable is not 
#' statistically significant and the estimate of the treatment 
#' effect remains statistically insignificant.


#' c. To the regression in (b) add 
#'                 100(education)= proportion w/college degree 
#' ,               100(proppoverty) = proportion below poverty level, and 
#'                 Lop = log of population in 1000s 

fit.price.3 <- lm(lnprice ~ d + izlaw + izlaw_d + lmedhhinc + educattain + proppoverty + lpop, data = means)
summary(fit.price.3)
#' In the LNPRICE equation the effects are:
#' EDUCATTAIN: Holding all else constant, we estimate that an increase 
#' in the proportion of the population holding a college degree will 
#' increase prices by a statistically significant amount.
#' If there is an increase in the proportion by 1% the estimated 
#' increase in house prices is 1.94%.

#' PROPOVERTY: Holding all else constant, an increase in the proportion 
#' of the population in poverty decreases house prices by a statistically
#' significant amount.If the poverty rate increases by 0.01, or 1%, 
#' we estimate that house prices will fall by 0.51%.

#' LPOP: Holding all else constant, an increase in the population 
#' of 1% is estimated to increase house prices by 0.039 %.
#' This effect is statistically significant at the 1% level.
#' The addition of these additional controls slightly reduces 
#' the estimated treatment effect to
100*(exp(coef(fit.price.3)[4])-1)
#' %. The treatment remains statistically significant at the 1% level.

#' The inclusion of these control variables does not alter the 
#' insignificance of the treatment effect.

fit.units.3 <- lm(lnunits ~ d + izlaw + izlaw_d + lmedhhinc + educattain + proppoverty + lpop, data = means)
summary(fit.units.3)

#' In the LNUNITS equation the effects are:
#' EDUCATTAIN: We estimate, that holding other factors fixed, an increase
#' in the percent of the population with a college degree increases 
#' by 0.01, or 1%, the number of housing units will
#' increase by 1.34 percent, which is significant at the 1% level.

#' PROPOVERTY: We estimate that holding other factors constant, an 
#' increase of the proportion living in poverty of 0.01, or 1%, 
#' is associated with a decrease of housing units of 2.62%, and
#' this effect is significant at the 1% level.

#' LPOP: Holding all else constant, we estimate that a 1% increase
#' in population is associated with a 0.998% (or about 1%) increase
#' in housing units. Again this effect is strongly significant.

#' The inclusion of these control variables does not alter the 
#' insignificance of the treatment effect.
#' There is no evidence that the policy increased the number 
#' of housing units.

#' d.

#' Doing the subtraction we have
#' ln(PRICE,2000) - ln(PRICE, 1990) = b3 + delta* IZLAW + (e(2000) - e(1990))
#' The unobserved CITY effect is eliminated, and the coefficient of IZLAW is the treatment effect.
#' The intercept of the differenced equation is the coefficient of the indicator variable D.

#' e.

summary(fit.price)
fit.4 <- lm(dlnprice ~ izlaw, data = means)
summary(fit.4)

#' The estimated regression is shown in the output above. 
#' The number of observations is cut in
#' half in the latter difference-in-difference model, but comparing
#'  the results to the first model we see that the coefficient
#' estimates are identical (look at IZLAW_D in the first model 
#' and IZLAW in the second).
#' The standard error is smaller in the differenced equation and 
#' the t-stat larger,meaning that our conclusion about the positive
#' effect of the treatment on prices is unchanged.


#