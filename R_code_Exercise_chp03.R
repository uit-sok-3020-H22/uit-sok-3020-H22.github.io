
rm(list = ls())


# 3.19 


# data defination 
browseURL("http://www.principlesofeconometrics.com/poe5/data/def/motel.def")

# load the data

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/motel.rdata"))
head(motel)

# a). Plot motel_pct and comp_pct vs time 

library(mosaic)       # dev.off() or graphics.off()

motel %>% ggplot(aes(x=time)) +
  geom_line(aes(y=motel_pct, color="motel_pct"), lwd=1) +
  geom_line(aes(y=comp_pct, color = "comp_pct"), lwd=1) +
  xlab("time") + ylab("Occupancy Rate") + 
  ggtitle("Plot of Occupancy Rate") 

#' Estimate the regression model 
#'  motel_pct ~beta_1 + beta_2 comp_pct +e 
fit1 <- lm(motel_pct ~comp_pct, data = motel)
summary(fit1)

# Construct 95% interval estimate or CI
confint(fit1, level = 0.95)


# b).

#' 90% interval estimate of the expected occupancy rate
#' of the model in question, motel_pct, given comp_pct =70 
f=makeFun(fit1)
f(70)
f(70, interval="confidence", level=0.9)
#f(70, interval="predict")

#c).
#' In the model a) above, test the hypothesis
#' H0: beta_2 <=0, vs H1: beta >0, alpha =0.01.
library(multcomp)
summary(glht(fit1,linfct = c("comp_pct <=0")))

#compare 
summary(glht(fit1,linfct = c("comp_pct <=0")))$test$tstat > qt(0.99,df=length(motel$motel_rate)-2)
# conclusion: reject Ho

#' d)
#' Test H0: beta_2 =1, vs H1: beta =! 1, alpha =0.01.
summary(glht(fit1,linfct = c("comp_pct =1")))

#compare 
summary(glht(fit1,linfct = c("comp_pct =1")))$test$tstat > abs(qt(c(0.005,0.995),df=length(motel$motel_rate)-2))
# conclusion: keep Ho

# e)
#' Calculate the least square residuals from the regression 
#' above and plot them against time.
#' Are there unusal features to the plot? 
#' What is the predominant sign of the residuals 
#' during the time period 17-23 
residuals <- resid(fit1)
#or 
motel$residuals <- fit1$residuals


# Include the residuals in the data frame.
#library(broom)
#motel.long <- augment(fit1)
#motel.long %>% ggplot(aes(x=time, y=.resid))+geom_point()

motel %>% ggplot(aes(x=time, y=residuals))+geom_point()
motel %>% ggplot(aes(x=time, y=residuals))+geom_line()+
  geom_hline(yintercept=0, linetype=3, lwd=2)+
  geom_vline(xintercept=c(as.numeric(motel$time[17]),as.numeric(motel$time[23])), linetype=2)

################################################################

# 3.20 

#' a) calculate the sample average occupancy rate for the 
#' motel during the time when there were no repair being 
#' made and during repair. How big a difference is there 

mean(motel_pct ~ repair, data = motel)
diffmean(motel_pct ~ repair, data = motel)

#' The average motel occupancy rate during no repair 
#' periods was 79.35%. 
#' When repairs were being made the average occupancy 
#' rate was 66.11%, which is a reduction of 13.24 
#' percentage points.

#' b) Estimate the model: motel_pct ~ sigma_1 +sigma_2*repair +e 
#' What are the estimated coeff.? how they are related to a)

#' Repair is an indicator variable taking the value 1 
#' during repair and 0 otherwise. 

fit <- lm(motel_pct ~repair, data = motel)
summary(fit)

#' Note that the estimated intercept is the average occupancy
#' rate during the non-repair period, and the estimated 
#' coefficient of REPAIR is the difference in the average 
#' occupancy rates for the two periods.

#' c). construct a 95% interval estimate for the parameter
#' sigma_2 and give interpretation. Precise estimate or not?

confint(fit,level = 0.95)

#' With 95% confidence, we estimate that during the repair 
#' period there was between a 25.6% and a 0.91% reduction 
#' in the expected motel occupancy rate. This interval is 
#' fairly wide, but it does not include zero or any positive
#'values. We conclude that the motel likely did suffer a loss
#' of occupancy during the repair period.

# d).Test Ho: sigma >=0, vs H1: sigma_2 <0
library(multcomp)
summary(glht(fit,linfct = c("repair >=0")))

summary(glht(fit,linfct = c("repair >=0")))$test$tstat < qt(0.05,df=length(motel$motel_pct)-2)
#conclusion: reject H0
# or also look at the p-value 

#' If we reject the null hypothesis we conclude that during the 
#' repair period the model did suffer a statistically significant
#' reduction in occupancy rate,with a small probability, 0.05, of
#'  a Type I error.  


#e). Estimate motel_pct -comp_pct ~ gamma_1 +gamma_2*Repair +e
# construct 95% interval estimate of siggma_2

fit3 <- lm(motel_pct - comp_pct ~repair, data = motel)
summary(fit3)

confint(fit3, level = 0.95)

#' We estimate with 95% confidence, that during the repair period 
#' the average difference between the motel’s occupancy rate and 
#' the competitor’s occupancy rate fell by between 22.37% and
#'  5.87%.

#f). Test H0: sigma_2 =0 vs H1: Sigma_2 <0, alpha=0.01

summary(glht(fit3,linfct = c("repair =0")))

summary(glht(fit3,linfct = c("repair =0")))$test$tstat < qt(0.01, df=length(motel$motel_rate)-2)
#or look at the p-value 
#conclusion: reject H0

#' We reject the null at 1% sig. level. Hence, we conclude
#'  that there is a statistically 
#' significant inverse relationship between the average 
#' difference between
#' the motel’s occupancy rate & the competitor’s occupancy
#'  rate and the repairs. 


