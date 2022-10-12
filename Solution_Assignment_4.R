

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
#' 
#' 






############################################################################

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


