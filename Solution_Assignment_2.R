

rm(list = ls())

#' 2.16 

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/capm5.def")

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/capm5.rdata"))
head(capm5)

str(capm5)

# a). 

#' The econometric linear regression model is: rj−rf = αj+βj×(rm−rf )+ej
#' The model is a simple regression model because it can be written as y= β1 +β2 x, where 
#' x = rm−rf , and y = rj−rf

#' b)

#' Using the I() function to create new variables inside the formula.

#' Disney monthly rate of return:
m1 <- lm(dis-riskfree~I(mkt-riskfree), data=capm5)
m1

#' General Electric monthly rate of return:
m2 <- lm(ge-riskfree~I(mkt-riskfree), data=capm5)
summary(m2)

#' Ford monthly rate of return:
m3 <- lm(ford-riskfree~I(mkt-riskfree), data=capm5)
m3

#' IBM monthly rate of return:
m4 <- lm(ibm-riskfree~I(mkt-riskfree), data=capm5)
m4

#' Microsoft monthly rate of return:
m5 <- lm(msft-riskfree~I(mkt-riskfree), data=capm5)
m5

#' Exxon-Mobil monthly rate of return:
m6 <- lm(xom-riskfree~I(mkt-riskfree), data=capm5)
m6
#' Just put all the betas in a vector form to make a comparison easy  
betas=c(coef(m1)[2],coef(m2)[2],coef(m3)[2],coef(m4)[2],coef(m5)[2],coef(m6)[2])
names(betas) = c("Disney","General Electric","Ford","IBM","Microsoft","Exxon-Mobil")
betas
# sort and put according to decreasing value 
sort(betas, decreasing = TRUE)

#' The stocks Ford, GE, and Microsoft are relatively aggressive with Ford being 
#' the most aggressive with a beta value of 1.662. The others are relatively defensive
#' with Exxon-Mobil being the most defensive with a beta value of  0.457.
#

#'  c).

#All estimates of the alpha_j are close to zero and are therefore consistent with finance theory. 

# Plot of the regression fits 
###########################################################################################
# plot of disney  
plot(dis-riskfree~I(mkt-riskfree), col="blue", data=capm5)
title(main=expression(paste("Plot of Disney ", beta, " versus market rate")))
abline(h=0,v=0, lty=2)
abline(m1, col="red")

# plot of general electic 
plot(ge-riskfree~I(mkt-riskfree), col="blue", data=capm5)
title(main=expression(paste("Plot of General Electric ", beta, " versus market rate")))
abline(h=0,v=0, lty=2)
abline(m2, col="red")

#' Plot of Ford 
plot(ford-riskfree~I(mkt-riskfree), col="blue", data=capm5)
title(main=expression(paste("Plot of Ford ", beta, " versus market rate")))
abline(h=0,v=0, lty=2)
abline(m3, col="red")

#' plot of IBM  
plot(ibm-riskfree~I(mkt-riskfree), col="blue", data=capm5)
title(main=expression(paste("Plot of IBM ", beta, " versus market rate")))
abline(h=0,v=0, lty=2)
abline(m4, col="red")

# plot of Microsoft 
plot(msft-riskfree~I(mkt-riskfree), col="blue", data=capm5)
title(main=expression(paste("Plot of Microsoft ", beta, " versus market rate")))
abline(h=0,v=0, lty=2)
abline(m5, col="red")

# plot of Exxon-Mobil
plot(xom-riskfree~I(mkt-riskfree), col="blue", data=capm5)
title(main=expression(paste("Plot of Exxon-Mobil ", beta, " versus market rate")))
abline(h=0,v=0, lty=2)
abline(m6, col="red")
#########################################################################################

#' make a plot of all six securities in one figure 
par(mfrow=c(3,2))
plot(dis-riskfree~I(mkt-riskfree), col="blue", data=capm5, ylim=c(-0.5,1.3))
title(main=expression(paste("Plot of Disney ", beta, " versus market rate")))
abline(h=0,v=0, lty=2)
abline(m1, col="red")
plot(ge-riskfree~I(mkt-riskfree), col="blue", data=capm5, ylim=c(-0.5,1.3))
title(main=expression(paste("Plot of General Electric ", beta, " versus market rate")))
abline(h=0,v=0, lty=2)
abline(m2, col="red")
plot(ford-riskfree~I(mkt-riskfree), col="blue", data=capm5, ylim=c(-0.5,1.3))
title(main=expression(paste("Plot of Ford ", beta, " versus market rate")))
abline(h=0,v=0, lty=2)
abline(m3, col="red")
plot(ibm-riskfree~I(mkt-riskfree), col="blue", data=capm5, ylim=c(-0.5,1.3))
title(main=expression(paste("Plot of IBM ", beta, " versus market rate")))
abline(h=0,v=0, lty=2)
abline(m4, col="red")
plot(msft-riskfree~I(mkt-riskfree), col="blue", data=capm5, ylim=c(-0.5,1.3))
title(main=expression(paste("Plot of Microsoft ", beta, " versus market rate")))
abline(h=0,v=0, lty=2)
abline(m5, col="red")
plot(xom-riskfree~I(mkt-riskfree), col="blue", data=capm5, ylim=c(-0.5,1.3))
title(main=expression(paste("Plot of Exxon-Mobil ", beta, " versus market rate")))
abline(h=0,v=0, lty=2)
abline(m6, col="red")


#' d). 

#' The estimates for beta  given the intercept (alpha) zero are as follows.

#' Estimate the above models again. But now the intercept is removed (just include 0 in the right hand side of each model)
m1t <- lm(dis-riskfree~0+I(mkt-riskfree), data=capm5)
m2t <- lm(ge-riskfree~0+I(mkt-riskfree), data=capm5)
m3t <- lm(ford-riskfree~0+I(mkt-riskfree), data=capm5)
m4t <- lm(ibm-riskfree~0+I(mkt-riskfree), data=capm5)
m5t <- lm(msft-riskfree~0+I(mkt-riskfree), data=capm5)
m6t <- lm(xom-riskfree~0+I(mkt-riskfree), data=capm5)


betas_without_intercept=c(coef(m1t),coef(m2t),coef(m3t),coef(m4t),coef(m5t),coef(m6t))
names(betas_without_intercept) = c("Disney","General Electric","Ford","IBM","Microsoft","Exxon-Mobil")
betas_without_intercept

# sort and put according to decreasing value 
sort(betas_without_intercept, decreasing = TRUE)


#' ###################################################################################



#' 2.27


rm(list=ls())


#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/motel.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/motel.rdata"))

#mPlot(motel)

#' a.
motel <- motel %>% mutate(relprice=100*relprice)

#' motel_pct: percentage motel occupancy
#' relprice: relative price = motel_rate/comp_rate
# ggplot 
motel %>% ggplot(aes(x=relprice, y=motel_pct)) + geom_point()
# alternatively 
gf_point(motel_pct ~ relprice, data = motel) %>%
  gf_lm() %>% gf_labs(title = "", caption = "")

#' b.
fit <- lm(motel_pct ~ relprice, data = motel)
summary(fit)

#' The estimated coefficient on the slope is negative as anticipated.

residuals(fit)
# some new code here, declar the residuals as time series data using the t() function 
ts.residuals = ts(residuals(fit), start=c(2003,3), frequency = 12)
plot(ts.residuals)

#' Save residuals in data.
motel$residuals <- residuals(fit)
#' create date variable
motel$date <- seq(as.Date("2003/3/1"), by = "month", length.out = 25)
str(motel)

#' c.
# http://www.sthda.com/english/articles/32-r-graphics-essentials/128-plot-time-series-data-using-ggplot/
ggplot(data = motel, aes(x = date, y = residuals))+
  geom_line(color = "#00AFBB", size = 2) + 
  geom_vline(xintercept=c(as.numeric(motel$date[17]),as.numeric(motel$date[23])), linetype=2) +
  annotate("text", x = as.Date("2004/10/1"), y = 10, label = "Repair period", size=5)

#' d.
#' The linear regression model with an indicator variable repair 
fit2 <- lm(motel_pct ~ repair, data = motel)
summary(fit2)

#' Outside repair
coef(fit2)[1]
#' During repair
coef(fit2)[1]+coef(fit2)[2]

mean(~motel_pct|repair, data=motel)
diffmean(motel_pct~repair, data=motel)

#'Interprate the results by your self: 
#'During the non-repair period, the average occupancy rate was..... During the repair period,
#' the estimated occupancy rate was...... on average.
#' The beta_2 coefficient measures the difference in average occupancy levels between these two periods.
#' 


###########################################################################################

rm(list=ls())

#' 2.28

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/cps5_small.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/cps5_small.rdata"))



#' a)
favstats(~wage, data = cps5_small)

cps5_small %>% ggplot(aes(x=wage)) + geom_histogram()
cps5_small %>% ggplot(aes(x=wage)) + geom_density(fill="lightblue")

favstats(~educ, data = cps5_small)

cps5_small %>% ggplot(aes(x=educ)) + geom_histogram()
cps5_small %>% ggplot(aes(x=educ)) + geom_density(fill="lightblue")

#' b)
fit <- lm(wage ~ educ, data = cps5_small)
summary(fit)

#' c)
augment(fit) %>% ggplot(aes(x=educ, y=.resid)) + geom_point()

#' d)

# all
cps5_small %>% do(tidy(lm(wage ~ educ, data = .)))
# blacks vs white
cps5_small %>% group_by(black) %>% do(tidy(lm(wage ~ educ, data = .)))
# female vs male
cps5_small %>% group_by(female) %>% do(tidy(lm(wage ~ educ, data = .)))

with(cps5_small, table(black,female))
# all combos
cps5_small %>% group_by(black,female) %>% do(tidy(lm(wage ~ educ, data = .)))

#' e)
cps5_small %>% do(tidy(lm(wage ~ I(educ^2), data = .)))

coef <- cps5_small %>% do(tidy(lm(wage ~ I(educ^2), data = .)))
coef$estimate[2]

slope <- function(educ) {2*coef$estimate[2]*educ}

slope(c(12,16))
coef(fit)[2]

#' f)

f0 <- makeFun(fit)
f1 <- makeFun(lm(wage ~ I(educ^2), data = cps5_small))

#' simple plot
cps5_small %>% ggplot(aes(x=educ, y=wage)) + geom_point() +
  stat_function(fun=f0, col="red") +
  stat_function(fun=f1, col="blue")

#' more advanced plot
cps5_small %>% ggplot(aes(x=educ, y=wage)) + geom_point() +
  stat_function(fun=f0, mapping = aes(color = "f0")) +
  stat_function(fun=f1, mapping = aes(color = "f1")) +
  scale_color_manual(name = "Functions",
                     values = c("blue", "red"), # Color specification
                     labels = c("linear model", "quadratic model")) +
  ggtitle("Wage as a function of education") +
  ylab("earnings per hour ($)") +
  xlab("years of education") + ylim(c(0,100))
