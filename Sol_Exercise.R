
# 

rm(list = ls())
library(mosaic)

#'  A.6

#' A.6 a) WHEAT = 0.58 +0.14*log(t)

WHEAT <- makeFun(0.58+0.14*log(t) ~ t)

WHEAT(1)

#' Plot function
plotFun(WHEAT(t) ~ t, xlim=c(1,51))

#' Derivative
D(expression(0.58+0.14*log(t)),"t")

#' Slope
dydt <- makeFun(0.14 * (1/t) ~ t)
dydt(1)

plotFun(dydt(t) ~ t, xlim=c(1,51))

#' Elasticity
e <- makeFun(0.14 * (1/t)*t/WHEAT(t) ~ t)
e(49)

e.simplify <- makeFun(0.14 * 1/WHEAT(t) ~ t)
e.simplify(49)

#or 
elas <- makeFun(dydt(t)*t/WHEAT(t)~t)
elas(1)

#' Plot elasticity
plotFun(elas(t) ~ t, xlim=c(1,51))


rm(list=ls())
library(mosaic)

#' A.6 b)  WHEAT = 0.78+0.0003*t^2

WHEAT <- makeFun(0.78+0.0003*t^2 ~ t)

#' Plot function
plotFun(WHEAT(t) ~ t, xlim=c(1,51))

#' Derivative
D(expression(0.78+0.0003*t^2),"t")

#' Slope
dydt <- makeFun(3e-04 * (3 * t^2) ~ t)
dydt(49)

plotFun(dydt(t) ~ t, xlim=c(1,51))

#' Elasticity
e <- makeFun(3e-04 * (3 * t^2)*t/WHEAT(t) ~ t)
e(49)

# or 
elas <- makeFun(dydt(t)*t/WHEAT(t)~ t)
elas(49)

#' Plot elasticity
plotFun(e(t) ~ t, xlim=c(0,51))

#' ------------------------------------------------



rm(list=ls())

#' A.20

WAGE <- function(EDUC,EXPER,FEMALES){-23.06+2.85*EDUC+0.80*EXPER-0.008*EXPER^2-
                 9.21*FEMALES + 0.34*(FEMALES*EDUC)-0.015*(EDUC*EXPER)}

WAGE(EDUC =10,EXPER=10,FEMALES = 1)
WAGE(10,10,1)

D(expression(-23.06+2.85*EDUC+0.80*EXPER-0.008*EXPER^2-
               9.21*FEMALES + 0.34*(FEMALES*EDUC)-0.015*(EDUC*EXPER)),"EDUC")

dWdE <- function(EDUC,EXPER,FEMALES){2.85 + 0.34 * FEMALES - 0.015 * EXPER}
dWdE(EDUC=16, EXPER = 10,FEMALES = 1)
dWdE(EDUC=16, EXPER = 10,FEMALES = 0)

#------------------------------------------



# C.18

rm(list=ls())
library(mosaic)

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/star5_small.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/star5_small.rdata"))

str(star5_small)
head(star5_small)

# a)

#  histogram using ggplot
star5_small %>% filter(regular==1) %>% select(mathscore)

df <- star5_small %>% filter(regular ==1) %>% select(mathscore) 
df %>% ggplot(aes(x=mathscore)) + geom_histogram()

# change the width of bins
df %>% ggplot(aes(x=mathscore)) + geom_histogram(binwidth=10)
# add a vertical line at the mean of mathscore 
df %>% ggplot(aes(x=mathscore)) + geom_histogram(binwidth=10) +
  geom_vline(aes(xintercept=mean(mathscore)),
             color="red", linetype="dashed", size=2)
# Add title
df %>% ggplot(aes(x=mathscore)) + geom_histogram(binwidth=10) +
     geom_vline(aes(xintercept=mean(mathscore)),
               color="red", linetype="dashed", size=2)+ labs(title=" mathscore histogram plot")

# some tips for ggplot: http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization


# H0: Normality

?jarque.test

#install.packages("moments")
require(moments)

jarque.test(df$mathscore)

#' The test statistic of the Jarque-Bera (JB) test is always a positive
#' number and if it’s far from zero, it indicates that the sample data
#' do not have a normal distribution.

# b)
mean(df$mathscore)
# alternatively 
mean(~mathscore, data=df)

sd(~mathscore, data=df)
sd(~mathscore, data=df)/sqrt(length(df$mathscore)) # tips: https://www.scribbr.com/statistics/standard-error/

#' The t-statistic in equation (2.16):

#' manual t
(mean(~mathscore, data=df)-490)/(sd(~mathscore, data=df)/sqrt(length(df$mathscore))) 

qt(0.975,df=length(df$mathscore)-1)
qt(0.025,df=length(df$mathscore)-1)


# alternatively,using the built in t-test function 
?t.test
t.test(~mathscore, data = df, mu=490,conf.level=0.95)


# c. ?  (central limit theorem)

# d.
ttest <- t.test(~mathscore, data = df, mu=490,conf.level=0.95)
names(ttest)

ttest$statistic
ttest$conf.int
# the function called confint, extract you the sample mean and CI
confint(t.test(~mathscore, data = df))



# e.

df2 <- star5_small %>% filter(small==1) %>% select(mathscore)

jarque.test(df2$mathscore)

# histogram 
df2 %>% ggplot(aes(x=mathscore)) + geom_histogram(binwidth=10) +
  geom_vline(aes(xintercept=mean(mathscore)),
             color="blue", linetype="dashed", size=2) +labs(title=" mathscore histogram plot")

#-------------------
mean(~mathscore, data=df2)
sd(~mathscore, data=df2)
sd(~mathscore, data=df2)/sqrt(length(df2$mathscore)) 

confint(t.test(~mathscore, data = df2,mu = 490))


# f.
df3 <- star5_small %>% filter(regular==1 | small==1) %>% 
      select(mathscore, regular, small)

head(df3)

mean(~mathscore|regular, data=df3)
mean(~mathscore|small, data=df3)

# Mathscore is highest in small classes
??difmean

diffmean(mathscore~regular, data=df3)
diffmean(mathscore~small, data=df3)

t.test(mathscore~regular, data=df3)
t.test(mathscore~small, data=df3)



# C.19

rm(list=ls())

library(mosaic)

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/cex5_small.def")

load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/cex5_small.rdata"))


str(cex5_small)
head(cex5_small)

#' a)

# HH with advanced degree
df_adv = cex5_small %>% filter(advanced ==1)
#favstats(df_adv$income)
?ggplot
df_adv %>% ggplot(aes(x=income))+geom_histogram(bins = 10) 

# HH with college degree 
df_coll= cex5_small %>% filter(college ==1)
#favstats(df_coll$income)
df_coll %>% ggplot(aes(x=income))+geom_histogram(bins = 10)

#the two histograms on the same page 

#install.packages("gridExtra")               
library("gridExtra")

# The gridExtra package contains the grid.arrange function.
# We can use this function to return our two example plots within the same plot window:
ggp1 <- df_adv %>% ggplot(aes(x=income))+geom_histogram(binwidth = 7)+ ggtitle("Advanced")+
        geom_vline(aes(xintercept=100),color="red", linetype="dashed", size=2)

ggp2 <- df_coll %>% ggplot(aes(x=income))+geom_histogram(binwidth = 7)+ ggtitle("College")+
        geom_vline(aes(xintercept=100),color="red", linetype="dashed", size=2)

grid.arrange(ggp1, ggp2, ncol = 2)          # Apply grid.arrange function

#' The college-degree histogram is more skewed to the right than the 
#' advanced-degree histogram.


# b).

HH_adv_100 <- cex5_small %>% filter(advanced ==1) %>% filter(income > 100) %>% summarise(n())
tot_adv <- cex5_small %>% filter(advanced ==1) %>% select(income) %>% summarise(n())
HH_adv_100/tot_adv

tot_col_100 <- cex5_small %>% filter(college ==1) %>% filter(income > 100) %>% summarise(n())
tot_col <- cex5_small %>% filter(college ==1) %>% select(income) %>% summarise(n())
tot_col_100/tot_col

# c).

?t.test
t.test(~income, mu = 90, data = df_adv, alternative = "greater", conf.level =0.95)
 
 
# d)
 t.test(~income, mu = 90, data = df_coll, alternative = "greater")

 # e).
 
 #confint(t.test(~income, mu = 90, data = df_adv, alternative = "greater"))
 
 #confint( t.test(~income, mu = 90, data = df_coll, alternative = "greater"))
 
 # To get the confidence interval, the one sided test to two sided test 
 confint(t.test(~income, mu = 90, data = df_adv))
 
 confint( t.test(~income, mu = 90, data = df_coll))
 
 
 # f). 
 
 df_adv_coll = cex5_small %>% filter(advanced ==1|college==1)
 head(df_adv_coll)
 
 ?t.test
 
 t.test(income~advanced, data= df_adv_coll, var.equal = TRUE)
 
 t.test(income~advanced, data= df_adv_coll, var.equal = TRUE, alternative = "greater")
 
 
 diffmean(income~advanced, data=df_adv_coll)
 
 
 
