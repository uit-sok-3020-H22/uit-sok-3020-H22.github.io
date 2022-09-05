
# P.8


rm(list = ls())

library(mosaic)

# a
revenue <- function(advert) {
  return(100 + 20*advert)
}

# or using the "makeFun()" function from the mosaic package 

revenue <- makeFun(100 + 20*advert ~ advert)

plotFun(revenue(advert)~advert, xlim=c(0,5), ylim=c(0,250))

#or
curve(revenue(x), 0, 5, xlim = c(0,5), ylim = c(0,250))

# b)
#We know that  var(revenue|advert)=900, hence, sd=

sqrt(900)

#when advert=2, revenue is
revenue(2)

# Prob(revenue > 110 | advert=2) =? 
xpnorm(110, mean=revenue(2), sd=sqrt(900))

#or using base R
1-pnorm(110, mean=revenue(2), sd=sqrt(900))


# c) 
#the Prob(revenue > 110 | advert=3)=?
xpnorm(110, mean=revenue(3), sd=sqrt(900))

# or 
1-pnorm(110, mean=revenue(3), sd=sqrt(900))

# d)
# 2.5 & 97.5 percentiles when advert=2
xqnorm(c(0.025, 0.975), mean=revenue(2), sd=sqrt(900))


# e)

#level of advert=? required to ensure revenue > 110 is 0.95 crude search, 
#what value of advert gives a prob of 0.95, 
#solution is advert between 2.9 and 3

1-pnorm(110, mean=revenue(3), sd=sqrt(900))

1-pnorm(110, mean=revenue(2.9), sd=sqrt(900))

#value of revenue
qnorm(0.95, mean=110, sd=sqrt(900))


#search for advert value
suppressPackageStartupMessages(library(pracma))

# we solve for advert when: 100+20*advert=159.3456 find the value, using bisect() answer is in $root
bisect(function(x) 100 + 20*x-qnorm(0.95, mean=110, sd=sqrt(900)), 2, 3)

soln_advert <- bisect(function(x) 100 + 20*x-qnorm(0.95, mean=110, sd=sqrt(900)), 2, 3)
soln_advert$root

#insert advert solution into revenue function
revenue(soln_advert$root)

#plot of solution to advert
xpnorm(110, mean=revenue(soln_advert$root), sd=sqrt(900))



## P.12

# a) probability distribution

letter_grade <- c("A","B","C","D","F")

grade <- c(4:0)
prob_grade_wo_F <- c(0.13,0.22,0.35,0.2) #prob of grade without F

#remained prob for F
1-sum(prob_grade_wo_F)

prob_grade <- c(0.13,0.22,0.35,0.2,(1-sum(prob_grade_wo_F))) #prob of grade including F
prob_grade

sum(prob_grade)

cbind(letter_grade,grade,prob_grade)

# b)  Expected value 
mean_grade <- sum(prob_grade*grade)
mean_grade

#Variance 
var_grade <- sum(prob_grade*grade^2)-mean_grade^2
var_grade 

# c) mean and variance for class average

# Remember: Expected value of sample mean is equal to pop mean (expected value), and 
# var(sample mean) = pop var / sample size

# Hence, the expected value will be the same as above, i.e., 2.08

#The variance of class_avr
1/300*var_grade   

# d)
majors <- makeFun(50+10*class_avg ~ class_avg)
majors(mean_grade)

#var(majors)
#var(50 + 10 class_avg) = 10ˆ2 var(class_avg)
10^2*1/300*var_grade


#' A.3


rm(list = ls())
library(mosaic)

inf <- makeFun(-3 + 7*(1/unemp) ~unemp)
inf(1)
inf(10)

# a).
plotFun(inf(unemp) ~ unemp, xlim = c(1,10))

# or 
curve(inf(x),1,10, xlim = c(1,10))
abline(h=0, col= "red")

# b). 

# plot the slope 

D(expression(-3 + 7*(1/unemp)), "unemp")

dinf <- makeFun(-(7 * (1/unemp^2))~unemp)

plotFun(dinf(unemp) ~ unemp, xlim = c(1,10))

#' the impact is greatest as the rate of unemployment approaches
#' zero and it is smallest as unemployment approaches infinity 


# c).

# The marginal effect of the unemployment rate on inflation when UNEMP = 5 % is
dinf(5)



#' A.19

rm(list=ls())


WAGE <- function(x) {-19.68+2.52*16+0.55*x-0.007*x^2}
curve(WAGE(x), 1,80)

D(expression(-19.68+2.52*16+0.55*x-0.007*x^2), "x")

dWAGE <- function(x) 0.55 - 0.007 * (2 * x)

findZeros(dWAGE(x)~x, xlim=c(30,50))

findZeros(dWAGE(x)~x, xlim=c(30,50), npts = 100000)

# calculus
-0.55/(2*-0.007)



#' C.22

rm(list=ls())
library(mosaic)

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/chemical_small.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/chemical_small.rdata"))

# chemical_small.def
# 
# year firm lsales lcapital llabor sk_labor lmaterials foreign export intangibles ownership latitude longitude
# 
# Obs: 1200 firms for the year 2006. Subset of large panel chemical.dat
# 
# year            year 2006
# firm            firm id
# lsales          log of sales
# lcapital        log of capital
# llabor          log of labor
# sk_labor        share of skilled labor
# lmaterials      log of materials
# foreign         foreign owned capital ratio
# export          =1 if firm exports, 0 otherwise
# intangibles     =1 if firm is intangible asset intenst, 0 otherwise
# ownership       =1 if state owned
# latitude        atitude location, in radians
# longitude       longitude location, in radians
# 
# Variable |        Obs        Mean    Std. Dev.       Min        Max
# -------------+---------------------------------------------------------
#   year |      1,200        2006           0       2006       2006
# firm |      1,200       600.5    346.5545          1       1200
# lsales |      1,200    10.39154    1.347168   5.010635    15.6589
# lcapital |      1,200    8.962099    1.725331   4.060443   15.91743
# llabor |      1,200    4.635098    1.193882   1.791759   9.812304
# -------------+---------------------------------------------------------
#   sk_labor |      1,200    .1467024    .1692022          0          1
# lmaterials |      1,200    9.847051    1.441309   3.526361   15.26684
# foreign |      1,200    .0120296     .082234          0          1
# export |      1,200    .1558333    .3628485          0          1
# intangibles |      1,200    .3683333    .4825536          0          1
# -------------+---------------------------------------------------------
#   ownership |      1,200    .1108333    .3140565          0          1
# latitude |      1,200    .6329787    .0791711   .3787365   .8602013
# longitude |      1,200    2.081797    .0643103   1.774491   2.321788
# 
# Notes:
#   
#   Badi H. Baltagi, Peter H. Egger, and Michaela Kesina, "Firm-level
#    Productivity Spillovers in China's Chemical Industry: A Spatial
#    Hausman-Taylor Approach", Journal of Applied Econometrics, Vol. 31,
# No. 1, 2016, pp. 214-248.
# 
# http://qed.econ.queensu.ca/jae/2016-v31.1/baltagi-egger-kesina/
#   
#   The data used in the article are information on Chinese firms in 
# the Chemical industry, retrieved by firm-level surveys.


#' a) Kernel density estimators
chemical_small %>% ggplot(aes(x=lsales)) + geom_histogram()
chemical_small %>% ggplot(aes(x=lsales)) + geom_density(fill="lightblue")

#' Testing normality of the data
require(moments)
#' H0: Normality
jarque.test(chemical_small$lsales)

#' b) sales
chemical_small %>% mutate(sales=exp(lsales)) %>% 
  ggplot(aes(x=sales)) + geom_histogram()

chemical_small %>% mutate(sales=exp(lsales)) %>% 
  ggplot(aes(x=sales)) + geom_density()

#save the data 
chemical_small <- chemical_small %>% mutate(sales=exp(lsales))

jarque.test(chemical_small$sales)

# manual JB test (using the formula on page 836)
sigma <- function(x) {sd(x)*(length(x)-1)/length(x)}
S <- function(x) {(sum((x-mean(x))^3)/length(x))*1/sigma(x)^3}
K <- function(x) {(sum((x-mean(x))^4)/length(x))*1/sigma(x)^4}
JB <- function(x) {length(x)/6*(S(x)^2+(K(x)-3)^2/4)}

JB(chemical_small$lsales) # a
JB(chemical_small$sales) # b

# critical value if alpha = 5% and degree of freedom = 2 (JB has achi-square) distribution with 2 degrees of freedom). 
qchisq(0.95, df=2)

# c) lsales when export =1 if firm exports, 0 otherwise

mean(~lsales, data = filter(chemical_small, export==1))
mean(~lsales, data = filter(chemical_small, export==0))

e1 <- exp(mean(~lsales, data = filter(chemical_small, export==1)))
e0 <- exp(mean(~lsales, data = filter(chemical_small, export==0)))

# percent difference in sales between the two groups
100*(e1-e0)/e0

# alternative way of calculating percent difference in sales b/n the two group 
100*(exp(diffmean(lsales~export, data = chemical_small))-1)

#  still another approach 
fit <- lm(lsales~export, data = chemical_small)
summary(fit)

# percent difference in sales between the two groups, example 7.6
100*(exp(coef(fit)[2])-1)

#' d) Do lsales have the same variance when export =1 or 0?

# In log scale
t.test(lsales~export, var.equal=FALSE, data = chemical_small)

# alternatively 
var.test(lsales~export, data=chemical_small)                  

chemical_small %>% ggplot(aes(x=lsales, fill=as.factor(export))) + geom_density(alpha=0.25)


# In non-log scale 
t.test(exp(lsales)~exp(export), var.equal=FALSE, data = chemical_small)


#--------------------------------------------------------------

# C.23

rm(list=ls())

browseURL("http://www.principlesofeconometrics.com/poe5/data/def/cps5.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/cps5.rdata"))

head(cps5)
str(cps5)

# a.
cps5 %>% filter(female==1) %>% filter(educ==12) %>% summarise(meanwage=mean(wage))
cps5 %>% filter(female==1) %>% filter(educ==16) %>% summarise(meanwage=mean(wage))

mean(~wage|educ, data=filter(cps5, female==1))

cps5 %>% filter(female==1) %>% group_by(educ) %>% summarise(meanwage=mean(wage))

cps5 %>% filter(female==1) %>% group_by(educ) %>% summarise(meanwage=mean(wage)) %>% 
  ggplot(., aes(x=educ, y=meanwage)) + geom_line() + 
  ggtitle("Mean wage of women as a function of years of education")

#' The values suggest that there is a large payoff to the extra 
#' years of education, especially after 10 years of educ. 

# b.
t.test(~wage, data = filter(cps5, female==1 & educ==12), conf.level = 0.95)
ttest <- t.test(~wage, data = filter(cps5, female==1 & educ==12), conf.level = 0.95)
confint(ttest)

#' or directly  
confint(t.test(~wage, data = filter(cps5, female==1 & educ==12)))
confint(t.test(~wage, data = filter(cps5, female==1 & educ==16)))

# c.
mean(~wage|educ, data=filter(cps5, female==0))

# Again, there is large a large payoff to the extra years of education. 

cps5 %>% filter(female==0) %>% group_by(educ) %>% summarise(meanwage=mean(wage)) %>% 
  ggplot(., aes(x=educ, y=meanwage)) + geom_line() + 
  ggtitle("Mean wage of men as a function of years of education")

# Both graphs on the same plot
cps5 %>% group_by(educ,female) %>% summarise(meanwage=mean(wage)) %>% 
  ggplot(., aes(x=educ, y=meanwage, colour = as.factor(female))) + geom_line() +
  ggtitle("Mean wage of men and women as a function of years of education")

# d.
confint(t.test(~wage, data = filter(cps5, female==0 & educ==12)))
confint(t.test(~wage, data = filter(cps5, female==0 & educ==16)))

# e.
# Manually
(mean(~wage, data=filter(cps5, female==1, educ==16))-mean(~wage, data=filter(cps5, female==1, educ==12)))-
  (mean(~wage, data=filter(cps5, female==0, educ==16))-mean(~wage, data=filter(cps5, female==0, educ==12)))


# Using linear model with indicator variables
df <- cps5 %>% filter(educ==12 | educ==16) %>% select(wage,female,educ)

summary(lm(wage~female*as.factor(educ), data=df))

# f.
confint(summary(lm(wage~female*as.factor(educ), data=df)))



