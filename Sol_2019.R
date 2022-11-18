

rm(list = ls())
library(mosaic)
load(url("http://ansatte.uit.no/oystein.myrland/data/asparagus.RData"))
names(asparagus)

# price - the relative price (price = 100*P/PM) per bunch of asparagus, where PM
         # was $2.782.
# green - the number of inches of green color on the 
          #asparagus(in hundredths of inches)
# nostalks - the number of stalks of asparagus per bunch 
# disperse - the variation in size per bunch (0 is uniform or all equal size)
            # measured in percent.
head(asparagus)

m1 <- lm(price ~ green + nostalks + disperse, data = asparagus)
summary(m1)

# Interprate the slope of green, when the change in green is 2 inches. 
coef(m1)[2]*2*100
round(coef(m1)[2]*2*100,2)

# nostalks, when the number of stalks per bunch decrease by 5.  
round(coef(m1)[3]*-5,2)

# disperse- the variation in size per bunch (0 is uniform, all equal )
# disperse increase with 10 percent 
round(coef(m1)[4]*10,2)

####################################
# Asparagus model 2
#################################
#' calculate a 95% CI for a 5 unit increase in nostalks
confint(m1,level = 0.95)[3,]*5
round(confint(m1,level = 0.95)[3,]*5,2)

#################################
# Asparagus model 3
#############################
#' Do an F-test comparing your parameter estimates to Waugh's
#' Joint F test
library(car)
linearHypothesis(m1,c("green=0.13826", 
                      "nostalks=-1.53394", 
                      "disperse=-0.27553"), 
                       level=0.95)

#Critical F-value 
qf(0.95, df1=3,df2 = 196)

##############################
# Asparagus model 4
###########################
f=makeFun(m1)

f(green = mean(asparagus$green),
  nostalks = mean(asparagus$nostalks),
  disperse = 0)

f(green = mean(asparagus$green),
  nostalks = mean(asparagus$nostalks),
  disperse = 50)

f(green = mean(asparagus$green),
  nostalks = mean(asparagus$nostalks),
  disperse = 0)- 
f(green = mean(asparagus$green),
  nostalks = mean(asparagus$nostalks),
  disperse = 50)

#f(green=mean(asparagus$green),nostalks=mean(asparagus$nostalks),disperse=0)-
#f(green=mean(asparagus$green),nostalks=mean(asparagus$nostalks),disperse=50)


###############################
# Asparagus model 5
###############################
deltaMethod(m1, "50*b4", parameterNames = paste("b",1:4,sep = ""))
round(deltaMethod(m1, "50*b4", parameterNames = paste("b",1:4,sep = "")),2)



###############
# Asparagus model 6
##############
deltaMethod(m1, "50*b4", parameterNames = paste("b",1:4,sep = ""),level =0.99)

round(deltaMethod(m1, "50*b4", parameterNames = paste("b",1:4,sep = ""),level =0.99),2)


##########################
# Asparagus model 7
################################
f(green = 550,
  nostalks = 20,
  disperse = 14,
  interval = "prediction",level = 0.95)

round(f(green = 550,
        nostalks = 20,
        disperse = 14,
        interval = "prediction",level = 0.95),2)


#####################
# Asparagus model 8
#########################
#' Elasticity, With respect green 
my <- f(green = mean(asparagus$green),
       nostalks = mean(asparagus$nostalks),
       disperse = mean(asparagus$disperse))
       
 
# A 10% increase 
10*coef(m1)[2]*mean(asparagus$green)/my
round(10*coef(m1)[2]*mean(asparagus$green)/my,2)

#' With respect to nostalks, a 10% increase 
10*coef(m1)[3]*mean(asparagus$nostalks)/my
round(10*coef(m1)[3]*mean(asparagus$nostalks)/my,2)


# with respect to disperse
round(10*coef(m1)[4],2)


###################
# Asparagus model 9
###################
library(car)
mgr = mean(~green, data=asparagus)
mp = mean(~price, data=asparagus)

deltaMethod(m1, "10*b2*mgr/mp",parameterNames = paste("b",1:4,sep = ""))
#round 
round(deltaMethod(m1, "10*b2*mgr/mp",parameterNames = paste("b",1:4,sep = "")),2)


############################
# Asparagus model 10
###########################
#' What is the effect in DOLLARS of an increase in nonstalks of
#' 10 stalks per bunch
summary(m1)
coef(m1)[3]*10*2.782/100
# rounding 
round(coef(m1)[3]*(10/100)*2.782,2)

deltaMethod(m1, "b3*(10/100)*2.782", parameterNames = paste("b",1:4,sep = ""))



#########################################
# Electricity model 
###########################################
rm(list = ls())
library(mosaic)
load(url("http://ansatte.uit.no/oystein.myrland/data/electricity.RData"))

head(electricity)

dim(electricity)

names(electricity)

#OLS model 
m2 <- lm(log(costs/pf) ~log(kwh) + log(pl/pf) + log(pk/pf), data = electricity)
summary(m2)

# R^2 value 
summary(m2)$r.squared
#or 
library(broom)
glance(m2)

stat <- glance(m2)
stat$r.squared

###########################
# Electricity model 2
############################
# Anova test for joint significance 
stat$statistic

# Critical f
qf(0.95, df1 =3, df2=141)
#----------------------------------
electricity<- mutate(electricity, 
                     lcosts_pf = log(costs/pf),
                     lkwh = log(kwh),
                     lpl_pf = log(pl/pf),
                     lpk_pf = log(pk/pf))
m3 <- lm(lcosts_pf ~lkwh + lpl_pf +lpk_pf, data = electricity)
summary(m3)
summary(m2)

library(car)
linearHypothesis(m3, c("lkwh=0", "lpl_pf=0", "lpk_pf=0"), level=0.95)
#-------------------------------


##########################
# Electricity model 3
#########################
# a1
deltaMethod(m2, "b3/b2", parameterNames = paste("b",1:4,sep = ""))

# a2
deltaMethod(m2, "b4/b2", parameterNames = paste("b",1:4,sep = ""))

# a3
deltaMethod(m2, "(1-b3-b4)/b2", parameterNames = paste("b",1:4,sep = ""))


###################
#Electricity model 4
######################
# r , r= a1 +a2 +a3
deltaMethod(m2, "b3/b2 +b4/b2 +(1-b3-b4)/b2", parameterNames = paste("b",1:4,sep = ""))
#or 
deltaMethod(m2, "(b3 +b4 +(1-b3-b4))/b2", parameterNames = paste("b",1:4,sep = ""))


# returns to scale r-1
deltaMethod(m2, "(b3 +b4 +(1-b3-b4))/b2-1", parameterNames = paste("b",1:4,sep = ""))

###########################
# Electricity model 5
############################

#RESET test 
# Indicates something wrong with the fn form 
library(lmtest)
resettest(m2,power = 2,type = "regressor")
#or
resettest(m3,power = 2,type = "regressor")
?resettest
