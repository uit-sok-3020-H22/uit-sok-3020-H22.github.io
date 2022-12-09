



# Solution to fall 2017 exam questions
#(Remember the solutions are also available on canvas file)



rm(list = ls())
library(mosaic)
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/tuna.rdata"))

names(tuna)

#change the price variable, i.e., apr1, from dollars to cents. We converted 
#the price to cents because we are asked to use price in cents, not in dollars. 
tuna1 <- tuna %>% mutate(price=apr1*100)

#the regression model 
m1 <- lm(sal1 ~ price+dispad, data = tuna1)
summary(m1)

#' Interpretation of slope w.r.t. price  

coef(m1)[2]

#' Interpretation of dispad


#' Elasticity 
#'  calculate the own price elasticity for brand #1 at a price of 70 cents (i.e., 
#'# calculate elasticity w.r.t. price )

# First calculate the sale forecast 
f = makeFun(m1)   

#sales when price = 0.7 cents, and with dispad =1 and dispad =0
f(70,1) #with dispad =1
f(70,0)  #with dispad =0

#elasticity with dispad =1
coef(m1)[2]*70/f(70,1)
round(coef(m1)[2]*70/f(70,1),2) 

#elasticity without dispad 
coef(m1)[2]*70/f(70,0)
round(coef(m1)[2]*70/f(70,0),2)




######################
# Siden 5
###########################
# Sale forecast 
f(75, 0) # the baseline sale 

f(75-15, 0)  #sales when price drop by 15 cents, and without dispad  

f(75+5, 1) #sales when price increased by 5 cents, and with dispad 

#' The sale manger assumption is not correct. 

#' Change in sales when dropping price by 15 cents:
f(75-15, 0)-f(75, 0)
round(f(75-15, 0)-f(75, 0),0)

# The change in sales interms of percentage 

# (New -Base)/Base
100*(f(75-15, 0)-f(75, 0))/f(75, 0)

#' Change in sales when increasing the price by 5 cents:
f(75+5, 1)-f(75, 0)
round(f(75+5, 1)-f(75, 0),0)
# In percentage term 
round(100*(f(75+5, 1)-f(75, 0))/f(75, 0),1)


#######################
# Siden 6
#######################
# SE
library(car)
car::deltaMethod(m1, "15*b2+b3", parameterNames = paste("b",1:3, sep = ""),level=0.95)
round(car::deltaMethod(m1, "15*b2+b3", parameterNames = paste("b",1:3, sep = ""),level = 0.95),0)

#90% CI
round(car::deltaMethod(m1, "15*b2+b3", parameterNames = paste("b",1:3, sep = ""), level = 0.9),0)


##-----------------------------------------
# This is not part of the exam 

str(tuna)
names(tuna)
 
# sal1 - number of cans of brand #1 sold during week
# apr1 - average price (in $) per can of brand #1 during week
# apr2 - average price (in $) per can of brand #2 during week
# apr3 -  average price (in $) per can of brand #3 during week
# disp=1 if store display for brand #1 during week but no newspaper ad
# dispad=1 if store display for brand #1 during week and newspaper ad during week


# Include competitors prices in our model,and 
# use prices in dollars, not in cents 

m2 <- lm(sal1 ~ apr1 +apr2 +apr3 +dispad, data = tuna1)
summary(m2)

#Interpret all the slope. 

# Calculate sale forecast with and without dispand.
#Keep all all the prices at their mean 

f = makeFun(m2)

#calculate mean of the prices 
apr1_m =mean(tuna$apr1)
apr2_m =mean(tuna$apr2)
apr3_m =mean(tuna$apr3)

#sale forecast with dispad 
f(apr1 = apr1_m,
  apr2 = apr2_m,  
  apr3 = apr3_m,
  dispad = 1)

#sale forecast without dispad 
f(apr1 = apr1_m,
  apr2 = apr2_m,  
  apr3 = apr3_m,
  dispad = 0)


# Calculate sale forecast with and without dispand.
#Take the price of brand #1 at a price of 90 cents per can, whereas
#brand #2 and #3 sells at a 10 cent discount compared to their mean prices.  
 
# Forecast,when apr1= 0.9, and both competitor drop by 10 cents 
f(apr1 = 0.9,
  apr2 = apr2_m-0.1,   #sale forcast with dispad
  apr3 = apr3_m-0.1,
  dispad = 1)

f(apr1 = 0.9,
  apr2 = apr2_m-0.1,   #sale forcast without dispad
  apr3 = apr3_m-0.1,
  dispad = 0)


#Question to you:

# Calculate the own price elasticity for brand #1 at a price of 90 cents per can, 
#with and without dispad. Keep the price of brand #2 and #3 at their mean levels.

#--------------------------------------------




##########################
# Bluestem model 
##########################

rm(list=ls())
load(url("https://goo.gl/f65Y9a"))
head(bluestem)

model <- lm(Sales ~ Promotion+Wednesday + Thursday + Friday + Saturday + Sunday, data = bluestem)
summary(model)

# R^2 value 
round(summary(model)$r.squared,4)

################
#' side 10
##################
# Hypothesis testing 
library(car)
linearHypothesis(model, c("Promotion=0","Wednesday=0", "Thursday=0", 
                          "Friday=0","Saturday=0","Sunday=0"), level = 0.95)

#critical value 
qf(0.95,df1 = 6, df2 = 242)

#############################
#Side 11
############################
# Anova of the regression model 
anova(model)

library(broom)
glance(model)

restricted.model <- lm(Sales ~1, data = bluestem)
anova(restricted.model)

anova(restricted.model,model)

# What is the Sums of Squares Total
SST = 99782901
sum((bluestem$Sales-mean(bluestem$Sales))^2)

# What is the Sums of Squares Error 
SSE =28510366
#' What is the Sum of Squares Regression
SST-SSE

###############
# Side 12
#################
#The average sales on Friday 
summary(model)
library(mosaic)

f <- makeFun(model)
f(0,0,0,1,0,0)

# alternatively 
coef(model)[1] +coef(model)[5]

# what is the effect of promotion on sales?
coef(model)[2]
f(1,0,0,1,0,0) -f(0,0,0,1,0,0) 

#########################
#Siden 13
#########################
#Test the null hypothesis that the slope coefficient on promotion 
# is equal to zero. Use a 5% level of significance.
glance(model)
summary(model)


#library(dplyr)
tidy(model)
tidy(model) %>% filter(term =="Promotion")

# critical t
round(qt(0.975,242),4)

#######################
#' Siden 14
#########################
# Find a 90% CI of the effect of promotion on sales?
confint(model, level = 0.9)
confint(model, level = 0.9)[2,]
########################
# Siden 15
##########################
# what is the average sales on Saturday with promotion?
summary(model)

# have to include the intercept
coef(model)[1]+coef(model)[2]+coef(model)[6]

f(1,0,0,0,1,0)

############################
#Siden 16
################################
#Find a 95% CI of the average sales on a Friday with promotion 

f(1,0,0,1,0,0, interval = "conf", level =0.95)

# or 
#library(car)
#car::deltaMethod(model, "b1+b2+b5",parameterNames = paste("b",1:7, sep = ""), level = 0.97)

#####################
#' Siden 17
###########################
# A popularity index....
2.87/2.53
coef(model)

coef(model)[6]/coef(model)[5]
#car::deltaMethod(model,"b6/b5", parameterNames = paste("b",1:7, sep = ""))
ttest <-car::deltaMethod(model,"b6/b5-1.134387", parameterNames = paste("b",1:7, sep = ""), level =0.95) # sat/fri
ttest

#car::deltaMethod(model,"b6/b5-2.87/2.53", parameterNames = paste("b",1:7, sep = "")) #sat/fri
round(ttest$Estimate/ttest$SE,3)

# p-value 
glance(model)

2*(1-pt(ttest$Estimate/ttest$SE, glance(model)$df.residual)) 

# alternatively 
2*pt(q=ttest$Estimate/ttest$SE, df=glance(model)$df.residual, lower.tail=FALSE)
# left-tailed test = lower.tail =True, right_tailed =lower.tail =false
# The critical value is 
round(qt(0.995,glance(model)$df.residual),3) 

##########################
#Siden 18
############################
#H0: Homo vs H1:Hetro 
#Do a Goldfeld-Quandt test on the Bluestem model. Use a 5% level of significance .
library(lmtest)
#gqtest(model, point=0.5)
gqtest(model)
