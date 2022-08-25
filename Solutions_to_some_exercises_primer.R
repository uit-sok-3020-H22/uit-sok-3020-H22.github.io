
# tidy
rm(list = ls())

# P.1
x <- c(17,1,0)
y <- c(5,2,8)

# a
sum(x[1:2])
sum(x)-x[3]

# b
sum(x*y)

# c
sum(x)/3
sum(x)/length(x)
mean(x)

# d
mx <- mean(x)
sum(x-mx)
sum(x-mean(x))

# e
sum((x-mx)^2)
sum((x-mean(x))^2)

# f
sum(x^2)-3*mx^2

# g 
my <- mean(y)
sum((x-mx)*(y-my))



# h
sum(x*y)-3*mx*my

# ---------------------------

# P.5

rm(list = ls())

library(mosaic)



# sales~N(50,000, 6000^2)

# a, Prob(sales > 60,000) =?

?xpnorm

xpnorm(60000, mean = 50000, sd = 6000)

# b, prob(40000 < sales <= 55000)
xpnorm(c(40000,55000), mean = 50000, sd = 6000)

# c, find the 97 percentile
xqnorm(0.97, mean = 50000, sd = 6000)

# d, profits ~ N(3000, 1800^2)

# after simplification  
xpnorm(0, mean = 3000, sd = 1800)

xpnorm(0, mean = 0.3*50000-12000, sd = 0.3*6000)



# ---------------------------------------------------------

# P.6


# a
xpnorm(55, mean = 40, sd = 10)

# b, V=0.8*X-5
xpnorm(55, mean = 0.8*40-5, sd = 0.8*10)




# ------------------------------------------------


# P.7

# a.
sales <- function(price) { 50000-100*price }

sales
# price ~ N(248,10) 
# so, the expected value will be 
sales(248)

plotFun(sales(price) ~ price)

# More bells and whistles
curve(sales(x), 0, 500)
curve(50000-100*x, 0, 500)

abline(h=0, col="grey")
abline(v=0, col="grey")
segments(248,0,248,sales(248))
segments(248,sales(248),0,sales(248))
points(248,sales(248), col="red", pch=19)

# At what price is demand 0?
?findZeros

findZeros(sales(price) ~ price)

# b. Var(50000-100*PRICE)
(-100)^2*10^2

# sd
sqrt((-100)^2*10^2)

# c.
xpnorm(24000, mean=50000-100*248, sd=sqrt((-100)^2*10^2))

# d.
xqnorm(0.95, mean=50000-100*248, sd=sqrt((-100)^2*10^2))

price <- function(sales) { (sales-50000)/-100 }

sales_95 = qnorm(0.95, mean=50000-100*248, sd=sqrt((-100)^2*10^2))

price(sales_95)




