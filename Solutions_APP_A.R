

rm(list = ls())

# How to define a function

#' Using `base R` functions.

f <- function(x) { 1+x }
f(0)

#' Plot 
par(pty="s") # set the aspect ratio in the plot to be square
curve(f(x), -3, 10, col="red", main="A linear relationship ") #not that this does not work if you change x to some other variable 
abline(h=0,v=0, col="blue")


#' Using `mosaic` functions.  

library(mosaic)

# Define a function 
g <- makeFun(1+x ~ x)
g(0)

#' Plot
plotFun(g(x) ~ x, xlim=c(-3,10))


#' Derivative (slope)
D(expression(x^2-8*x+16),"x")

df <- makeFun(2*x-8 ~x) 

# Slope of the curve at x=0
df(0)

#plot the slope 
plotFun(df(x) ~ x, xlim = c(0,3))


# Elasticity 
elas <- makeFun(df(x)*x/(x^2-8*x+16) ~ x)
elas(0)

# plot the elasticity 
plotFun(elas(x) ~ x, xlim = c(0,3))



###########################################
#Exercises, page 764 - 767 
#############################################


rm(list=ls())

# A.1

# Q = -3 + 2*P
library(mosaic)

Q1 <- makeFun(-3 + 2*P ~ P)
Q1(10)
Q1(P=10)

# Plot
plotFun(Q1(P)~P, xlim = c(0,12))

# Slope
D(expression(-3 + 2*P),"P")

d1 <- function(P) {2}
d1(10)

# Elasticity, slope*x/y
e1 <- function(P) {2 * P/Q1(P)}
e1(10)
e1(P=10)

plotFun(e1(P) ~ P, xlim = c(2,12))


# 2) Q = 100 - 20*P

Q2 <- makeFun(100 -20*P ~ P)
Q2(10)

# Plot
plotFun(Q2(P)~P, xlim = c(0,12))

# Slope
D(expression(100 -20*P),"P")

d2 <- function(P) {-20}
d2(10)

# Elasticity, slope*x/y
e2 <- function(P) {-20 * P/Q2(P)}
e2(10)


plotFun(e2(P) ~ P, xlim = c(0,4))

# 3) Q=50*P^-2
Q3 <- makeFun(50*P^-2 ~ P)
Q3(4)

# Plot
plotFun(Q3(P)~P, xlim = c(0,12))

# Slope
D(expression(50*P^-2),"P")

d3 <- function(P) {-(50*(P^-(2+1)*2))}
d3(10)

plotFun(d3(P) ~ P, xlim = c(0,3))

# Elasticity, slope*x/y
e3 <- function(P) {-(50*(P^-(2+1)*2)) * P/Q3(P)}
e3(10)
e3(P=10)

plotFun(e3(P) ~ P)


# A2

# 1) log(MORTALITY) = 7.5 - 0.5* log(INCOME)

log(exp(7.5))

# make function
f1 <- makeFun(exp(7.5)*INCOME^-0.5 ~INCOME)

plotFun(f1(INCOME) ~ INCOME)
plotFun(f1(INCOME) ~ INCOME,xlim = c(0,30))

# slope
D(expression(exp(7.5)*INCOME^-0.5),"INCOME")

# elasticity

e1 <- function(INCOME) {-(exp(7.5) * (INCOME^-(0.5 + 1) * 0.5))*INCOME/f1(INCOME)}

e1(1)
e1(0.5)
e1(3)
e1(25)


# 2) MORTALITY = 1400- 100*INCOME +1.67*INCOME^2

# make function
f2 <- makeFun(1400- 100*INCOME +1.67*INCOME^2 ~INCOME)

plotFun(f2(INCOME) ~ INCOME)
plotFun(f2(INCOME) ~ INCOME,xlim = c(0,30))

# slope
D(expression(1400- 100*INCOME +1.67*INCOME^2),"INCOME")

# elasticity

e2 <- function(INCOME) {(1.67 * (2 * INCOME) - 100)*INCOME/f2(INCOME)}

e2(1)
e2(3)
e2(25)


#'  A.6

rm(list=ls())

#' A.6 a) WHEAT = 0.58 +0.14*log(t)

WHEAT <- makeFun(0.58+0.14*log(t) ~ t)

#' Plot function
plotFun(WHEAT(t) ~ t, xlim=c(1,51))

#' Derivative
D(expression(0.58+0.14*log(t)),"t")

#' Slope
dydt <- makeFun(0.14 * (1/t) ~ t)
dydt(49)

plotFun(dydt(t) ~ t, xlim=c(1,51))

#' Elasticity
e <- makeFun(0.14 * (1/t)*t/WHEAT(t) ~ t)
e(49)

e.simplify <- makeFun(0.14 * 1/WHEAT(t) ~ t)
e.simplify(49)

#' Plot elasticity
plotFun(e(t) ~ t, xlim=c(0,51))


rm(list=ls())

#' A.6 b)  WHEAT = 0.78+0.0003*t^3

WHEAT <- makeFun(0.78+0.0003*t^3 ~ t)

#' Plot function
plotFun(WHEAT(t) ~ t, xlim=c(1,51))

#' Derivative
D(expression(0.78+0.0003*t^3),"t")

#' Slope
dydt <- makeFun(3e-04 * (3 * t^2) ~ t)
dydt(49)

plotFun(dydt(t) ~ t, xlim=c(1,51))

#' Elasticity
e <- makeFun(3e-04 * (3 * t^2)*t/WHEAT(t) ~ t)
e(49)

#' Plot elasticity
plotFun(e(t) ~ t, xlim=c(0,51))

#' ------------------------------------------------


#' A.7

rm(list=ls())

#' WAGE <- 10+200 *AGE -2*AGE^2

WAGE <- makeFun(10+200*AGE-2*AGE^2 ~ AGE)

#' a) Plot function
plotFun(WAGE(AGE) ~ AGE, xlim=c(20,70))

#' Derivative
D(expression(10+200*AGE-2*AGE^2),"AGE")

#' b) Slope
dWAGEdAGE <- makeFun(200 - 2 * (2 * AGE) ~ AGE)

dWAGEdAGE(c(30,50,60))

plotFun(dWAGEdAGE(AGE) ~ AGE, xlim=c(20,70))

#' for AGE=30
WAGE(30) # value of wage (y) when age=30 (x)
dWAGEdAGE(30) # slope at age=30
4210-80*30 # intercept for tangent at age=30

#' Make a plot of slope at AGE=30
curve(10+200*x-2*x^2, 0,70, xlab="AGE", ylab="WAGE(AGE)", lwd=2)
curve(1810+80*x, 0,70, col="red", add=TRUE, lwd=2) # add tangent
segments(30,0,30,4210)

#' c)
findZeros(dWAGEdAGE(AGE)~AGE)
findZeros(dWAGEdAGE(AGE)~AGE, near=40)

#' WAGE is maximized at AGE = 50 . 

#' d)
WAGE(29.99)
WAGE(30.01)

curve(10+200*x-2*x^2, 29.8,30.2, xlab="AGE", ylab="WAGE(AGE)", lwd=2)
points(29.99,WAGE(29.99), col="red", pch=16, cex=1.5)
points(30.01,WAGE(30.01), col="blue", pch=16, cex=1.5)

# e)
(WAGE(30.01)-WAGE(29.99))/(30.01-29.99)
dWAGEdAGE(30)

segments(29.99,WAGE(29.99),30.01,WAGE(29.99))
segments(30.01,WAGE(29.99),30.01,WAGE(30.01))


#' which is identical to the derivative computed in part (b).
#' The values should be close because the slope
#' of the tangent (the derivative) is approximately equal to
#'the “rise” divided by the “run” for a triangle
#' centered at AGE = 30 . 

#' ------------------------------------------------


#' A.8    



#' A.8 a)

rm(list=ls())
suppressPackageStartupMessages(library(mosaic))


q <- makeFun(15-5*p ~ p)
integrate(q, lower = 1, upper = 2)
## 7.5 with absolute error < 8.3e-14

num = integrate(q, lower = 1, upper = 2) ; num
str(num)
## 7.5 with absolute error < 8.3e-14

#plot
curve(q(x),0,4, ylab="q(p)", xlab="p", main=expression(paste("a) P(1 <= p <= 2)")))
abline(h=0)

#create data for the area to shade
cord.x <- c(1,seq(1,2,0.01),2)    # or c(1,1,2,2)
cord.y <- c(0,q(seq(1,2,0.01)),0) # or c(0,q(1),q(2),0)
polygon(cord.x,cord.y,col="skyblue")
text(1.5, 4, paste(round(num$value,3)))

# b) and c) same proc




#' A.10

rm(list=ls())


f <- function(y) 2*exp(-2*y)

curve(f(x),0,4, ylab="f(y)", xlab="y", main=expression(paste("P(1 <= y <= 2)")))

integrate(f, lower = 1, upper = 2)

# Create data for the area to shade
cord.x <- c(1,seq(1,2,0.01),2)
cord.y <- c(0,f(seq(1,2,0.01)),0)

# Add the shaded area.
polygon(cord.x,cord.y,col='skyblue')

num = integrate(f, lower = 1, upper = 2) ; num
str(num)
text(1.2, 0.1, paste(round(num$value,3)))




#' ------------------------------------------------

#' A. 12  

rm(list=ls())

# Some tips here http://www.talkstats.com/threads/in-r-how-to-solve-3-simultaneous-equations.31946/

require(pacman)
require(manipulate)
# Production fn
plotFun(6*L^(1/2)*K^(1/3) ~ L & K)
plotFun(6*L^(1/2)*K^(1/3) ~ L & K, surface=TRUE)

# Profit fn
f <- function(L,K) {4*(6*L^(1/2)*K^(1/3))-12*L-5*K}

plotFun(4*(6*L^(1/2)*K^(1/3))-12*L-5*K ~ L & K)
plotFun(4*(6*L^(1/2)*K^(1/3))-12*L-5*K ~ L & K, surface = TRUE)

D(expression(4*(6*L^(1/2)*K^(1/3))-12*L-5*K), "L")
D(expression(4*(6*L^(1/2)*K^(1/3))-12*L-5*K), "K")

p_load(nleqslv)

fun <- function(x) {
  f <- numeric(length(x))
  
  f[1] <-   4 * (6 * (x[1]^((1/2) - 1) * (1/2)) * x[2]^(1/3)) - 12
  f[2] <-   4 * (6 * x[1]^(1/2) * (x[2]^((1/3) - 1) * (1/3))) - 5
  
  return(f) 
} 

startx <- c(1,1) # start-values

answers <- as.data.frame(nleqslv(startx,fun))  # answers["x"] = x,y are the solutions closest to startx if there are multiple solutions
answers
f(answers$x[1],answers$x[2])

# try a different approach
L <- seq(0, 3, length= 100)
K <- seq(0, 5, length= 100)
Profit <- outer(L, K, f)

p_load(rgl)
persp3d(L, K, Profit, col="gray", alpha=0.5)
points3d(answers$x[1], answers$x[2], f(answers$x[1],answers$x[2]), col="red")

# Optimum conditions, derivative of profit fn
D(expression(4*(6*L^(1/2)*K^(1/3))-12*L-5*K), "L")
D(expression(4*(6*L^(1/2)*K^(1/3))-12*L-5*K), "K")

# second derivative
D(D(expression(4*(6*L^(1/2)*K^(1/3))-12*L-5*K), "L"),"L")
D(D(expression(4*(6*L^(1/2)*K^(1/3))-12*L-5*K), "K"),"K")
# http://www.wolframalpha.com/input/?i=d%5E2%2Fdx+dy+(4*(6*x%5E(1%2F2)*y%5E(1%2F3))-12*x-5*y)


g <- function(L,K) 48*L^(-5/2)*K^16/3
h <- function(L,K) 16/(9*L*K^4)
g(answers$x[1],answers$x[2])>h(answers$x[1],answers$x[2])

# b
D(expression(6*L^(1/2)*K^(1/3)), "L")
dQdL <- function(L,K) {6 * (L^((1/2) - 1) * (1/2)) * K^(1/3)}
D(expression(6*L^(1/2)*K^(1/3)), "K")
dQdK <- function(L,K) {6 * L^(1/2) * (K^((1/3) - 1) * (1/3))}

dQdL(answers$x[1],answers$x[2])
dQdK(answers$x[1],answers$x[2])



#A.20

wage <- function(educ, exper, female) {-23.06+2.85*educ+0.8*exper-0.008*exper^2-9.21*female+0.34*female*educ-0.015*educ*exper}
wage(educ=16, exper=10, female=1)


#predicted wages as a fn of educ
plotFun(wage(educ, exper = 10, female = 1)~educ, xlim = c(0,20))


#slope of educ

D(expression(-23.06+2.85*educ+0.8*exper-0.008*exper^2-9.21*female+0.34*female*educ-0.015*educ*exper), "educ")

## 2.85 + 0.34 * female - 0.015 * exper
slope_educ <- function(female,exper) {2.85 + 0.34 * female - 0.015 * exper}
slope_educ(female = 1, exper = 16)
