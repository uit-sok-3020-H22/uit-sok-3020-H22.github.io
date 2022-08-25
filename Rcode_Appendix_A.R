#' Mathematical tools in R
#' ========================

rm(list=ls())

#' Arithmetic operators
2+1
4-2
2*3
1/5
2^3
8^(1/3)
8^1/3

#' Equality testing "=="
4-2==2
1==.9999999999999

#' Objects (or variables) assignment "<-"
a <- 2
b <- 5
a
b
#' Separate to commands with ;
c <- 3 ; c

a+b
a/b

#' Relational
a>b
a<b

c <- 5
a*c<b*c

#' Remember last update rules old ones
c <- -4
a*c<b*c

#' Number of digits on an object
round(1/3,4) # 4 digits
round(2*4.7536,2) # 2 digits

#' A number raised to the power of zero is equal to one
2^0
223^0

#' Infinity
2/0 # Positive inf
-10/0 # Negative inf

#'  x^Inf when 0 < x < 1 is equal to zero
0.8^Inf

2^Inf

#' Scientific notation
a <- 510000 ; a
b <- 0.00000034 ; b

a*b
a/b

options(scipen=15)  # force R not to use exponential notation 
a/b
b/a
options(scipen=F) # use exponential notation 
b/a

#' Logarithms
ls()    #the ls() function lists all vectors and functions defined in the current working directory of R
rm(list=ls()) # removes objects in your workspace

x <- c() # An empty object
x[1] <- 1          # x(1) is 1 
for (i in 1:7) {  # Loop i from 1 to 7
  x[i] <- 10^(i-1)   # x(i) 
}

options(scipen=999)
x
cbind(x,log(x))

table <- cbind(x,log(x)) ; table
colnames(table)[2] <- "log(x)"
table

log(1000*10000)
log(1000)+log(10000)
log(1000*10000)==log(1000)+log(10000)

a <- log(1000*10000)
a
exp(a)

curve(log(x), from=0, to=100, main="Natural logarithm")
curve(log(x), from=0, to=1000000)

#' Defining a function
f <- function(x) { log(x) }
f
f(100)
log(100)

#' Decimal and percentages
y <- c(3,3.02)
y

diff(y)
diff(y)/y[1]
100*diff(y)/y[1]

lny <- log(y)
100*diff(lny)

y <- seq(1,1.25,0.05)
y
y <- c(1,y[1]+0.01,y[-1])  #combine c()
y
percent <- 100*(y-y[1])/y[1]
logdiff <- 100*(log(y)-log(y[1]))
approx.error <- 100*((percent-logdiff)/logdiff)

table <- cbind(y, percent, logdiff, approx.error)
table
table[-1,]

#' A linear relationship
rm(list=ls())
f <- function(x) { 1+1*x }

#' Intercept
f(0)

par(pty="s") # set the aspect ratio in the plot to be square
curve(f(x), -3,10, main="A linear relationship")
abline(h=0,v=0, col="red")

x1 <- 2
x1;f(x1)
points(x1,f(x1), pch=20, col="blue")

x2 <- 4
x2;f(x2)
points(x2,f(x2), pch=20, col="green")

#' Slope
m <- (f(x2)-f(x1))/(x2-x1)
m

rise <- f(x2)-f(x1) ; rise
run <- x2-x1 ; run
rise/run

#' Slope as the derivative
D(expression(1+1*x), "x")

curve(1*x/f(x),0,10,  main = "Elasticity of y=1+1x")
abline(v=2, col="red")
abline(h=2/f(2), col="blue")

curve(1*x/f(x),0,10,  main = "Elasticity of y=1+1x")
arrows(2,0,2,2/f(2), col="blue")
arrows(2,2/f(2),-0.3,2/f(2), col="green")

#'  Nonlinear relationships
rm(list=ls())

#'  Derivative rule 1
D(expression(c), "x")

#'  Derivative rule 2
D(expression(x^n), "x")

#'  Derivative rule 4
D(expression(c*x^n), "x")

#'  Derivative rule 7
D(expression(exp(x)), "x")
D(expression(exp(a*x+b)), "x")

#'  Derivative rule 8
D(expression(log(x)), "x")
D(expression(log(a*x+b)), "x")

#'  Example A.1
D(expression(4*x+1), "x")

#'  Example A.2
curve(x^2-8*x+16, from=-1, to=10)
D(expression(x^2-8*x+16), "x")

#'  The slope (derivative) of the y=log(x) function
D(expression(log(x)), "x")

#'  The slope dy/dx as a function
d <- function(x) { 1/x }
x <- 1:50
d(x)

plot(x,d(x),type="l")

#' Elasticity = slope * x/y  of the log(x) function
e <- function(x) { (1/x)*(x/log(x)) }
plot(x,e(x),type="l")

e(1)
e(2)

curve((1/x)*(x/log(x)), from=0, to=5)

#'  Example A.2
rm(list=ls())
curve(x^2-8*x+16, from=-1, to=10)
D(expression(x^2-8*x+16), "x")

f <- function(x) { x^2-8*x+16 }
dy <- function(x) { (2*x-8) }
e <- function(x) { (2*x-8)*(x/f(x)) }

x <- seq(0,8,2)
f(x)
dy(x)
e(x)

cbind(x,f(x),dy(x),e(x))
curve(e(x), 0,8)

#'  Partial derivative
D(expression(a*x^2+b*x+c*z+d), "x")

#'  Area under the curve
curve(2*x,0,1)
integrand <- function(x) { 2*x }
integrate(integrand, lower = 0, upper = 1)

integrate(integrand, lower = 0.2, upper = 0.6)
0.6^2-0.2^2

#'  We would like to shade the region represented by
#'  P(0.2 < X < 0.6). The first vertex of our polygon is (0.2,0).
cord.x <- c(0.2)
cord.y <- c(0)
#'  The second vertex is (0.2,f(0.2)), where f(0.2) is the value on the y axis of 2*x evaluated at 0.2. 
cord.x <- c(cord.x,0.2) 
cord.y <- c(cord.y,integrand(0.2))
#'  The third and fourth vertices is (0.6,f(0.6)) and (f(0.6),0).
cord.x <- c(cord.x,0.6,0.6)
cord.y <- c(cord.y,integrand(0.6),0)

curve(2*x,0,1)
polygon(cord.x,cord.y,col='aquamarine3')

num <- integrate(integrand, lower = 0.2, upper = 0.6) ; num
str(num)
text(0.4, 0.4, paste(num$value))



###########################################
#Exercises, page 764 - 767 
#####################################################

rm(list=ls())

#'Load package with dedicated functions.
suppressPackageStartupMessages(library(mosaic))

#' Using `mosaic` functions.  

#' Make function:
f <- makeFun(2+3*x ~ x)
f(0)
f(2)

#' Plot function:
plotFun(f(x) ~ x, xlim=c(0,4))

#' Derivative:
D(expression(2+3*x),"x")

#' Elasticity:
e <- makeFun(3*x/f(x) ~ x)

#' Plot elasticity:
plotFun(e(x) ~ x, xlim=c(0,4), col="brown")
e(1)

#' Using `base R` functions.  

#' Make function:
g <- function(x) {2+3*x}
g(0)
g(2)

#' Plot function:
curve(g(x), 0, 4, col="red")

#' Elasticity:
h <- function(x) {3*x/g(x)}

#' Plot elasticity:
curve(h(x), 0, 4, col="green")
h(1)


#

rm(list=ls())


# A.1

# Q = -3 + 2*P
library(mosaic)

Q1 <- makeFun(-3 + 2*P ~ P)
Q1(0)

# Plot
plotFun(Q1(P)~P, xlim = c(0,12))

# Slope
D(expression(-3 + 2*P),"P")

d1 <- function(P) {2}
d1(2)
d1(10)

# Elasticity, slope*x/y
e1 <- function(P) {2 * P/Q1(P)}
e1(10)
e1(P=10)

plotFun(e1(P) ~ P, xlim = c(2,12))


# 2) Q = 100 - 20*P
Q2 <- makeFun(100 -20*P ~ P)
Q2(0)

# Plot
plotFun(Q2(P)~P, xlim = c(0,12))

# Slope
D(expression(100 -20*P),"P")

d2 <- function(P) {-20}
d2(2)
d2(10)

# Elasticity, slope*x/y
e2 <- function(P) {-20 * P/Q2(P)}
e2(10)
e2(P=10)

plotFun(e2(P) ~ P, xlim = c(0,4))

# 3) Q=50*P^-2
Q3 <- makeFun(50*P^-2 ~ P)
Q3(4)

# Plot
plotFun(Q3(P)~P, xlim = c(0,12))

# Slope
D(expression(50*P^-2),"P")

d3 <- function(P) {-(50*(P^-(2+1)*2))}
d3(2)
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