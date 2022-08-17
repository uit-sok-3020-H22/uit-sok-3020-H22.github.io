#' code 


#' Deletes all objects in memory 
rm(list=ls())

#' comments
2+3

a <- 2
b <- 4

a+b 

#' help
?c

#'or 

#' help("c")

 1, 3, 5

#' combine numbers into a vector
c(1,3,5)

x <- c(1,3,5)
x

1+3+5 #' that is tedious to do, rather use the function "sum"

#' using a math function, sum
sum(x)

?sum

#' select items in a vector
x

x[1]
x[2]

x[1:2]

#' or 
x[c(1,2)]

#' lets sum item 1 and 3 of the vector x
x[1]+x[3]

sum(x[c(1,3)])

sum(x)

length(x)   

#' mean
sum(x)/length(x)

mean(x)

?mean

#' make a function
mymean <- function(i) {    #' using "mean", override the base R function mean, 
  return(sum(i)/length(i)) #' which is not a good idea (use different name)
}

mymean(x)

#' deviations from mean

x-3  #'NO!, do not do this! because we are writing a general code, if we  
     #' change the vector,the answer will be wrong, you have to keep your 
     #' code general

x-mean(x)

mx <- mean(x)

x-mx

#' deviations from mean always sum to zero!
sum(x-mean(x))


#' square the deviations from mean, and sum 
sum((x-mean(x))^2)

#' variance
sum((x-mean(x))^2)/(length(x)-1)

#' variance function
var(x)

#' standard deviation, square root of variance
sqrt(var(x))

#' average dev from mean in the units of x
sd(x)

y <- c(7,3,8)

mean(y)

mymean(y)

sd(y)

#' covariance
cov(x,y)

#' correlation 
cor(x,y)
#' or 
cov(x,y)/(sd(x)*sd(y))

#' plot of data
plot(x,y)
plot(x,y, xlim = c(0,10), ylim = c(0,10))
abline(h=mean(y), v=mean(x), col="red")


###################################################
#' principles of econometrics textbook
###################################################

#' R can also retrieve files over the internet 

#' Copy the following link on a browser 
#' https://www.principlesofeconometrics.com/
#' Clink the link for our textbook: 	Principles of Econometrics, 5th Edition
#' Then you can browse the data definition file and also load the data on R as follows

#'data def files


 browseURL("http://www.principlesofeconometrics.com/poe5/data/def/andy.def")

#'andy.def
#'
#'sales price advert
#'
#'obs:  75   a sample of hamburger franchises in 75 cities from Big Andy's Burger Barn.
#'
#'  sales = S    Monthly sales revenue ($1000s)
#'  price = P    A price index for all products sold in a given month.
#'  advert = A   Expenditure on advertising ($1000s)
#'
#'
#'Data source:  Courtesy of hamburger franchise, called Big Andy's Burger Barn
#'
#'
#'Variable |       Obs        Mean    Std. Dev.       Min        Max
#'-------------+--------------------------------------------------------
#'  sales |        75    77.37467    6.488537       62.4       91.2
#'price |        75      5.6872     .518432       4.83       6.49
#'advert |        75       1.844    .8316769         .5        3.1


#'load "andy" data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/andy.rdata"))

#' viewing data
andy

head(andy)
head(andy,10) #'first 10 rows

tail(andy)

#' variable names 
names(andy)

#' we can also access variables
andy[,"sales"]

#' or 

andy$sales

#' Change one specific variable name
mydata <- andy
names(mydata)

names(mydata)[1] <- "SALES"
names(mydata)

names(mydata) <- c("SALES", "PRICE","ADVERT")
names(mydata)

#' make a scatter plot of sales on the x axis and price on the y axis!

plot(andy$sales,andy$price, pch=19, col="red", xlim=c(60,100), ylim=c(4,8),
     main="Scatterplot of sales and price",
     xlab = "Sales",
     ylab = "Price")

cor(andy$sales,andy$price)

cor.test(andy$sales,andy$price)


#'sales = price x quantity

#'how to find quantity?
#'quantity = sales / price

#' Here we use the "base R" function
andy$quantity <- andy$sales/andy$price  #'creating a new var. "quantity"
head(andy)


#' R ships with some basic functions called base R.
#' However,it extended by packages(read functions) written by collaborators
#' all over the world. Packages in R adds new functionality to R, and you have 
#' to load them ("start them") each time you need them. 

#' install.packages("tidyverse")  #'Note the use of: " " around the package name

#' or 

#' you can also use the "Package toolbar"

library(tidyverse) #'Note that the " " are gone

andy$quantity <- NULL
head(andy)

#' create a new variable with mutate and pipes
andy <- andy %>%
  mutate(quantity=sales/price)

head(andy)

andy %>% ggplot(aes(x=price, y=sales)) + geom_point()

#' Make a scatter plot of quantity on the x axis and price on the y axis! 

andy %>% ggplot(aes(y=price, x=quantity)) + geom_point()


#' Numerical summaries

#' install.packages("mosaic")
library(mosaic)

summary(andy)

mean(~sales, data = andy)
histogram(~sales,data = andy)

#' X ~ N(77,6^2). Find P(80 <=X <=90)
xpnorm( c(80,90),mean = 77, sd = 6 )

#' Regression Model 

m1=lm(sales ~ price, data = andy)

summary(m1)
