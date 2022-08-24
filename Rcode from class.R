
ls()

rm(list = ls())

ls()

#' 1). Simple calculations 

#' a).
34+25

#' b).
54*79

#' c).
698/52

#' d).
4^2

#' e). 
(1/2)^3

#' f). 
9^(1/3)

#' g). 
sqrt(16)

#' h). 
log(3)

?log

#' i). 
exp(1)
log(exp(1))



#' -----------------------------

# 2. 
x <- 24
y <- 30

z <- x+y
z

k <- 2*x-3*y
k

#' 3). working with vectors 
a <- c(12,8,10,15,20)
a

length(a)
min(a)
max(a)

b <- 10:14
b
length(b)

a+b

p <- c(12,8,10,15,20,10:14)

#' or, in simpler way  

p <- c(a,b)
p

#' the function, seq()

?seq

seq(2,100,3)

q <- seq(2,300,3)

length(q)

#' just extracting the values at position 5
q[5]
q[c(5)]

#' Extracting the values at positions 5,10,15, and 20
q[c(5,10,15,20)]

#' Droping some values from the vector 
q[-c(1)]
q[-c(1,2,3,4)]

#' Exract the values at positions 10 to 30  
q[c(10:30)]

x <- q[c(10:30)]
x

sum(x)
length(x)

sum(x)/length(x)
mean(x)
x-mean(x)
sum(x-mean(x))
(x-mean(x))^2
sum((x-mean(x))^2)

sum((x-mean(x))^2)/(length(x)-1)
var(x )
sqrt(var(x))
sd(x)

t <- x+2*x
t

cov(x,t)

#mannually 
sum((x-mean(x))*(t-mean(t)))/(length(x)-1)


# correlation
cor(x,t)
#' calculate cor, Manually
cov(x,t)/(sd(x)*sd(t))

plot(x,t)
plot(x,t, 
     xlim = c(20,100), 
     ylim = c(80,300),
     main="x vs t",
     xlab ="x-axis",
     ylab = "t-axis")

abline(h=mean(t), v=mean(x), col="red")

#-----------------------------


#' 4). Lists and data frames 

c(Alex, Tomas, John, Evan)
c("Alex", "Tomas", "John","James", "Evan")

stname <- c("Alex", "Tomas", "John", "James","Evan")
stname

stname[2]
stname[c(2)]
stname[c(2:4)]

mark <- c(75,90,99,85,100)
mark

#' Apply cbind function (this create a matrix)

?cbind

cbind(stname,mark)

mark.info <- cbind(stname,mark)
class(mark.info)


# Apply data.frame function 
data.frame(stname,mark) 
mark.info <- data.frame(stname,mark)
class(mark.info)

#' Matrix vs data frame in R
#' tips: https://www.geeksforgeeks.org/matrix-vs-dataframe-in-r/ 

names(mark.info)
names(mark.info)[1] <- "student"
names(mark.info)

names(mark.info) <- c("studet","point")
mark.info

#' Display row 3 in the consol 
mark.info[3,]

#' Display just column 1 in the consol 
mark.info[,1]

mark.info$studet

#' Display the item of data in row 4, column 1
mark.info
mark.info[4,1]

#' Replace the item of data in row 4, column 1 to 98
mark.info[4,2] <- 98
mark.info



#' 5. Reading in data from a file 

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


#'load "andy" data
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/andy.rdata"))


#' viewing data
View(andy)

str(andy)

#' Viewing part of a data
andy 
head(andy)
head(andy,10) #'first 10 rows

#' last 10 rows 
tail(andy,10)

#' variable names 
names(andy)

#' we can also access variables
andy[,"sales"]

#' or 

andy$sales

#' rename a specific variable name
names(andy)

mydata <- andy
names(mydata)

names(mydata)[1] <- "SALES"
names(mydata)

#' renaming all the variables 
names(mydata) <- c("SALES", "PRICE","ADVERT")
names(mydata)


#' Now back to our original data frame, andy 

#' make a scatter plot of sales on the x axis and price on the y axis!

plot(andy$sales,andy$price)

plot(andy$sales,andy$price, pch=19, col="red", xlim=c(60,100), ylim=c(4,8),
     main="Scatterplot of sales and price",
     xlab = "Sales",
     ylab = "Price")

#----------------------------------------------------

#' basic statistics 

min(andy$sales)
max(andy$sales)

mean(andy$sales)

#' Using the colMeans() function to calculate the mean of each data set 

colMeans(andy) 

summary(andy)

var(andy$sales)
sd(andy$sales)

cov(andy$sales,andy$price)

cor(andy$sales,andy$price)

cor.test(andy$sales,andy$price)


#########################################################


#' R ships with some basic functions called base R.
#' However,it extended by packages(read functions) written by collaborators
#' all over the world. Packages adds new functionality to R, and you have 
#' to load them ("start them") each time you need them. 

#' install.packages("mosaic") #'Note the use of: " " around the package name

#' or  you can also use the "Package toolbar"

library(mosaic) #'Note that the " " are gone

#' Make a scatter plot of price on the x axis and sales on the y axis!
#' using the function ggplot from the mosaic package 


andy %>% ggplot(aes(x=sales,y=price)) + geom_point()

# Histogram 

# using the base R histogram 
histogram(~sales,data = andy)

# from the mosaic package 
gf_histogram(~sales, data = andy) 

andy %>% ggplot(aes(sales)) +geom_histogram(bins =15, alpha= 0.7)


#' creating a new var. "quantity"

#'sales = price x quantity

#'how to find quantity?
#'quantity = sales / price

#' Here we use the "base R" function
head(andy)

andy$quantity <- andy$sales/andy$price  
head(andy)

#' create a new variable with mutate and pipes  

andy$quantity <- NULL  #drop the variable quantity from the data frame
head(andy)


andy %>% mutate(quantity=sales/price)

#save the data frame as andy 
andy <- andy %>% mutate(quantity=sales/price)

head(andy)


#' Make a scatter plot of quantity on the x axis and price on the y axis! 

andy %>% ggplot(aes(x=quantity,y=price)) + geom_point()

#' Add a smoother to our data 
andy %>% ggplot(aes(x=quantity, y=price)) + geom_point()+geom_smooth(method = lm, se = FALSE)


#--------------------------------------------------------------------------


#' Reading in data from a comma-delimited file

read.csv("http://www.principlesofeconometrics.com/poe5/data/csv/andy.csv")

#' assign a name to the data that you read in using the assignment operator 
andy <- read.csv("http://www.principlesofeconometrics.com/poe5/data/csv/andy.csv")
head(andy)

