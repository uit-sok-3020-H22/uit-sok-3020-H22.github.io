#' ## Example P.1
#' 
#' ### Using a cdf

rm(list=ls())

suppressPackageStartupMessages(library(prob))
suppressPackageStartupMessages(library(tidyverse))

#' Would you like to know more on the `prob` package. Run the following code:
#browseURL("https://cran.r-project.org/web/packages/prob/vignettes/prob.pdf")
#or 
#browseURL("https://mran.microsoft.com/snapshot/2020-04-22/web/packages/prob/vignettes/prob.pdf")

#' Generate the population
#' X - value (x=1,2,3,4)
#' Y - color (y=1 (grey) or y=0 (white))

X <- c(1,2,3,4,4,2,3,3,4,4)
Y <- c(rep(1,4), rep(0,6))

pop <- probspace(cbind(X,Y))
pop

#' The parts of the pdf (probability density function), which is the probability of each possible value occurring 
Prob(pop, X==1)
Prob(pop, X==2)
Prob(pop, X==3)
Prob(pop, X==4)

#' The sum of all the probabilities has to be 1
Prob(pop)

pop %>% ggplot(aes(x=X, y=probs)) + geom_bar(stat = "identity") + 
  ggtitle("Figure P.1 Probability density function for X (value on tie)") +
  xlab("X value") + ylab("Probability")

#' F(2) = P(X <= 2)
Prob(pop, X==1) + Prob(pop, X==2)

#' P(X > 2)
1 - (Prob(pop, X==1) + Prob(pop, X==2))

#' This is tedious if we have many values.
#' We find the marginal pdf of X
marginal(pop, vars = "X")

#' Could also use 
pop %>% group_by(X) %>% summarise(probs=sum(probs))
pdfX <- pop %>% group_by(X) %>% summarise(probs=sum(probs))
pdfX

#' F(2) = P(X <= 2)
pdfX %>% filter(X <= 2) %>% summarise(prob=sum(probs))

#' P(X > 2)
pdfX %>% filter(X > 2) %>% summarise(prob=sum(probs))

#' Cumulative probability of X
pdfX %>% mutate(cumprob=cumsum(probs))

#' P(X <= x)
pdfX %>% mutate(cumprob=cumsum(probs)) %>% ggplot(aes(x=X, y=cumprob)) + geom_bar(stat = "identity") + 
  ggtitle("Figure P.1a Cumulative probability density function for X (value on tie)") +
  xlab("X value") + ylab("Probability")



#' ## Example P.2
#' ### Calculating a conditional probability

rm(list=ls())

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(prob))

#' Would you like to know more on the `prob` package. Run the following code:
# browseURL("https://cran.r-project.org/web/packages/prob/vignettes/prob.pdf")

#' Generate the population
#' X - value (x=1,2,3,4)
#' Y - color (y=1 (grey) or y=0 (white))

X <- c(1,2,3,4,4,2,3,3,4,4)
Y <- c(rep(1,4), rep(0,6))

pop <- probspace(cbind(X,Y))
pop

#' Joint and Marginal probabilities, Table P.4
marginal(pop, vars = "X")
marginal(pop, vars = "Y")
marginal(pop, vars = c("X", "Y"))

#' What is the probability of drawing a random slip with the value X=2, given that it is grey, Y=1?
#' P(X=2|Y=1)
A <- subset(pop, X == 2)
B <- subset(pop, Y == 1)

Prob(A)
Prob(B)

#' P(A and B)
intersect(A,B)
Prob(intersect(A,B))

#' P(A|B)=Prob(A and B)/ P(B)
Prob(A, given = B)

#' ... or long way
Prob(intersect(A,B))/Prob(B)



#' ## Example P.3
#' ### Calculating an expected value

rm(list=ls())

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(prob))

#' Would you like to know more on the `prob` package. Run the following code:
# browseURL("https://cran.r-project.org/web/packages/prob/vignettes/prob.pdf")

#' Generate the population
#' X - value (x=1,2,3,4)
#' Y - color (y=1 (grey) or y=0 (white))

X <- c(1,2,3,4,4,2,3,3,4,4)
Y <- c(rep(1,4), rep(0,6))

pop <- probspace(cbind(X,Y))
pop

#' Find the marginal pdf of X
probX <- marginal(pop, vars = "X")
probX

probX %>% ggplot(aes(x=X, y=probs)) + geom_bar(stat = "identity") + 
  ggtitle("Figure P.1 Probability density function for X (value on tie)") +
  xlab("X value") + ylab("Probability")

#' The expected value, or mean of X, E(X) is:
probX %>% mutate(prod=X*probs) %>% summarise(mean=sum(prod))


#' ## Example P.4
#' ### Calculating a Conditional Expectation

rm(list=ls())

suppressPackageStartupMessages(library(prob))
suppressPackageStartupMessages(library(tidyverse))

#' Would you like to know more on the `prob` package. Run the following code:
# browseURL("https://cran.r-project.org/web/packages/prob/vignettes/prob.pdf")

#' Generate the population
#' X - value (x=1,2,3,4)
#' Y - color (y=1 (grey) or y=0 (white))

X <- c(1,2,3,4,4,2,3,3,4,4)
Y <- c(rep(1,4), rep(0,6))

pop <- probspace(cbind(X,Y))
pop

#' Find the conditional pdf of X when Y=1 (grey)
probX <- pop %>% filter(Y==1) %>% probspace()
probX 

#' Find the expected value of X (value), given that Y=1 (grey)?
#' E(X|Y=1)
probX %>% mutate(prod=X*probs) %>% summarise(mean=sum(prod))



#' ## Example P.5
#' ### Calculating a Variance

rm(list=ls())

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(prob))

#' Would you like to know more on the `prob` package. Run the following code:
# browseURL("https://cran.r-project.org/web/packages/prob/vignettes/prob.pdf")

#' Generate the population
#' X - value (x=1,2,3,4)
#' Y - color (y=1 (grey) or y=0 (white))

X <- c(1,2,3,4,4,2,3,3,4,4)
Y <- c(rep(1,4), rep(0,6))

pop <- probspace(cbind(X,Y))
pop

#' Find the pdf of X
probX <- marginal(pop, vars = "X")
probX

#' The expected value, or mean of X, E(X) is:
EX <- probX %>% mutate(prod=X*probs) %>% summarise(mean=sum(prod))
EX

#' E(X^2)
EX2 <- probX %>% mutate(prod2=X^2*probs) %>% summarise(sum=sum(prod2))
EX2

#' The variance of X
EX2$sum-EX$mean^2

#' ... or
probX %>% mutate(prod=X*probs) %>% mutate(mean=sum(prod),
                                          dev2=(X-mean)^2,
                                          var=dev2*probs) %>% summarise(VarX=sum(var))


#' ## Example P.6
#' ### Calculating a Correlation

rm(list=ls())

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(prob))

#' Would you like to know more on the `prob` package. Run the following code:
# browseURL("https://cran.r-project.org/web/packages/prob/vignettes/prob.pdf")

#' Generate the population
#' X - value (x=1,2,3,4)
#' Y - color (y=1 (grey) or y=0 (white))

X <- c(1,2,3,4,4,2,3,3,4,4)
Y <- c(rep(1,4), rep(0,6))

pop <- probspace(cbind(X,Y))
pop

#' Finding E(XY)
pop %>% mutate(prod=X*Y*probs) %>% summarise(sum=sum(prod)) -> EXY
EXY

#' Find the pdf of X
probX <- marginal(pop, vars = "X")
probX

#' The expected value, or mean of X, E(X) is:
probX %>% mutate(prod=X*probs) %>% summarise(mean=sum(prod)) -> EX
EX

#' Find the pdf of Y
probY <- marginal(pop, vars = "Y")
probY

#' The expected value, or mean of Y, E(Y) is:
probY %>% mutate(prod=Y*probs) %>% summarise(mean=sum(prod)) -> EY
EY

#' E(X^2)
probX %>% mutate(prod2=X^2*probs) %>% summarise(sum=sum(prod2)) -> EX2

#' The variance of X
VarX <- EX2$sum-EX$mean^2
VarX

#' E(Y^2)
probY %>% mutate(prod2=Y^2*probs) %>% summarise(sum=sum(prod2)) -> EY2
EY2

#' The variance of Y
VarY <- EY2$sum-EY$mean^2
VarY

#' The correlation
(EXY$sum-EX$mean*EY$mean)/(sqrt(VarX)*sqrt(VarY))

#' ... or
pop %>% mutate(devx=(X-EX$mean)/sqrt(VarX),
               devy=(Y-EY$mean)/sqrt(VarY),
               prod=devx*devy*probs) %>% summarise(corr=sum(prod))



#' ## Example P.7
#' ### Conditional Expectation

rm(list=ls())

suppressPackageStartupMessages(library(prob))
suppressPackageStartupMessages(library(tidyverse))

#' Would you like to know more on the `prob` package. Run the following code:
# browseURL("https://cran.r-project.org/web/packages/prob/vignettes/prob.pdf")

#' Generate the population
#' X - value (x=1,2,3,4)
#' Y - color (y=1 (grey) or y=0 (white))

X <- c(1,2,3,4,4,2,3,3,4,4)
Y <- c(rep(1,4), rep(0,6))

pop <- as_tibble(cbind(X,Y))

#' Find the expected value of X (value), given that Y=1 (grey)?
#' E(X|Y=1)
pop %>% filter(Y==1) %>% probspace() %>% mutate(prod=X*probs) %>% summarise(mean=sum(prod))

#' Find the expected value of X (value), given that Y=0 (white)?
#' E(X|Y=0)
pop %>% filter(Y==0) %>% probspace() %>% mutate(prod=X*probs) %>% summarise(mean=sum(prod))

#' The conditional means of Y given different X's
pop %>% filter(X==1) %>% probspace() %>% mutate(prod=Y*probs) %>% summarise(mean=sum(prod))
pop %>% filter(X==2) %>% probspace() %>% mutate(prod=Y*probs) %>% summarise(mean=sum(prod))
pop %>% filter(X==3) %>% probspace() %>% mutate(prod=Y*probs) %>% summarise(mean=sum(prod))
pop %>% filter(X==4) %>% probspace() %>% mutate(prod=Y*probs) %>% summarise(mean=sum(prod))

#' The unconditional mean of X is
pop %>% probspace() %>%  marginal(vars = "X") %>% mutate(prod=X*probs) %>% summarise(mean=sum(prod))

#' The unconditional mean of Y is
pop %>% probspace() %>%  marginal(vars = "Y") %>% mutate(prod=Y*probs) %>% summarise(mean=sum(prod))



#' ## Example P.8
#' ### Conditional Variance

rm(list=ls())

suppressPackageStartupMessages(library(prob))
suppressPackageStartupMessages(library(tidyverse))

#' Would you like to know more on the `prob` package. Run the following code:
# browseURL("https://cran.r-project.org/web/packages/prob/vignettes/prob.pdf")

#' Generate the population
#' X - value (x=1,2,3,4)
#' Y - color (y=1 (grey) or y=0 (white))

X <- c(1,2,3,4,4,2,3,3,4,4)
Y <- c(rep(1,4), rep(0,6))

pop <- as_tibble(cbind(X,Y))
pop

#' The expected value, or mean of X, when Y=1 (grey), E(X|Y=1) is:
pop %>% filter(Y==1) %>% probspace() %>% mutate(prod=X*probs) %>% summarise(mean=sum(prod)) -> EXy1
EXy1$mean

#' E(X^2)
pop %>% filter(Y==1) %>% probspace() %>% mutate(prod2=X^2*probs) %>% summarise(sum=sum(prod2)) -> EX2y1

#' The variance of X, when Y=1 (grey), Var(X|Y=1) is:
VarXy1 <- EX2y1$sum-EXy1$mean^2
VarXy1

#' The expected value, or mean of X, when Y=0 (white), E(X|Y=0) is:
pop %>% filter(Y==0) %>% probspace() %>% mutate(prod=X*probs) %>% summarise(mean=sum(prod)) -> EXy0
EXy0$mean

#' E(X^2)
pop %>% filter(Y==0) %>% probspace() %>% mutate(prod2=X^2*probs) %>% summarise(sum=sum(prod2)) -> EX2y0

#' The variance of X, when Y=0 (grey), Var(X|Y=0) is:
VarXy0 <- EX2y0$sum-EXy0$mean^2
VarXy0

#' ... or
#' Find the conditional variance of X when Y=1
pop %>% filter(Y==1) %>% probspace() %>% mutate(prod=X*probs,
                                                mean=sum(prod),
                                                dev2=(X-mean)^2,
                                                var=dev2*probs) %>% summarise(VarXy1=sum(var))

pop %>% filter(Y==0) %>% probspace() %>% mutate(prod=X*probs,
                                                mean=sum(prod),
                                                dev2=(X-mean)^2,
                                                var=dev2*probs) %>% summarise(VarXy0=sum(var))

pop %>% filter(X==1) %>% probspace() %>% mutate(prod=Y*probs,
                                                mean=sum(prod),
                                                dev2=(Y-mean)^2,
                                                var=dev2*probs) %>% summarise(VarYx1=sum(var))

pop %>% filter(X==2) %>% probspace() %>% mutate(prod=Y*probs,
                                                mean=sum(prod),
                                                dev2=(Y-mean)^2,
                                                var=dev2*probs) %>% summarise(VarYx2=sum(var))

pop %>% filter(X==3) %>% probspace() %>% mutate(prod=Y*probs,
                                                mean=sum(prod),
                                                dev2=(Y-mean)^2,
                                                var=dev2*probs) %>% summarise(VarYx3=sum(var))

pop %>% filter(X==4) %>% probspace() %>% mutate(prod=Y*probs,
                                                mean=sum(prod),
                                                dev2=(Y-mean)^2,
                                                var=dev2*probs) %>% summarise(VarYx4=sum(var))



#' ## Example P.9
#' ### Iterated Expectation

rm(list=ls())

suppressPackageStartupMessages(library(prob))
suppressPackageStartupMessages(library(tidyverse))

#' Would you like to know more on the `prob` package. Run the following code:
# browseURL("https://cran.r-project.org/web/packages/prob/vignettes/prob.pdf")

#' Generate the population
#' X - value (x=1,2,3,4)
#' Y - color (y=1 (grey) or y=0 (white))

X <- c(1,2,3,4,4,2,3,3,4,4)
Y <- c(rep(1,4), rep(0,6))

pop <- as_tibble(cbind(X,Y))
pop

#' Previously
#' E(X|Y=0)
pop %>% filter(Y==0) %>% probspace() %>% mutate(prod=X*probs) %>% summarise(mean=sum(prod))

#' E(X|Y=1)
pop %>% filter(Y==1) %>% probspace() %>% mutate(prod=X*probs) %>% summarise(mean=sum(prod))

#' Also
pop %>% filter(X==1) %>% probspace() %>% mutate(prod=Y*probs) %>% summarise(mean=sum(prod))
pop %>% filter(X==2) %>% probspace() %>% mutate(prod=Y*probs) %>% summarise(mean=sum(prod))
pop %>% filter(X==3) %>% probspace() %>% mutate(prod=Y*probs) %>% summarise(mean=sum(prod))
pop %>% filter(X==4) %>% probspace() %>% mutate(prod=Y*probs) %>% summarise(mean=sum(prod))

#' Find the marginal pdf of X
probX <- pop %>% probspace() %>% marginal(vars = "X")
probX

#' Iterated Expectation
EYx1 <- pop %>% filter(X==1) %>% probspace() %>% mutate(prod=Y*probs) %>% summarise(mean=sum(prod))
EYx2 <- pop %>% filter(X==2) %>% probspace() %>% mutate(prod=Y*probs) %>% summarise(mean=sum(prod))
EYx3 <- pop %>% filter(X==3) %>% probspace() %>% mutate(prod=Y*probs) %>% summarise(mean=sum(prod))
EYx4 <- pop %>% filter(X==4) %>% probspace() %>% mutate(prod=Y*probs) %>% summarise(mean=sum(prod))

fx1 <- probX$probs[1]
fx2 <- probX$probs[2]
fx3 <- probX$probs[3]
fx4 <- probX$probs[4]

EYx1$mean*fx1+EYx2$mean*fx2+EYx3$mean*fx3+EYx4$mean*fx4


#' ## Example P.10
#' ### Covariance Decomposition

rm(list=ls())

suppressPackageStartupMessages(library(prob))
suppressPackageStartupMessages(library(tidyverse))

#' Would you like to know more on the `prob` package. Run the following code:
# browseURL("https://cran.r-project.org/web/packages/prob/vignettes/prob.pdf")

#' Generate the population
#' X - value (x=1,2,3,4)
#' Y - color (y=1 (grey) or y=0 (white))

X <- c(1,2,3,4,4,2,3,3,4,4)
Y <- c(rep(1,4), rep(0,6))

pop <- as_tibble(cbind(X,Y))
pop

#' The unconditional mean of X is
EX <- pop %>%  probspace() %>% marginal(vars = "X") %>% mutate(prod=X*probs) %>% summarise(mean=sum(prod))

#' Conditional means of (Y|X=x)
EYx1 <- pop %>% filter(X==1) %>% probspace() %>% mutate(prod=Y*probs) %>% summarise(mean=sum(prod))
EYx2 <- pop %>% filter(X==2) %>% probspace() %>% mutate(prod=Y*probs) %>% summarise(mean=sum(prod))
EYx3 <- pop %>% filter(X==3) %>% probspace() %>% mutate(prod=Y*probs) %>% summarise(mean=sum(prod))
EYx4 <- pop %>% filter(X==4) %>% probspace() %>% mutate(prod=Y*probs) %>% summarise(mean=sum(prod))

EX$mean
EYx1$mean
EYx2$mean
EYx3$mean
EYx4$mean

#' Find the marginal pdf of X
probX <- pop %>% probspace() %>% marginal(vars = "X")
probX

fx1 <- probX$probs[1]
fx2 <- probX$probs[2]
fx3 <- probX$probs[3]
fx4 <- probX$probs[4]

#' Covariance(X,Y)
x <- 1:4 
mx <- EX$mean
EYx <- c(EYx1$mean, EYx2$mean, EYx3$mean, EYx4$mean)
fx <- c(fx1,fx2,fx3,fx4)

sum((x-mx)*EYx*fx)

#' ... or
#' The unconditional mean of Y is
EY <- pop %>%  probspace() %>% marginal(vars = "Y") %>% mutate(prod=Y*probs) %>% summarise(mean=sum(prod))

pop %>% probspace() %>% mutate(xdev=X-EX$mean,
                               ydev=Y-EY$mean,
                               prod=xdev*ydev*probs) %>% 
  summarise(covXY=sum(prod))


#' ## Example P.11
#' ### Normal Distribution Probability Calculation

rm(list=ls())

suppressPackageStartupMessages(library(mosaic))

#' Base
curve(dnorm(x), -3,3)

#' Mosaic
plotDist("norm") # mean=0, sd=1
plotDist("norm", mean=2, sd=1)
plotDist("norm", mean=0, sd=4)

#' Use ggplot
ggplot(data = data.frame(x = c(-4, 6)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1), colour = "red") + 
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 2), colour = "blue", lty=4) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 2, sd = 1), colour = "black", lty=2) +
  ylab("f(x)") +
  ggtitle(expression(paste("Figure P.5 Normal probability density functions ", N(mu,sigma^{2})))) +
  geom_text(x = -1.2, y=0.3, label="N(0,1)", color="red") +
  geom_text(x = -3, y = 0.1, label="N(0,4)", color="blue") +
  geom_text(x = 3.2, y = 0.3, label="N(2,1)", color="black") +
  theme_bw()

#' If X~N(3,9), then P(4 <= X <= 6)
xpnorm(c(4,6), mean = 3, sd = sqrt(9))

#' Base
pnorm(6, mean = 3, sd = sqrt(9))  
pnorm(4, mean = 3, sd = sqrt(9))

pnorm(6, mean = 3, sd = sqrt(9)) - pnorm(4, mean = 3, sd = sqrt(9))

#' Standard normal percentiles, Table P.7
qnorm(0.975)
qnorm(0.025)

xpnorm(qnorm(0.975), mean = 0, sd = 1)
xpnorm(qnorm(0.025), mean = 0, sd = 1)

#' ...or
xqnorm(c(0.025, 0.975), mean = 0, sd = 1)

#' The Bivariate Normal Distribution
suppressPackageStartupMessages(library(bivariate))

#' Let; $\mu_x=\mu_y=0,\; \sigma_X=\sigma_y=1,\;\rho=0.7$.
f <- nbvpdf (0, 0, 1, 1, 0.7)
plot(f, main = expression(paste("Figure P.6 The bivariate normal distribution: ", mu[x]," = ",mu[y]," = 0, ",
                                sigma[x]," = ",sigma[y]," = 1, and ",rho," = 0.7.")))
plot(f, TRUE, main = expression(paste("Figure P.6 The bivariate normal distribution: ", mu[x]," = ",mu[y]," = 0, ",
                                      sigma[x]," = ",sigma[y]," = 1, and ",rho," = 0.7.")))

#' Let; $\mu_x=\mu_y=0,\; \sigma_X=\sigma_y=1,\;\rho=0$.
f <- nbvpdf (0, 0, 1, 1, 0)
plot(f, main = expression(paste("Figure P.7 The bivariate normal distribution: ", mu[x]," = ",mu[y]," = 0, ",
                                sigma[x]," = ",sigma[y]," = 1, and ",rho," = 0.")))
plot(f, TRUE, main = expression(paste("Figure P.7 The bivariate normal distribution: ", mu[x]," = ",mu[y]," = 0, ",
                                      sigma[x]," = ",sigma[y]," = 1, and ",rho," = 0.")))

#' The linear regression function.
#' Let; $\mu_x=\mu_y=5,\; \sigma_x=\sigma_y=3,\;\rho=0.7$.
f <- nbvpdf (5, 5, 3, 3, 0.7)
plot(f, main = expression(paste("Figure P.8 The bivariate normal distribution: ", mu[x]," = ",mu[y]," = 5, ",
                                sigma[x]," = ",sigma[y]," = 3, and ",rho," = 0.7.")))
plot(f, TRUE, main = expression(paste("Figure P.8 The bivariate normal distribution: ", mu[x]," = ",mu[y]," = 5, ",
                                      sigma[x]," = ",sigma[y]," = 3, and ",rho," = 0.7.")))

#' The regression function, $E(Y|X)=\alpha+\beta X$
slope <- 0.7*3*3/3^2
intercept <- 5-0.7*5

#' A plot of the bivariate normal distribution with the regression line.
plot(f, main = expression(paste("Figure P.8a Including the regression function: E(Y|X) = ", alpha," + ", beta," X")))
abline(a=intercept, b=slope, col="red", lwd=2)

#' A plot of the conditional distribution of Y when X=10.
x <- seq(0,20, length.out = 100)
y <- dnorm(x, mean = 8.5, sd = sqrt(4.59))
yshift <- y + 10
plot(yshift,x, ylim = c(0,14), xlim=c(8,11), type = "l", ylab = "y", xlab = "x",
     main = "Figure P.8b Conditional distribution of Y given X = 10")
abline(a=intercept, b=slope, col="red", lwd=2)