p <- function(x,lambda=2){
  # x must be a whole number
  (lambda^x/factorial(x))*exp(-lambda)
}

p(1)
p(1,1)
p(0,1)

1- (p(1,1)+p(0,1))

sum(p(2:10000,lambda=1))


# The probability of less than 8 Florida tropical storms this year using .
sum(p(0:7,3))

# The probability of less than 8 Florida tropical storms this year using .
sum(p(0:7,6))

# The probability of at least 8 Florida tropical storms this year using .
1-sum(p(0:7,6))
sum(p(8:100,6))


# The probability of more than 12 Florida tropical storms this year using .
1-sum(p(0:12,5))
sum(p(13:100,5))



#Class code from wiki.
#Define the Poisson distribution with a default value for lambda
p <- function(x,lambda=2){
  # x must be a whole number
  (lambda^x/factorial(x))*exp(-lambda)
}

#The probability of 4 Florida tropical storms this year (lambda = 2 is assumed)
p(4) 

#The probability of 4 Florida tropical storms this year (lambda = 2 is assigned - should match above. )
p(4,2) 

#The probability of 5 Florida tropical storms this year (lambda = 2 is assumed)
p(5)

#The probability of 2 Florida tropical storms this year (lambda = 2 is assumed)
p(2)

#The probability of $x$ Florida tropical storms this year (lambda = 2 is assumed) for each x from 0 to 10
p(0:10)

#The probability of 4 Florida tropical storms this year, using lambda = 5. 
p(4,5) 

#The probability of $x$ Florida tropical storms this year, using lambda = 5, for each x from 0 to 10
p(0:10,5)

#The probability of $x$ Florida tropical storms this year, using lambda = 1, for each x from 0 to 10
p(0:10,1)



# Compute the probability of 7 Florida tropical storms in a year assuming labmda = 8.
# Compute the probability of 7 Florida tropical storms in a year assuming lambda = 5.
# If we actually observed of 7 Florida tropical storms in a year, would you be more likely to say that lambda = 8  or lambda = 4? Why?
# Compute the probability of 7 Florida tropical storms in a year assuming lambda = 6.3 . (Wait, can we use decimals for ?)
# Compute the probability of 7 Florida tropical storms in a year using various values for lambda . Then as a group decide what value you think is the best value to assume for lambda  if we actually did see 7 tropical storms in a year.
# Construct a plot that has  on the horizontal() axis, and on the vertical() axis we place the probability of 7 Florida tropical storms in a year assuming that value for . Describe the shape of this plot, and how can we use it to find the "best value" for .
# How does your choice of "best"  change if we know there will be 4 tropical storms?
#   How your choice for  change if we know there will be 5 tropical storms?

p(7,8)
p(7,5)
p(7,7)
p(7,10)
lambda <- seq(1,10,0.01)

plot(lambda,p(7,lambda), type = "l")




p.3v1 <- function(x,lambda=2){
  # each element of x must be a whole number
  prod((lambda^x/factorial(x))*exp(-lambda))
}

p.3v2 <- function(x1,x2,x3,lambda=2){
  # x1, x2, and x3 must be whole numbers
  (lambda^(x1+x2+x3)/(factorial(x1)*factorial(x2)*factorial(x3)))*exp(-3*lambda)
}



#The probability of 4 Florida tropical storms this year, 4 Florida tropical storms next year, and 8 Florida tropical storms the year after.
p.3v1(c(4,4,8))
#The same probability as above, using the other version
p.3v2(4,4,8)
#The same probabilty as above by just multiplying probabilities of independent events together. 
p(4)*p(4)*p(8)

#The probability of 2 Florida tropical storms this year, 5 Florida tropical storms next year, and 3 Florida tropical storms the year after.
p.3v1(c(2,5,3))


p.3v1(c(4,4,8), 1)
p.3v1(c(4,4,8), 7)

lambda <- seq(1,10,0.01)
plot(lambda,p.3v2(4,4,8,lambda) )

lambda <- seq(5,6,0.01)
plot(lambda,p.3v2(4,4,8,lambda) )

lambda <- seq(5.3,5.4,0.001)
plot(lambda,p.3v2(4,4,8,lambda) )




#Seems like 5.3 is a good choice for lambda, if we want to make what happened be the most likely thing to happen.

lambda <- seq(5.33333,5.33334,0.000001)
plot(lambda,p.3v2(4,4,8,lambda) )


sum(p(10:100,5.333333))




p.3v2(8,2,8,0)
p.3v2(8,2,8,1)
p.3v2(8,2,8,2)
p.3v2(8,2,8,3)
p.3v2(8,2,8,4)
p.3v2(8,2,8,5)
p.3v2(8,2,8,6)
p.3v2(8,2,8,7)
p.3v2(8,2,8,8)
p.3v2(8,2,8,9)
p.3v2(8,2,8,10)

p.3v2(8,2,8,0:10)


lambda <- seq(0,15,0.01)
p <- p.3v2(8,2,8,lambda)

plot(lambda,p,type='l')





rm(list=ls())
p <- function(x,lambda=1){
  # x must be a whole number
  (lambda^x/factorial(x))*exp(-lambda)
}

f2 <- function(x,lambda=1){
  # x must be positive
  lambda*exp(-lambda*x)
}

f3 <- function(x,mu=0,s=1){
  (1/sqrt(2*pi*s^2))*exp(-(x-mu)^2/(2*s^2))
}



LP <- function(lambda,x){
  # The element of x must be a whole numbers.
  prod((lambda^x/factorial(x))*exp(-lambda))
}

LE <- function(lambda,x){
  # The elements of x must be positive.
  prod(lambda*exp(-lambda*x))
}

# For simplicity assume sigma is 1.
LN <- function(mu,x){
  prod((1/sqrt(2*pi))*exp(-(x-mu)^2/2))
}









p <- function(x,lambda=1){
  # x must be a whole number
  (lambda^x/factorial(x))*exp(-lambda)
}
LP <- function(lambda,x){
  # The element of x must be a whole numbers.
  prod((lambda^x/factorial(x))*exp(-lambda))
}

###Possible Parameter Values for lambda###
p1 <- seq(5.8,6.0,0.001)
###Data###
# Florida Hurricane Data (2000-2020)
FL <- c(4,4,8,8,6,8,2,8,8,4,8,6,4,3,2,4,6,7,4,7,13)
#Here we calculate the output from the likelihood functions given the observed data.
y.LP <- as.numeric(lapply(p1,FUN=LP,x=FL))
#We have possible parameter values and corresponding likelihood function outputs and we can plot each of the likelihood functions.
par(mar=c(2.5,2.5,3,0.25))
plot(p1,y.LP,type='l',main='Poisson Likelihood')




###Data###
# Florida Hurricane Data (2000-2020)
FL <- c(4,4,8,8,6,8,2,8,7,4,8,6,4,3,3,4,5,7,4,7,13)
# Some Simulated Data (This is data from an Exponential random variable.)
dE <- c(0.45729967, 0.47156107, 1.21461705, 0.20539769, 1.78975399, 0.09095850, 0.64675475, 1.60109333, 1.57752679, 0.01238945)
# Some more Simulated Data (This is data from a Normal random variable.)
dN <- c(-3.77117676, -2.91429587, -2.02774901, -0.23984575, -1.41960740, -3.17490528, -3.21755276, -0.06442566, -1.92134953, -0.93160739)


#Here we calculate the output from the likelihood functions given the observed data.
y.LP <- as.numeric(lapply(p1,FUN=LP,x=FL))
y.LE <- as.numeric(lapply(p1,FUN=LE,x=dE))
y.LN <- as.vector(lapply(p2,FUN=LN,x=dN))

#We have possible parameter values and corresponding likelihood function outputs and we can plot each of the likelihood functions.
par(mar=c(2.5,2.5,3,0.25))
plot(p1,y.LP,type='l',main='Poisson Likelihood')

par(mar=c(2.5,2.5,3,0.25))
plot(p1,y.LE,type='l',main='Exponential Likelihood')

par(mar=c(2.5,2.5,3,0.25))
plot(p2,y.LN,type='l',main='Normal Likelihood')

