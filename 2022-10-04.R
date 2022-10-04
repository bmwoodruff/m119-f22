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

lambda <- seq(5.2,5.4,0.01)
plot(lambda,p.3v2(4,4,8,lambda) )

#Seems like 5.3 is a good choice for lambda, if we want to make what happened be the most likely thing to happen.