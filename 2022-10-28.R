library(data4led)
bulb <- led_bulb(1,seed=123)
t <- bulb$hours
y <- bulb$percent_intensity

plot(t,y, pch = 16)



a1 <- sum(y*t-100*t)/sum(t)

f1 <- function(x, a1){100+a1*x}
x <- seq(0,5000)
plot(x,f1(x,a1), type="l")

plot(t,y, pch = 16)
lines(x,f1(x,a1), type="l")




c.11 <- sum(t^2)
c.12 <- sum(t^3)
b.1 <- sum((y-100)*t)
c.21 <- sum(t^3)
c.22 <- sum(t^4)
b.2 <- sum((y-100)*t^2)
  
a2 <- (c.11*b.2 - c.21*b.1)/(c.11*c.22 - c.21*c.12)
a1 <- (b.1-c.12*a2)/c.11

a2 
a1 

f2 <- function(x, a1,a2){100+a1*x+a2*x^2}
x <- seq(0,5000)
plot(t,y, pch = 16)
lines(x,f2(x,a1,a2), type="l")

