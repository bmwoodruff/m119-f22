library(data4led)
bulb <- led_bulb(1,seed = 5898)

t <- bulb$hours
y1 <- bulb$percent_intensity

par(mfrow=c(1,1),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y1,xlab="Hour", ylab="Intensity(%) ", pch=16)


f1 <- function(x,a0=0,a1=0){ a0 + a1*x }

x <- seq(-10,80001,2)
yM <- f1(x,a0=101,a1=0.00025)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f1')
lines(x,yM,col=2)
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,yM,col=2)




f4 <- function(t, a0,a1,a2){a0 + a1*t + a2*log(0.005*t+1)}

x <- seq(-10,80001,2)
yM <- f4(x,a0=100,a1=-0.0001,a2=0.9)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f4')
lines(x,yM,col=2)
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,yM,col=2)

