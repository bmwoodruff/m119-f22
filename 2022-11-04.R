D2 <- matrix(c(2,4,4,2), nrow = 2, ncol = 2, byrow = TRUE)
det(D2)
D3 <- matrix(c(2,4,0,4,2,2,0,2,2), nrow = 3, ncol = 3, byrow = TRUE)
det(D3)




rm(list=ls())
data <- read.csv(url("https://byuistats.github.io/M119/data3_ls.csv"))
x <- data$x
y <- data$y

plot(x,y)

f <- function(x,a){a* exp(-x)}
x.values <- seq(-2,4,0.1)
a <- sum(y * exp(-x))/sum(exp(-x)^2)
y.values <- f(x.values,a)
plot(x,y)
lines(x.values,y.values, type = "l")



rm(list=ls())
data <- read.csv(url("https://byuistats.github.io/M119/data5_ls.csv"))
x <- data$x
y <- data$y
plot(x,y)
f <- function(x,a){a*log(x)}
x.values <- seq(0,5,0.01)
a <- sum(y * log(x))/sum(log(x)^2)
y.values <- f(x.values,a)
plot(x,y)
lines(x.values,y.values, type = "l")


rm(list=ls())
data <- read.csv(url("https://byuistats.github.io/M119/data1_ls.csv"))
x <- data$x
y <- data$y
plot(x,y)
f <- function(x,m){m*x}
x.values <- seq(-5,5,0.01)
m <- sum(y * x)/sum(x^2)
y.values <- f(x.values,m)
plot(x,y)
lines(x.values,y.values, type = "l")

rm(list=ls())
data <- read.csv(url("https://byuistats.github.io/M119/data5_ls.csv"))
x <- data$x
y <- data$y
plot(x,y)
f <- function(x,m){m*x}
x.values <- seq(-5,5,0.01)
m <- sum(y * x)/sum(x^2)
y.values <- f(x.values,m)
plot(x,y)
lines(x.values,y.values, type = "l")




rm(list=ls())
data <- read.csv(url("https://byuistats.github.io/M119/data5_ls.csv"))
x <- data$x
y <- data$y

m.best <- (sum(x*y) - length(x)*mean(x)*mean(y))/(sum(x^2) - length(x)*(mean(x))^2)
b.best <- mean(y) - m.best*mean(x)

m.best
b.best

h <- function(x, b=0, m=1){
  b + m*x
}

x.val <- seq(min(x),max(x),(max(x)-min(x))/5) 
y.val <- h(x.val,b=b.best, m=m.best)

par(mfrow=c(1,1),mar=c(2.5,2.5,0.25,0.25))
plot(x,y,type='p',pch=16)
lines(x.val,y.val,col=3)

