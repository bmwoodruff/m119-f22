rm(list=ls())
data <- read.csv(url("https://byuistats.github.io/M119/data5_ls.csv"))
x <- data$x
y <- data$y

a <- sum(y * log(x))/(sum(log(x)^2))
a
f <- function(x,a){a*log(x)}

x.values <- seq(0,5,0.1)
y.values <- f(x.values,a)

plot(x,y)
lines(x.values,y.values,type = "l")

f(4,a)

#To find where f(x)=5, we could guess (waste of time?)
#or we could use uniroot!!!!!!!
f(2,a)
f(1.5,a)
f(1.6,a)

solve_me <- function(x){f(x,a)-5}
uniroot(solve_me,c(1,2))$root
