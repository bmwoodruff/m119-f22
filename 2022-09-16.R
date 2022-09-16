g <- function(x){x*0+2}
x <- seq(-5,5,1)
y <- g(x)
plot(x,g(x))


f <- function(x){x*3}
x <- seq(-4,7,1)
x <- -4:7
x <- 7:-4
y <- f(x)
plot(x,f(x), type = "l")

g <- function(x){
  ifelse(x<=5 & x>=-5,3*x,NaN)
}
g(4)
g(1:10)
x <- 1:10
plot(x, g(x), type = "l")




install.packages("devtools")
devtools::install_github("byuidatascience/data4led", force = TRUE)



f <- function(x, a = 3, b = 2){a*x+b}
f(2)
f(2,b = 7, a = 1)
f(2,7,1)
a
f


