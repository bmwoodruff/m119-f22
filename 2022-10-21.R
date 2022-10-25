g <- function(x){8/x}
x <- seq(-20,20,.001)
plot(x,g(x), xlim=c(-5,5), ylim = c(-5,5))

f <- function(x){sqrt(x^2+(8/x)^2)}
plot(x,f(x), xlim=c(-4,4), ylim = c(0,20))
