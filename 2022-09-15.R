-2,-1,0,1,2,3,4,5

(-2)^2

v <- c(-2,-1,0,1,2,3,4,5)

2*v
10*v
v^2

v * v

?plot
x <- v
y <- v^2
plot(x,y)
?plot
plot(x,y, type = "l")

x <- seq(-5,1,1)
x
2*x
plot(x,2*x, type = "l")
x <- seq(1,5)
x
3+0*x
plot(x,3+0*x, type = "l")


f <- function(x){
  ifelse(x<1, 2*x, 3+0*x )
}

f(-2)
f(0)
f(6)

x <- seq(-5,5,0.001)
f(x)
plot(x,f(x), type = "l")
