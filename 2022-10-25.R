f <- function(x){x*exp(-x)}
Df <- function(x){1*exp(-x) - x*(exp(-x))}
D2f <- function(x){-2*exp(-x) + x*exp(-x)}

x <- seq(0,10, 0.1)
plot(x,f(x),type="l")
uniroot(Df,c(0,10))$root
#Seems like the critical value is 1. 
#Check, do we get zero?
Df(1)
#now compute second derivative
D2f(1)
#It's negative, so concave downwards and we have a local max.





g <- function(x){x*(1-x)}
Dg <- function(x){1-2*x}
D2g <- function(x){0*x-2} #Is the zero important?

cv <- uniroot(Dg,c(0,10))$root
cv
D2g(cv)

x <- seq(0,2, 0.1)
plot(x,g(x),type="l")



my_plot <- function(f,left_bound,right_bound,gap = (right_bound-left_bound)/100, mar = c(2.5,2.5,0.25,0.25), type = "l",... ){
  x <- seq(left_bound,right_bound,gap)
  par(mar=mar)
  plot(x,f(x),type = type,...)
}
my_lines <- function(f,left_bound,right_bound,gap = (right_bound-left_bound)/100, type = "l",... ){
  x <- seq(left_bound,right_bound,gap)
  lines(x,f(x),type = type,...)
}

uniroot(Df,c(-10,10))$root
cv <- uniroot(Df,c(-10,10))$root
Df(cv) #Check we get Df = 0
Df(1) #Uniroot gives approximates, so we may need to round.

par(mfrow=c(2,3))
my_plot(f,-10,10,0.001) #We specify plotting another point every 0.0001. 
my_plot(f,-10,10) #The default in our custom function uses 101 points. 
my_plot(f,-10,10,2) #We specify plotting another point every 2. 
my_plot(f,-1,10)
my_plot(f,-1,10,ylim=c(-3,1))
my_plot(f,0,10,ylim=c(0,0.5))

a<-0
b<-10
par(mfrow=c(1,1))
my_plot(f,a,b,ylim=c(-0.5,0.5))
my_lines(Df,a,b,col = "red")
abline(h=0, lty = 2)
my_lines(D2f,a,b, col = "green")
abline(v=cv,col="blue",lty = 2)
points(cv,f(cv))
points(cv,Df(cv),col="red")
points(cv,D2f(cv),col="green")
legend(6, -.2, legend=c("f", "f\'", "f\'\'"),
       col=c("black","red", "green"), lty=1, cex=0.8)




g <- function(x){x*(1-x)}
Dg <- function(x){1-2*x}
D2g <- function(x){0*x-2} #Is the zero important?


uniroot(Dg,c(-10,10))$root
cv <- uniroot(Dg,c(-10,10))$root
cv

D2g(1/2)
D2g(cv)

my_plot(g,-2,2)
points(cv,g(cv))


a <- -2
b <- 2
my_plot(g,a,b)
my_lines(Dg,a,b,col = "red")
abline(h=0, lty = 2)
my_lines(D2g,a,b, col = "green")
abline(v=cv,col="blue",lty = 2)
points(cv,g(cv))
points(cv,Dg(cv),col="red")
points(cv,D2g(cv),col="green")
legend((a+b)/2, (g(a)+g(b))/2, legend=c("f", "f\'", "f\'\'"),
       col=c("black","red", "green"), lty=1, cex=0.8)





h <- function(x){x^3-x}
Dh <- function(x){3*x^2-1}
D2h <- function(x){6*x}

my_plot(h,-10,10)

x <- seq(-10,10,0.1)
plot(x,h(x),type="l")

my_plot(h,-2,2)


cv <- uniroot(Dh,c(0,10))$root
cv
Dh(cv)
D2h(cv)


cv <- uniroot(Dh,c(-2,0))$root
cv
Dh(cv)
D2h(cv)

my_plot(h, -1,2)
