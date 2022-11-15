f0 <- function(x,a=0,b=1){(1/(b-a))+0*x}

x <- seq(-1,2,0.1)
y <- f0(x,-1,2)

plot(x,y,type='l',xlim=c(-2,3),ylim=c(0,0.5))

a <- -1
b <- 2
set.seed(123)
tmp <- runif(25000,a,b)
sum(tmp <= 1.2)/25000
mean(tmp <= 1.2)
x <- length(which(tmp <= 1.2))
p <- x/length(tmp)
p
11/15





draw_rect_approx <- function(f,a,b,num_rectangles, method = "mid"){
  n <- num_rectangles
  dx <- (b-a)/n
  x <- c(a,seq(a,b,dx/100),b,a)
  y <- c(0,f(seq(a,b,dx/100)),0,0)
  par(mar=c(2.5,2.5,0.25,0.25))
  plot(x,y,type = "l")
  
  if(method == "left"){
    xi <- seq(a+0*dx/2,b-dx/2,dx)
    lines(xi,f(xi),type = "h")
    lines(xi,f(xi),type = "s")
    lines(c(xi[n],xi[n]+dx),f(c(xi[n],xi[n])),type = "l")
    lines(c(xi[n],xi[n]+dx),f(c(xi[n],xi[n])),type = "h")
  }
  else if(method == "right"){
    xi <- seq(a+dx,b+dx/2,dx)
    lines(xi-dx,f(xi),type = "h")
    lines(xi-dx,f(xi),type = "s")
    lines(c(xi[n]-dx,xi[n]),f(c(xi[n],xi[n])),type = "l")
    lines(c(xi[n]-dx,xi[n]),f(c(xi[n],xi[n])),type = "h")
  } 
  else{#Use midpoint
    xi <- seq(a+dx/2,b,dx)
    lines(xi-dx/2,f(xi),type = "h")
    lines(xi-dx/2,f(xi),type = "s")
    lines(c(xi[n]-dx/2,xi[n]+dx/2),f(c(xi[n],xi[n])),type = "l")
    lines(c(xi[n]-dx/2,xi[n]+dx/2),f(c(xi[n],xi[n])),type = "h")
  }
}


g <- function(x){4-x^2}
a <- -1
b <- 2
n <- 100
dx <- (b-a)/n
draw_rect_approx(g,a,b,n) 

#Start at half of dx to the right of a, and then step by dx.
xi <- seq(a+dx/2,b,dx)
points(xi,g(xi),pch=16,col=2)
segments(xi,rep(0,length(xi)),xi,g(xi),col=2)

Ai <- g(xi)*dx
sum(Ai)

draw_rect_approx(g,a,b,n) 
draw_rect_approx(g,a,b,n,method='right') 

draw_rect_approx(g,a,b,n,method='left') 
xi <- seq(a,b-dx,dx)
points(xi,g(xi),pch=16,col=2)
segments(xi,rep(0,length(xi)),xi,g(xi),col=2)

Ai <- g(xi)*dx
sum(Ai)

draw_rect_approx(g,a,b,n,method='right') 
xi <- seq(a+dx,b,dx)
points(xi,g(xi),pch=16,col=2)
segments(xi,rep(0,length(xi)),xi,g(xi),col=2)

Ai <- g(xi)*dx
sum(Ai)




g <- function(x){1/2*(x-1)}
a <- 1
b <- 3
n <- 25000000
dx <- (b-a)/n
draw_rect_approx(g,a,b,n) 

#Start at half of dx to the right of a, and then step by dx.
xi <- seq(a+dx/2,b,dx)
points(xi,g(xi),pch=16,col=2)
segments(xi,rep(0,length(xi)),xi,g(xi),col=2)

Ai <- g(xi)*dx
Area <- sum(Ai)
EV <- sum(xi* Ai)
EV


