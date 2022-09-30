f <- function(t){log(3*t, base=2)-2}
f(2)

f.other <- function(t){
  f(t)-2
}

uniroot(f.other,c(0,10))$root





f <- function(x){3*x-5}
interval <- c(0,10)
interval[1]
interval[2]

x <-seq(interval[1],interval[2],(interval[2]-interval[1])/100)
x
plot(x,f(x), type="l")
abline(h=0, col = "lightgray")

uniroot(f,interval)$root




f <- function(x){3*x-5-7}
interval <- c(0,10)
x <-seq(interval[1],interval[2],(interval[2]-interval[1])/100)
plot(x,f(x), type="l")
abline(h=0, col = "lightgray")

uniroot(f,interval)$root






f <- function(x){3*x-5 - exp(-x)}
interval <- c(0,10)
x <-seq(interval[1],interval[2],(interval[2]-interval[1])/100)
plot(x,f(x), type="l")
abline(h=0, col = "lightgray")

uniroot(f,interval)$root





f <- function(x){3*x-5-log(x)}
interval <- c(1,10)
x <-seq(interval[1],interval[2],(interval[2]-interval[1])/100)
plot(x,f(x), type="l")
abline(h=0, col = "lightgray")

uniroot(f,interval)$root


f <- function(x){x^2+x-6}
interval <- c(0,3)
x <-seq(interval[1],interval[2],(interval[2]-interval[1])/100)
plot(x,f(x), type="l")
abline(h=0, col = "lightgray")

uniroot(f,interval)$root


interval <- c(-4,0)
x <-seq(interval[1],interval[2],(interval[2]-interval[1])/100)
plot(x,f(x), type="l")
abline(h=0, col = "lightgray")

uniroot(f,interval)$root







f <- function(x){3*x-5}
interval <- c(0,10)
x <-seq(interval[1],interval[2],(interval[2]-interval[1])/100)
plot(x,f(x), type="l")
abline(h=0, col = "lightgray")
uniroot(f,interval)$root





f <- function(x){x^2-8*x+12}
x <- seq(0,6,0.1)
plot(x,f(x))

uniroot(f, c(0,3))$root
uniroot(f, c(5,7))$root




f <- function(x){1/x}
x <- seq(-10,10,0.1)
plot(x,f(x))

uniroot(f, c(0,3))$root
uniroot(f, c(5,7))$root
uniroot(f, c(-7,7))$root

