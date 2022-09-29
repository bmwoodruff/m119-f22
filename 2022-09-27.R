f <- function(x){3^x-17}
interval <- c(2,3)
my_root <- uniroot(f, interval)$root

#The code below produces a plot that illustrates what uniroot found. 
x<-seq(interval[1],interval[2],0.1)
plot(x,f(x),type="l")
abline(h=0,col = "gray")
abline(v=my_root,col = "gray")
points(my_root,0, col = "red")

uniroot(f,interval)$root

f <- function(x){log(4*x-2)-5}
interval <- c(1,1000000000)
uniroot(f, interval)

f <- function(x){log(5*x-30,base=10)-1}
interval <- c(6.1,200)
uniroot(f, interval)

f <- function(x){log(x-3,base=3)-16}
interval <- c(100,10000000000)
uniroot(f, interval)

