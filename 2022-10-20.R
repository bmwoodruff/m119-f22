A <- function(a){(300-2*a)*a}
A(0)

a <- seq(0,150,1)
plot(a,A(a),type = "l")

a <- seq(74.9,75.1,.01)
plot(a,A(a),type = "l")

Ab <- function(b){b*(300-b)/2}

b <- seq(0,300,1)
plot(b,Ab(b), type = "l")
