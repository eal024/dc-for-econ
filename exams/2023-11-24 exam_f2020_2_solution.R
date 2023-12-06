
# 

U <- function(x,y, g = 0.5, tau = 0.3, w = 2){
    x^(1-g)/(1-g) + w*y^(1-tau)/(1-tau)     
 }

# Execute the Utility function
U(x = 2, y = 2)

# b) 
# R = p1x + p2y
# y = (R-p1x)*(1/p2)
U_as_f_x <- function(x, p1 =1, p2 = 2, R = 10){
    U(x, y = ((R-p1*x)/py ))
}

U_as_f_x <- function(x, p1 =1, p2 = 2, R){
    U(x, y = ((R-p1*x)/py ))
}


U_as_f_x(x = 1, R = 10)

R = 10
U_as_f_x(x = 2)

U_as_f_x(x = 4)

interv <- seq( from = 0.1, to = 5, length.out = 20 )

df <- tibble( x = interv, u = map_dbl( interv, \(x) U_as_f_x(x = x, R= 10)) )

ggplot( df, aes(x = x, y = u ) ) +
    geom_line()

vUx <- function(x, p1 = 1, p2 = 2) {
 return(
 -U(x, (R-p1*x)/p2 )
 )
}

R = 10
demand <- optim(1,vUx)$par

# Engels
Rs <- seq(.2,15,length.out = 100)
xs <- NULL
for (R in Rs) {
    opt <- optim(0.1,vUx)
    xs  <- c(xs,opt$par)
    }

# Optimum X of different values of R
plot(Rs,xs,type='l')

# Optimum X based on diff values p1
R <- 10
vUx <- function(x, p1 = px, p2 = 2) {
 return(
 -U(x, (R-p1*x)/p2 )
 )
}

pxs <- seq(0.2,4,length.out = 50)
xs <- NULL
for (px in pxs) {
 opt <- optim(0.1,vUx)
 xs <- c(xs,opt$par)
}

plot(pxs,xs,type='l')




