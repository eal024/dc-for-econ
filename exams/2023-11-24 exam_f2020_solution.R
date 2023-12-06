
UU <- function(x,y) {
 return(x^(1-gamma)/(1-gamma)+psi*y^(1-theta)/(1-theta))
}

gamma <- 0.5
theta <- 0.3
psi <- 2
UU(2,2)


px <- 1
py <- 2
R <- 10
xs <- seq(0.01,5,length.out = 100)
plot(xs,U(xs,(R-px*xs)/py ),type='l')

UU(1,(R-px*1)/py )

Ux <- function(x) {
 return(
 -UU(x,(R-px*x)/py)
 )
}

R <- 10
demand <- optim(1,Ux)

demand$par

## Engel cuve
Rs <- seq(.2,15,length.out = 100)
xs <- NULL
for (R in Rs) {
 opt <- optim(0.1,Ux)
 xs <- c(xs,opt$par)
}
plot(Rs,xs,type='l')

R <- 10
pxs <- seq(0.2,4,length.out = 50)
xs <- NULL
for (px in pxs) {
 opt <- optim(0.1,Ux)
 xs <- c(xs,opt$par)
}



plot(pxs,xs,type='l')

R <- 10
pxs <- seq(0.2,4,length.out = 50)
xs <- NULL
for (px in pxs) {
 opt <- optim(0.1,Ux)
 xs <- c(xs,opt$par)
}
plot(pxs,xs,type='l')
