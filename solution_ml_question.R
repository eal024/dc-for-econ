
accidents <- data

# Computing the log likelihood for (alpha,beta)=(1,0.5)
y=accidents$accidents
x=accidents$speed
beta=0.5
alpha=1

# Computing log likelihood for each observation
accidents$Loglik = y-exp(alpha+beta*x)-log(factorial(y))
sum(accidents$Loglik)

# Creating function for computing the likelihood
L<-function(a,b){
  L=sum(y*(a+b*x)-exp(a+b*x)-log(factorial(y)))
  return(L)
}


# we then
L(a=alpha,b=beta)

# b) 

## Numerical approach
# We use f'(x)=lim (h->0) (f(x+h)-f(x))/h

# Setting h to a small number
h=1e-7

# Finding log likelihood at alpha+h and alpha
L.ah = L(1+h,0.5)
L.a  = L(1,0.5)

# Finding the derivative numerically
(L.ah-L.a)/h

# Check values with numDeriv
library(numDeriv)
ll <- function(x) return(L(x[1],x[2]))

grad(ll,c(1,0.5))


# Find the optimum
# Setting initial values
param=c(1,0.5)

# Redefining function for optim-function
ll <- function(x) return(-L(x[1],x[2]))


# Inserting 1 and 0.5 as initial values
param=c(-0.57,0.007)
maxlik=optim(param,ll, method = 'BFGS')
maxlik$par


glm(y~x,family="poisson")
