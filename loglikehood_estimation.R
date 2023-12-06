

# Pakker og bibliotek
library(tidyverse)

# Speedlimits and car accidentts
data <- vroom::vroom("data/accidents.csv")

# Assume number of traffic accidents can be modeled by Poisson distribution

# THe log likehood function: 𝐿(𝛼, 𝛽; 𝑦, 𝑥) = ∑𝑦𝑖(𝛼 + 𝛽𝑥𝑖) − exp(𝛼 + 𝛽𝑥𝑖) − ln (𝑦𝑖!)


# a) Construct a function to compute the log like function, alpha = 0.5 and Beta = 1

ll <- function( alpha, beta, y = data$accidents, x = data$speed ) {
    return( sum( y*(alpha + beta*x) - exp(alpha + beta*x) - log( factorial(y) ) ) )
}

# Test the function.
ll( y = data$accidents, x = data$speed, alpha = .5, beta = 1)


# b) Compute the numerical derivation. log likehood. ragard Alpha, at a = 0.5, b = 1

fn_deriv <- function( f,a,  h){
    (f(a) - f(a+h))/h
}

# adjust the ll-function
f_ll <- function( a){ 
    return( ll(alpha = a, beta = 1, y = data$accidents, x = data$speed) )
    }

# h <- 1e-7
# Calcualte the deriva
fn_deriv(f = f_ll, a = 0.5, h = 10^-7) #(f_ll( a = (0.5+h) ) - f_ll(a = 0.5))/h 

# Simular as with the numDeriv packages
library(numDeriv)

fn_numDeriv_ll <- function(x) return(ll( beta = x[1], alpha = x[2])  ) 

grad(fn_numDeriv_ll, c(1,0.5) )

# c) Find the optimum
# Setting initial values
param <- c(1,0.5)

# Redefine ll

LL <- function(x) { return( -ll( beta = x[1], alpha = x[2])) }

param <- c(0.5, -0.5)

optimum <- optim( param , LL,  method = "BFGS")

# THe optimum value
optimum$par


## Control the result
y <- data$accidents
x <- data$speed
glm(y~x,family="poisson")



