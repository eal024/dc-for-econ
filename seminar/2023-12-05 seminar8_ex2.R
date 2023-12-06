

# Speed limits and accidents (postponed exam 2022)

library(tidyverse)


# data
data <- vroom::vroom("data/accidents.csv")

# Accident the depentent variable
obsy <- data$accidents

obsx <- data$speed


L <- function( alfa, beta , y, x ){
    sum( y*(alfa + beta*x) - exp( alfa + beta*x))
}

sum( obsy*(.5 + .5*obsx) - exp( 0.5 + 0.5*obsx) )

ll <- function(ukjent_a ){
    L( alfa = 1, beta = ukjent_a, y = obsy, x = obsx)
}

# b) The numerical derivate of the log likehold

fn_deriv <- function(f,a,  h = 0.00000000001){
    (f(a + h) - f(a))/h
}


fn_deriv(f = ll, a = 10)


