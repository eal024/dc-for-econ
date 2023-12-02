

# Pre
library(tidyverse)

# Market
s <- 3
d <- function(p) 7-p-log(p)

vec <- seq(from = 0.1, to = 5, length.out = 100)
demand <- sapply( vec, function(p) d(p) )

df <- tibble( x = vec, demand = demand)

#
df |>
 ggplot(
    aes( x = x, y = demand) 
    ) + 
    geom_line() +
    geom_hline( yintercept = s) +
    geom_point( data = tibble( x = 2.9, y = 3),
        aes(y = y, x  = x),
        inherit.aes = F,
        color = "red",
        size = 4
    ) 


# 2) Numerical algorithm

market <- function(p, s = 3 ) { 7 - p - log(p) - s }

# descant
fn_descant <- function(x0, x1, f, error = 0.005, max_rep = 15){

    xn_1 <- x0 
    xn   <- x1
    
    i <- 0
    e <- 100


    while( i < max_rep |  e > error )   { 
        # The next guess
        x1 = xn - ((xn-xn_1)/(f(xn)-f(xn_1)))*f(xn) 

        xn_1 <- xn
        xn <- x1

        e <- abs(f(x1)) # Error: should be closs to 0 to stop
        i <- i + 1
    }
    x1
}

fn_descant(x0 = 0.01, x1 = 19, f = market, error = 0.000001, max_rep = 4)

market( p = 2.926271)

# 3) How equilibrium depends on supply S

# price(S): For any level of supply gives the eq.price.


eq_price_supply <- function( give_s){ 

    fn_market <- function(p, s = give_s ) { 7 - p - log(p) - s }

    fn_descant(x0 = 0.01, x1 = 19, f = fn_market, error = 0.000001, max_rep = 4)

}

# 
eq_price_supply( give_s = 2)

# 4) How the price change when supply increases
# Numerical derivation

fn_derivates <- function(f, a, h = 0.001){ (f(a+h) - f(a))/h }

fn_derivates(f = eq_price_supply, a = 2)



