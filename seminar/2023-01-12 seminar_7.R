

# Models
market <- function(p, a = 1) {5-p-a*log(p)}

#
market(p = 3)

fn_bisection <- function(f, inna,innb, N, error) {
    
    if(f(inna) > 0 ){
        a <- inna
        b <- innb
    }

    if( f(inna) < 0){
        a <- innb
        b <- inna
    }

    if( (f(a) > 0 & f(b) > 0) ){ stop("a og b gir begge positiv")}
    if( (f(a) < 0 & f(b) < 0) ){ stop("a og b gir begge negativ")}


    m <- (a+b)/2
    i <- 0
    e <- 100
    added <- FALSE

    while( e > error ){
        
       m = (a+b)/2
       # print( paste0(m, " f(a)", f(a) |> round(2), "  f(b)", f(b) |> round(2) ) )
       if( f(a) > 0 & f(m) < 0){
        b = m
       } 

       if( f(a) > 0 & f(m) > 0){
        a = m
       }

        #print(paste0("i :", i, " m", m, " f", f(m)))
        i = i + 1
        
        e <- abs(f(m))
       
    }
   
   m
}

fn_bisection(f = market, inna = 10, innb = 0.5, N = 10, error = 0.000000005)

market(3.693441)

# Equilibrium price and Q
a_vec <- seq(from = 0.1, to = 5, length.out = 10)
eq_price <- list() 

for(i in 1:length(a)){

    market <- function(p, a = a_vec[i]) {5-p-a*log(p)}
    eq_price[i] <- fn_bisection(f = market, inna = 10, innb = 0.5, N = 10, error = 0.0000005)
} 

library(tidyverse)

df <- tibble( a = a_vec, price_eq = eq_price |> unlist() )

df |> ggplot( aes( y = price_eq, x = a) ) + geom_line()


#map(a, \(x) fn_bisection(f = market, inna = 10, innb = 0.5, N = 10, error = 0.000005) )