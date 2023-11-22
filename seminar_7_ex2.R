
library(tidyverse)

# Market equilibrium

# D(p) = 5-p
# S(p)= A log(p)
D <- function(p) 5-p
S <- function(p, A = 1) A*log(p)

# 1) a = 1
market <- function(p){ D(p)-S(p)}
# Bisection algorithm

# i) Guess m = (a+b)/2.
# f(m) same sign as f(b) or f(a)?
# Ex. f(x) -> keep [a, m], calculate n = (a+m/2)  
# repeat, 
## Repeat N times, where error is samller than e 


## bisection 

f <- function(p) { 5-p- 1*log(p) }

# return( paste0("ok", "f(a) =", f(a), "f(b) =", f(b), " og f(m)", f((a+b/2)) )  )

fn_bisection <- function(a,b,f, epsilon){
    
    if( ( f(a) > 0 & f(b) > 0) ){  stop("No interception") }
    if( ( f(b) < 0 & f(a) < 0) ){  stop("No interception") }
    
    #  
    m = (a+b)/2
    start = a
    slutt = b

        # 
    while( abs(f(m))  >= epsilon){
        #
        if( f(p = start) > 0 & f(p = m) < 0 ) {
            start = start
            slutt = m
        }
        #
        if( f(p = start) > 0 & f(p = m) > 0 ) {
            start = m
            slutt = slutt
        }
        
        m = (start+slutt)/2
    }
    
    return( c(start, slutt) )
}

market(p = 1)
market(p = 10)
fn_bisection(a = 1, b = 10, f = market, epsilon = 0.0005)[[1]] |> 
    market()


# Test of function
fn_bisection(a = 1, b = 4, f = market, epsilon = 0.00005)



# Algorithm two: Secant algorithm
fn_secant <- function(x0,x1,f, tol){
    
    # Two starting point
    x1 = x1
    x0 = x0
    # tol: The difference between two points
    diff = x1 - x0
    while( abs(diff) > tol){
        #
        
        x2 = x1 - f(x1)/((f(x1)-f(x0))/(x1-x0) ) 

        
        
        print( paste0( "C: ",diff) )
        x0 = x1
        x1   = x2
        
        diff = x1 - x0        
    }
    
    return(x2)    
    
}

fn_secant(x = 1, x1 = 10,market, tol = 0.01)


# a = 1
# b = 2
# market(p = a-(a-b)/( f(a)-f(b) ) )
2- ( (2-1)/( market(2)-market(1) )) 
fn_secant(x0 = 10, x1 = 50, tol = 0.05, f = market)


## The Newton algorithm
library(numDeriv)

genD( func = market, x = 2)$D[1]

# Alternative
# ## Derivative
# 
# fn_der <- function(f,a, h){
#     (f(a+h)-f(a))/h
#     
# }
# 
# fn_der( f = market, a = 2, h = .00001)
# 
# genD(func = market, x = 2)$D[1]


# Function derivatives
fn_der <- function(f,a, h){
    (f(a+h)-f(a))/h
    
}

numDeriv::grad(market, 1)
fn_der( f = market, a = 1, h = 0.0000001)
hessian(market, 1)

fn_newton <- function(x1, f, tol){
    
    x2 <- NULL
    f(x1)
    while( abs(f(x1)) > tol){
    
        # x2 = x1 - (f(x1)/numDeriv::genD(func = f, x = x1)$D[1])
        x2 = x1 - (f(x1)/fn_der(f = f, a = x1, h = .0001))
        
    #diff = abs(x2-x1)
    x1 = x2
    }
    return(x2)
    
}

#
market(100)
9.7 + market(9.7)/fn_der(f = market, a = 9.7, h = .001)

fn_newton(x1 = 100, f = market, tol = 0.05)
fn_secant(x = 1, x1 = 10, f = market, tol = 0.005)
fn_bisection(a = 1,b =10, f = market, epsilon = .1)


# The cobweb model --------------------------------------------------------

# a) 
S <- function(p) 2*(p)^0.5
D <- function(p) 10-p
P <- function(q) 10-q

# b)


market_seq <- function(p0, itr){ 
    for( i in 1:20){
        
        price = p0
        
        si = S(p = price)
        di = D(p = si)
        pi = P(q = di)
        
        print( paste0("itr: ", i, " S: ", si, " D: ",di, " Price: ", pi) )
        p0 = pi
        
        
    }
}

market_seq( p0 = 2, itr = 20)

# C)
D <- function(p){ 10 - p + rnorm(n = 1, 0, 1)}

market_seq <- function(p0, itr){ 
    for( i in 1:20){
        
        price = p0
        
        si = S(p = price)
        di = D(p = si)
        pi = P(q = di)
        
        print( paste0("itr: ", i, " S: ", si, " D: ",di, " Price: ", pi) )
        p0 = pi
        
        
    }
}
market_seq( p0 = 2, itr = 20)

# d) Solve price p
market_two <- function(p) {S(p)-D(p) }

market_two(4)
market_two(10)

set.seed(123)
fn_bisection(a = 10, b = 4,f = market_two, epsilon = 0.0005)
fn_secant(x0 = 10, x1 = 1, f = market_two, tol = 0.0005)
fn_newton(x1 =10, f = market_two, tol = 0.0005)











