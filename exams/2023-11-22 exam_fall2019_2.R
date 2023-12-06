

## The Cobweb-model

# a) 

# S(p) = 2(p)^0.5
# D(p) = 10-p
# P(q) = 10-Q

# Write them as function

S <- function(p) 2*p^0.5
D <- function(p) 10-p
P <- function(q) 10-q

# b) p0 = 2, Construct a loop 20 periods

pi = 4
for(i in 1:20){
    qi = S(p = pi) # First S(pe) 
    pp = P(q = qi)
    
    print( paste0("In period ", i, " the Pe is ", round(pi,2),
                  " , which give Q= ", round(qi,2),
                  " and p",i, ": ", round(pp,2) )
           )
   pi = pp 
}


# Price converge

# c) With a random error term
Perror <- function(q) 10-q + rnorm(n = 1, mean = 0, sd = 1)

fn_the_cobweb <- function(p0, n, f){
    
    pi = p0
    for(i in 1:n){
        qi = S(p = pi) # First S(pe) 
        pp = f(q = qi)
        
        print( paste0("In period ", i, " the Pe is ", round(pi,2),
                      " , which give Q= ", round(qi,2),
                      " and p",i, ": ", round(pp,2) )
        )
        pi = pp
    }  
    
}

fn_the_cobweb(p0 = 0, n = 20, f = P)
fn_the_cobweb(p0 = 2, n = 20, f = Perror)

# d) Solve the price level p numerical

# Preferred algorithm

# 1) Bisection

fn_bisection <- function(a,b,f, error ) {
    
    # Need this condition to fulfill
    try( if( (f(a) > 0 & f(b) > 0) | (f(a) < 0 & f(b) < 0) ) stop("same sign") )

    if( f(a) < 0 & f(b) > 0){
        mid = a
        a = b
        b = mid
    }
    
    m = (a+b)/2

    while(  abs( f(a) ) >= error){
        
        if( f(a) > 0 & f(m) > 0){
            a = m
        } else{
            b = m
        }
        m = (a+b)/2
    }
    
    #print(f(a))
    return(c(a,b))

}


fn_bisection(a = 1, b = -2, f = function(x) x, error = .001)

fn_market <- function(p){
    S(p) - D(p)
}

fn_market(10)
fn_bisection(a = 10, b = 3, f = fn_market, error = .000001)


















