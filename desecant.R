

# 2) Solving equations. Numerical algorithm

# Secant

fn_descant <- function(x0, x1, f, error = 0.005, max_rep = 15){

    # Start points
    xn_1 <- x0 
    xn   <- x1

    i <- 0    # Counting iterations
    e <- 100  # Error start with a large value 


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

# Example of use
market <- function(p, s = 3 ) { 7 - p - log(p) - s }

# Give the value where the eq. is 0
fn_descant(x0 = 0.01, x1 = 19, f = market, error = 0.000001, max_rep = 4)

# Testing
market( p = 2.926271)
