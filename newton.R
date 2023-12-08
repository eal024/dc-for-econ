


## Algorithm for finding solution to equations

test <- function(x) { (x-5)/(x+7)^0.5 - 5 + log(x)}

# Function goes from minus to abbove 0
sapply( c(10:20), \(x) test(x))


# Find the solution

# First we need a helper:

fn_derive <- function(f, a, h = 1e-7){ (f(a+h)-f(a))/h }

# Newton: bases the argument on f(x) = f(a) + f`(a)(x-a)

fn_newton <- function(f, x0, lim = 1e-5){

    xn = x0 # Start value
    error = 1
    while( error > lim ) {   
        
        # Next step
        xn1 = xn - ( f(xn)/fn_derive(f = f, a = xn) ) 

        xn = xn1 # Change the x
        error <- abs( f(xn) )
         
    }
    return(xn)
}

# Return value where eqution simmular to 0 
fn_newton(f = test, x0 = 10)

#test in the equation
test(x = 15.7)