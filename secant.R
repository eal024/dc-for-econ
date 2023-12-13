
## Test function. Obs intercetion with y = 0 in two points
test_function <- function(x){ (x^2 + 2*x -1) }

fn_secant <- function(f, x0, x1, maxit = 100, eps = 0.001){

    # Start with two points x0,x1
    xn = x1
    xn0 = x0 
    itr = 0
    d = 100
    # Find a gess 
    while( d > eps ) {
        xn1 = xn - ( (xn-xn0)/(f(xn)-f(xn0)) )*f(xn)

        d = abs( xn1 - xn)  # Calculate the diff between iterreations
        itr = itr + 1
        xn0 = xn
        xn = xn1
    }

    return(xn1)
}

fn_secant( f = test_function, x0 = -4, x1 = -1, maxit = 10)


