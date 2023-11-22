


# Intro: # Numerical methods ----------------------------------------------

# A basic example
# Market equilibruim and numeric solution

q_market <- function(p){ 5-p-log(p) }

# q-function (D = S), can not be solved analytically, but by numerical methods
df <- tibble( p = seq(from = 2, to =4, by = 0.1), q = q_market(p = p) )

# plot
ggplot( df,  aes( x = p, y = q)) +
    geom_line() +
    geom_hline( yintercept = 0, linetype = 2)
    


# 2. Solving equations ----------------------------------------------------

# all eq. can be written f(x) = 0

# Some assumption is need to be fulfilled 
    # That f(x) = 0 exist (skjæringspunkt = intermediate value theorem)
    # continuity

# Methods 1) Bisection algorithm

# i) Guess m = (a+b)/2.
    # f(m) same sign as f(b) or f(a)?
    # Ex. f(x) -> keep [a, m], calculate n = (a+m/2)  
    # repeat, 
    ## Repeat N times, where error is samller than e 

# How big can the error be? hat(x) = (a+b)/2
    # worst case x = b
    # max error is therefore (b-a)/2 -> half of the width of the interval

## If we wont to fin error bellow a threshold (err):
    # Start [a,b]
    # Find a mid point m = (a+b)/2
    # THis give -> [a,m] or [ m,b]
    # Repeat unti (b-a)/2 < err (here b (or a) is the last interval (m))
    
# Some notes about error tolerance

## ii) The secant method

## iii) Newton`s method

# Based on f(x) = f(a) + f`(a)(x-a) = 0

# Solve:
# x-a+a = -f(a)/f`(a) +a 
# x = a - f(a)/f`(a)

# using approx when f(x) is non-linear:
# f(x) ≈ f(a) + f`(a)(x-a) = 0


## Criteria for a good algorithm:
    # Fast -- few iterations
    # Robust -- get the solutoin even starting value not very good

## About convergence and robustnes

## Convergence rate-> How fast toward solution ( see slide, conclution -> Newton converge quadritc, fastest)
## Robustnes -> Bisection finds a slotion as long as it exist. Newton and Secant can get lost.
## Trade off: Start with fast algo, if run for a while, swithc to fast


# System of equations -----------------------------------------------------

# Example Ax = b
# soltion: x = A^-1b
# But inverse is costly
# Use solve()

# Example
A <- matrix( c(1,2,-1,2), nrow = 2, ncol = 2)
b <- c(2,1)

solve(A,b)

## When equation is not linear.
# Gauss-Jacobi algorithm: First solve the first equations, then the scond,...
# Once we have solve all, go over all again. and again and again. 
# Keep going until convergence


## Example
library(nleqslv)

# Equations: 
    # x^2,1 + x^2,2 = 2
    # ex,1-1 + x^3,2 = 2

f <- function(x){
    return( c(x[1]^2 + x[2]^2 -2,
              exp(x[1]-1)+x[2]^3-2
              )
            )
}

x0 <- c(5,5)

nleqslv(x0, f)

f(x =c(1,2))
f(x =c(1,1))



# 3) Computing derivates -----------------------------------------------------








