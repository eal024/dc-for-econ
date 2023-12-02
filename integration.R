
## The use of R to find out a numerical answer to an n-fold integral.


# The simple case
f_x <- function(x) x

# Example 2
f_ex2 <- function(x) {1/((x+1)*x^0.5)}

# 1) Base R
integrate(f = f_x, lower = 0, upper = 1)
integrate(f = f_ex2, lower = 0, upper = Inf)

# 2) Packages mosaicCal ---------------------------------------------
library(mosaicCalc)

# Anti derivation
# Example 1
F <- antiD( x ~x, a = 1)

# Return symbolic form when possible. If not, return numerical integreation

P <- antiD( exp(x^2) ~x)
print(P)

# When the F also is a integreal
one <- makeFun(1~x + y)
by_x <- antiD(one(x =x , y = y) ~x)
by_xy <- antiD( by_x( x = 4, y = y) ~ y)

by_x(x = 10, y = 2, C = 2)


# Example one
f <- makeFun( x^2 )

F <- antiD(f)

# How to find area.
F(x = 4, C = 4) -  F(x = 3, C = 4)


# 3) Monte Carlo integration  -----------------------------------

# The function (example)
fn_example2 <- function(x) x^2

num_p <- 1000
# Generate random points in [0,1]
x <- runif(num_p, 0,1)

# Evaluate at random points
y <- fn_example2(x = x )

# The average over [a,b]
mean(y)

# Multiply by the interval width
mean(y)*1

# The standard devations
sd(y)/num_p^0.5





























