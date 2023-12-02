

library(mosaicCalc)

# Anti derivation
F <- antiD( a*x^2 ~x, a = 1)

print(F) # The result

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


# Monte Carlo integration

antiD( x^2 ~ x)

f <- function(x) x^2

num_p <- 1000
# Generate random points in [0,1]
x <- runif(num_p, 0,1)

# Evaluate at random points
y <- f(x)

# The average over [a,b]
mean(y)

# Multiply by the interval width
mean(y)*1

# The standard devations
sd(y)/num_p^0.5





























