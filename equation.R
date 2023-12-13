
# Solving eq.system.-------------------------------

# For solving
library(nleqslv)


# Example: Two equations wiht two variables 
f_equation <- function(x) {

    eq_one <- x[1]^2 + x[2]^2-2
    eq_two <- exp(x[1]-1) + x[2]^3-2

    c(eq_one, eq_two) # 

}

f_equation( x = c(5,5)) # Testing eq.

x0 <- c(5,5)

nleqslv( x0, f_equation)

# Tesing
f_equation(x = c(1,1))


# Ploting the function---------------------------------------
f1 <- function(x) x[1]^2 + x[2]^2-2
f2 <- function(x) exp(x[1]-1) + x[2]^3-2

xs <- seq(-3, 3, by = .1)
ys <- seq(-3, 3, by = .1)

z1 <- NULL
z2 <- NULL

for( x in xs){
    for(y in ys){
        z1 <- c(z1,f1(c(x,y)))
        z2 <- c(z2,f2(c(x,y)))
    }
}

dim(z1) <- c(length(xs), length(ys))
dim(z2) <- c(length(xs), length(ys))
plotly::plot_ly( z = ~z1) |> plotly::add_surface( )
plotly::plot_ly( z = ~z2) |> plotly::add_surface( )
