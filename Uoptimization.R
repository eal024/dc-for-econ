
library(tidyverse)

# The utility optimzation problem. 
# A given budget, and a given U-function

# U = x^2*0.25*y
# R = p1*x + p2*y

U <- function(x, y){ x^2*0.25*y}

# Budget line respect to y
BL <- function(R, p1 , p2, x){ (R - x*p1 )*(1/p2) }

U(x = 1, y = 1)

# Utility for some consumtion.
map2_dbl( 1:4, 1:4, \(x,y) U(x,y))

# How find the optimum, given R?

df <- tibble( 
    u = map_dbl(6:14,\(i) U( x = i, y = BL(R = 45, p1 = 3, p2= 1.5, x = i) ) ),
    x = 6:14
    )

# Utility by X
ff <- function(i){ U( x = i, y = BL(R = 45, p1 = 3, p2= 1.5, x = i) ) }

# optim. problem
df |> 
    ggplot(aes(x = x, y = u)) +
    geom_line()


# Find optimum of ff (utilty by x)
deriv <- function(a,h = 0.0005, f = ff){ (f(a+h)-f(a))/h }

# Use the Decant algorithm
fn_descant(x0 = 1, x1 = 10, f = deriv, max_rep = 5)


# Solving the optimum by traditional optim
library(Ryacas)

Ryacas::

x <- Sym("x")
y <- Sym("y")
U <- Sym("U")

total_budget <- 45
p1 <- 3
p2 <- 1.5

n_x <- total_budget/p1
n_y <- total_budget/p2

budget <- function(x) (-n_y/n_x)*x + n_y
tibble( x = 1:5, b = budget(x = x)) # Look at how the budget works

# U
U <- function(x, y){ x^2*0.25*y}


# The Utility rewrite as y
u_y <- function( my_x, my_u){
    solved   <- yac_str( solve( U(x,y) == U, y ) )
    solved
    #solution <- Eval( solved, list(x = my_x, U = my_U))

    #solution
}

u_y(my_x = 4, my_u = 4)
