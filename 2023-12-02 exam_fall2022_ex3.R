
# Fall 2022. Intertemporal consumption problem

# Cunsum pr. 2
fn_c2 <- function(c1, r, Y){ (1+r)*(Y-c1)}

# a) The Utility function, from c1 and c2. g = 0.4, and B = 0.96

utility <- function(c1, c2, g = 0.4, B = 0.96){ c1^(1-g)/(1-g) + B*(c2^(1-g)/(1-g)) }

# Person A: Y = 10, r = 0.05

# Utility as a function of c1
utility_c1 <- function(c_pr_1){
    utility( c1 = c_pr_1 , c2 = fn_c2(c1 = c_pr_1, r = 0.05, Y = 10) )
}

# Testing the function
utility_c1( c_pr_1 =  1)

# Utility from 1 to 10
vec_c1 <- 1:10

# Calculating U from 1 to 10, than make a plot
library(tidyverse)

df <- tibble( utility = map_dbl( vec_c1, \(x) utility_c1(x)), c1 = vec_c1 )

# Making the plot
graph <- df |>
    ggplot( aes( x = c1, y = utility) ) +
    geom_line()

graph
# b) Find max, from where the derivat is eq. 0

fn_der <- function(a,f, h = 0.00001){ (f(a+h)-f(a))/h }

fn_der(
    f = utility_c1,
    a = 2
)

fn_derivate_uc1 <- function(a){
    fn_der(
    f = utility_c1,
    a = a
)
    
}

# Use algorithm to find the value 0
fn_descant(x0 = 1, x1 = 7, f = fn_derivate_uc1, max_rep = 4,error = 0.00004 )

# Returned value
p <- tibble( 
    x=  fn_descant(x0 = 1, x1 = 7, f = fn_derivate_uc1, max_rep = 4,error = 0.00004 ),
    y = utility_c1(c_pr_1 = x)
    )

# Grapichal representation
graph + 
    geom_point( data = p, aes(x = x, y = y),
    color= "red",
    size = 6
    )


# c) Return to saving is random

# Cunsum pr. 2, with random return of saving R
R <- function( lambda = 0.9 ){ rexp(n = 1, rate  = lambda)}

fn_c2 <- function(c1, R = R(), Y){ R*(Y-c1)}

