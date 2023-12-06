


# Cumsumers problem

# a) Construct a function of agents utility from goods x and y

utility <- function(x, y, g = 0.5, w = 2, tau = 0.3){
    x^(1-g)/(1-g) + w*(y^(1-tau))/(1-tau)
} 

# Utility from x= 2 and y = 2
utility(x = 2, y = 2)

# px and py is the price for each goods.
# R <- px*x + py*y

# c) Utility of values of X, givens budget constrain.

# Budget constrain as a function of R,x,price
fn_budget <- function( R = 10, px = 1, py = 2,x ){ (R - px*x)*(1/py) }

# utility for x = 2
utility( x = 2, y = fn_budget( x = 2))

# For different levels

xvec <- seq( from = 0.1, to = 5, by = .1)

sapply( xvec, \(x) utility(x = x, y = fn_budget(x = x)))

# Consumers demand for X
# Choice X from income R

# Optimal choice of X, based on R
fn_demand_x <- function(x){ -utility(x = x, y = fn_budget(R = 10, x = x))  }

# Example
opt <- optim( par = 2, fn = fn_demand_x, method = "BFGS")

# The demand for X, ginved R 
opt$par

# The demand for y, given X
fn_budget( R = 10, px = 1, py = 2, x = opt$par)

# c) The Engel-curve

# Find the optimum for differnet values of R
x_opti <- list()
R_list <- seq(from = 4, length.out = 10, by = 1)
for( i in 1:10){

    # Change the function, The R-value
    fn_demand_x <- function(x){ -utility(x = x, y = fn_budget(R = R_list[i], x = x))  }  #   

    # Find the optimum
    opt <- optim( par = 2, fn = fn_demand_x, method = "BFGS")

    x_opti[i] <- opt$par
    

}

# c) The Engel-curve
df <- data.frame( R = R_list, x = x_opti |> unlist() )
plot( df$R, df$x, main = "demand for X, as R increases", type= "ln", xlab = "R-value", ylab = "Amount of X")


# The Engel-curve for Y
# Y  defined from BB
df_y <- data.frame( 
    Y =  purrr::map2_dbl(df$R, df$x, \(R,X) fn_budget( R = R, x = X) ),
    R = R_list
    )

fn_budget( R = 10, x = 5)

plot( df_y$R, df$Y, main = "demand for X, as R increases", type= "ln", xlab = "R-value", ylab = "Amount of Y")

library(tidyverse)

# Graphs of both goods.
df |> 
    left_join( df_y, join_by(R)
    ) |> 
    pivot_longer(-R) |> 
    ggplot(aes(x = R, y = value, fill= factor(name), color = factor(name)
        ) 
    ) +
    geom_line() 


# d) The demand curve as a function of price

pxvec <- seq( from = 0.1, to = 3, length.out = 100)
x_opti_price <- list()
for( i in 1:length(pxvec)){

    # Change the function, The R-value
    fn_demand_x <- function(x){ -utility(x = x, y = fn_budget(R = 10, x = x, px = pxvec[i]) ) }    

    # Find the optimum
    opt <- optim( par = 2, fn = fn_demand_x, method = "BFGS")

    x_opti_price[i] <- opt$par
    

}

df_demand_x <- tibble( 
    x     = x_opti_price |> unlist(),
    price = pxvec
    ) 

df_demand_x |> ggplot(aes( y = x, x = price) ) + geom_line()


