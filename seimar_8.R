

# Seminar 8 

# 1) Utility max

# a) Utility function
U <- function(x,y, gamma = 0.5, omega = 0.3, w = 2){
    (x^(1-gamma))/(1-gamma) + w*(y^(1-omega)/(1-omega))
}

# Find the U for x = 2, y = 2
U(x = 2, y = 2)

# b) R = 10, px = 1, py = 2

# R = p1x1 + p2y
# y = (R - p1x1)*(1/p2) = ((R-p1x)*(1/p2))

partialU <- function(x, gamma = 0.5, omega = 0.3, w = 2, R = 10, p1 = 1, p2 = 2) {
    (x^(1-gamma))/(1-gamma) + w*( ( (R-p1*x)*(1/p2) )^(1-omega)/(1-omega))
    
}

partialU(x = 1)

# Utility for (0.1, 5)

x_levels <- seq(from = 0.1, to = 5, length.out = 20)

# Utility for varity of levels
map_dbl(x_levels, \(x) partialU(x = x))

# approx_derive <- function(f,a, h){ (f(a+h) - f(a))/h }

tibble( U = map_dbl(x_levels, \(x) partialU(x = x)), 
        x = x_levels
        ) |> 
    ggplot( aes( x= x_levels, y = U) ) +
    geom_line()

# The consumers demand for the two goods
fn_derivates <- function(f, a, h){ ((f(a+h)-f(a) )/h )}

partialU_der <- function(p){
    fn_derivates(f = partialU,
                 a = p,
                 h = 0.001
                 )
}

# Gives the derivates
partialU_der(p = 3)
fn_secant(f = partialU_der, x0 = 1, x1 = 3 , tol = 0.01)
fn_bisection(a = 1, b = 4, f = partialU_der,  0.001)

# Y follows x
y <- 10 - 1*2.25 

# c) The relationsship between demand and income

partialU <-
    function(x,
             gamma = 0.5,
             omega = 0.3,
             w = 2,
             R = 10,
             p1 = 1,
             p2 = 2) {
        (x ^ (1 - gamma)) / (1 - gamma) + w * (((R - p1 * x) * (1 / p2)) ^ (1 -
                                                                                omega) / (1 - omega))
        
    }


## d) The engel curve
# R = x*px + y*py
# x = (R-y*py)*(1/px)

# optim y = p2^(-1/omega)
# Engel-curve: The change in demand for X, when R changes
engel_x <- function(R, omega = .3, gamm = 0.5, w = 2, p2 = 2, p1 = 1){
    (R-(1/w)*p2^(-1/omega))*(1/p1)
}

tibble( x = 1:10, engel = engel_x(R = 1:10, p2 =2, w = 4, p1 =1)) |> 
    ggplot(aes(x = x, y = engel) 
           ) + 
    geom_line( )


# d) The demand
tibble( price = 1:10, demand = engel_x(R = 10, p2 =2, w = 4, p1 =1:10)) |> 
    ggplot(aes(x = price, y = demand) 
    ) + 
    geom_line( )

