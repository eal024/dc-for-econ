

# a)
U <- function(x,y, g = 0.5, tau = 0.3, w = 2){
    x^(1-g)/(1-g) + w*(y^(1-tau)/1-tau)
}


# Utility at given point
U(x = 2, y = 2)


# b) y given by the budget constraint

U_levelx <- function(x, R = 10, p1 = 1, p2 = 2){
    U(x = x, y = (R-p1*x)*(1/p2) )
}

x_lev <- seq(from = 0.1, to = 5, length.out = 40)

df <- tibble( u = map_dbl( x_lev, \(x) U_levelx(x = x) ),
        x = x_lev
        )

#
df |> 
    ggplot( aes(y = u, x = x)
            ) +
    geom_line()


# c) Engel curve good x

# X-return from different values of R
deriv <- function(a,f, h){ (f(a+h)-f(a))/h } 

deriv_u <- function(x){
    deriv( f = U_levelx, a = x, h = 0.01)
    
}  

fn_bisection(a = 2, b = 10, f = deriv_u, error = 0.001)




# Logic behind the X-demand

# Crate a list for storing x
x_dem <- list()

for(i in 6:10){
    
    # Demand for X, is 3.95, when R = 10
    # Change the function for each R = i
    fU_levelx <- function(x, R = i, p1 = 1, p2 = 2){
        U(x = x, y = (R-p1*x)*(1/p2) )
    }
    
    # Change function derivated
    fderiv_u <- function(x){
        deriv( f = fU_levelx, a = x, h = 0.0001)
        
    }  

    # Find where the function is equal 0 based on numerical method
    x_dem[[i]] <- fn_bisection(a = 2, b = 5, f = fderiv_u, error = 0.001)
}


# Do steps over in function, based on R
fn_find_x <- function(i){
    
    fu <- function(x, R = i, p1 = 1, p2 = 2){ U(x = x, y = (R-p1*x)*(1/p2) ) }
    deriv_fn <- function(x){deriv( f = fu, a = x, h = 0.01) } 
    
    fn_bisection(a = 1, b = 5, f = deriv_fn, error = 0.001)[1]
        
}

# To a for loop over R
# Store the data in a tibble
rr <- 4:10 # The R-levels
dem_x <- tibble( R = rr, x = map_dbl(rr, \(x) fn_find_x(i = x)) )


## Demand for Y: Use the Budget constrain to find y based on optimal X
dem_y <- function(r,x, p1 = 1, p2 = 2 ){(r-p1*x)*(1/p2)}

dem_y(r = 10, x = fn_find_x(i = 10))
dem_y(r = 9, x = fn_find_x(i = 9))


## d) Draw the Engel curve
dem_x |> 
    ggplot( aes( y = x, x = R) 
            ) +
    geom_line()


# d) The demand for X, given R = 10, and py = 2
f_dem_x <- 
    function(x, R = 10, px = 1.5, py =2, g = 0.5, w = 2, tau = .3) {
        (x*px) + py*( (x^(-g))*(py/(w*px)) )^(-1/tau) - R 
        }

# 

prices <- seq(from = 0.6, to = 1.4, length.out = 7)

store <- list()

for( i in seq_along(prices) ){
    
    f_dem_x_r <- 
        function(x, R = 8, px = prices[i], py =2, g = 0.5, w = 2, tau = .3) {
            (x*px) + py*( (x^(-g))*(py/(w*px)) )^(-1/tau) - R 
        }
    
    print(
        store[i] <- fn_bisection(a = 1, b =15 , error = 0.5, f = f_dem_x_r)[1]
    )
}

tibble( x = unlist(store), p = prices )








    