

# Seminar for week 36

# Exercise 1: Eastern is first moon after 21 March-
# Fin the date for Eastern in year Y

# test of mod
8%%3
8-(3*2) == 2
8%%3

# defination variables
# a <- Y%%19
# b <- Y%/%100
# c <- Y %% 100
# d <- b %/% 4
# e <- b %% 4
# h <- (15+19*a+b-d-g) %%30
# i <- c %/% 4
# k <- c %% 4
# l <- (32+2*e+2*i-h-k)%%7
# m <- (a+11*h+19*l) %/%433

# # month
# mnd <- (90+h+1-7m) %/% 25
# 
# # Day
# 19+h+1-7*m+33*mnd

# Function that gives the date for Eastern in year Y
fun_eastern_counter <- function(Y){
    
    a <- Y %% 19
    b <- Y %/% 100
    c <- Y %% 100
    d <- b %/% 4
    e <- b %% 4
    g <- (13+8*b) %/% 25
    h <- (15+19*a+b-d-g) %%30
    i <- c %/% 4
    k <- c %% 4
    l <- (32+2*e+2*i-h-k)%%7
    m <- (a+11*h+19*l) %/%433
    
    # month
    mnd <- (90+h+l-7*m) %/% 25
    
    # Day
    day <- (19+h+l-7*m+33*mnd) %% 32
    
    list( mnd = mnd, day = day)
}

fun_eastern_counter(Y = 2021)

## 2. Days after March 21.
fun_day_after_march21 <- function(Y){
    
    if( fun_eastern_counter(Y = Y)$mnd == 3 ){
        (fun_eastern_counter(Y = Y)$day - 21)
    }else{
        (fun_eastern_counter(Y = Y)$day + 31-21)
    }
}

i <- 0

fun_seq_eastern_start <- function(from, to){
    l <- list()
    teller <- 1;
    for(i in from:to){
        l[[teller]] <- list( `mnd and day` = fun_eastern_counter(Y = i),
                             day_after = fun_day_after_march21(Y = i)
                             )
        teller = teller + 1
    }
    l
    }

list_eastern_c <- fun_seq_eastern_start(from = 2000, to = 2004)

# Set names to elements
fr <- 1900
t <- 2200
list_eastern_c <- fun_seq_eastern_start(from = fr, to = t) |> 
    set_names(
        paste0("year", fr:t )
        )


# Which day?
vec_days_after <- map_dbl( list_eastern_c,2)


# 4 
vec_days_after |> 
    hist()
abline( v = mean(vec_days_after), col  = "red")


## Ex. 2

# Fibonacci: c(1,1,2,3,5,8)

fun_seq_Fibonacci <- function(num){
    n <- num
    v <- vector()
    for( i in 1:n){
        if( i == 1 | i == 2 ){
            v[i] <- 1
        }else{ v[i] <-  v[i-1] +v[i-2]}
    }
    v
}

fun_seq_Fibonacci( num = 6)

fibo <- function(n){
    if( n < 3){ return(1)}else{ fibo(n-1) + fibo(n-2)}
}

map_dbl(1:10, \(x) fibo(n = x))

microbenchmark::microbenchmark(
    `for loop` = fun_seq_Fibonacci( num = 6),
    recursive = map_dbl(1:6, \(x) fibo(n = x))
)


## 3 The Lotka-Volterra model
# the size of the population of two species


periods <- 200
R <- vector(length = periods)
Fox <- vector(length = periods)

R[1] <- 80
Fox[1] <- 20

#
a <- 0.07
b <- 0.002
c <- 0.02
d <- 0.0025
for( i in 2:periods){
      R[i] = (1+a)*R[i-1] - b*R[i-1]*Fox[i-1]
    Fox[i] = (1-c)*Fox[i-1] + d*R[i-1]*Fox[i-1]
}

df <- tibble( 
    time = 1:length(R),
    rab = as.integer(R),
    fox = as.integer(Fox)
)

df |> 
    pivot_longer(-time) |> 
    ggplot( 
        aes(x = time, y = value, fill = factor(name), color = factor(name))
        ) +
        geom_line()


# Appendix----------------------------------------------------------------------



# # is leap year?
# fun_is_leap_year <- function(Y){
#     ifelse(
#         ( Y %% 4 == 0 & Y %% 100 != 0) | Y %% 400 == 0,
#         TRUE,
#         FALSE
#            )
# }


