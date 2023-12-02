
# Integration
library(mosaicCalc)

# a)
f <- function(x) {(2*x+5)/(x^2+1)}

F <- mosaicCalc::antiD((2*x+5)/(x^2+1)~x)

# Solution
F(1) - F(0)

# Monte Carlo

draw <- 1000
x <- runif(draw, 0, 1)

y <- f(x)

# Montecarlo Integration (mean over x )
mean(y)


# b ) 

I <- (5*pi)/4 + log(2)

x_rep <- c(10,100,1000,10000, 100000)


fn_mc_I <- function(nr_draws, f, a = 0, b = 1) {
    #x <- runif(draws, a,b) # uniform distribution    
    
    l_drw_x <- map(nr_draws, \(x) runif(x, a,b) )
     
    map_dbl(l_drw_x, \(x) mean(f(x) ) ) # integral
}

fn_mc_I(nr_draws = x_rep, f = f)

set.seed(123)
tibble(
    rep = x_rep, 
    Itrue = I,
    Imc = fn_mc_I(nr_draws = x_rep, f = f, a = 0, b = 1)
    ) |> 
    pivot_longer(-rep)  |> 
    ggplot( aes(y = value, x = rep, color= factor(name) ) ) +
    geom_point() +
    geom_line()


# The MC converge toward the true value.

# How does accuracy depend on the number of draws?

# To say something aboot accuracy

fn_mc_i <- function( rep, drw){

    map_dbl(1:rep,\(x) fn_mc_I( nr_draws = drw, f = f) )

    
}

estimated_integ <- fn_mc_i(rep = 100, drw = 100)

mean(estimated_integ)
sd(estimated_integ)


df_sim <- tibble( 
    drw = seq(from = 10, to = 100000, length.out = 100),
    mean_estimate = map(drw,\(x)  fn_mc_i(rep = 100, drw = x) |> mean() ),
    sd_estimate = map(drw,\(x)  fn_mc_i(rep = 100, drw = x) |> sd()/(100)^0.5 ) 
    ) |> 
    unnest( col = c(mean_estimate,sd_estimate) 
    )



df_sim |> 
    pivot_longer(-drw) |> 
    ggplot(aes(y = value, x = drw, color = factor(name))
    ) +
    geom_line() +
    geom_point( ) +
    facet_wrap(~name, scales = "free")


# c)

ll <- map(1:3, \(x) runif(1000, 0,1 ) )

fn_mid_prule <- function(a,b, f){
    x1 <- (a+b)/2
    (b-a)*f(x1)
    }

fn_mid_prule(a = 0, b = 1, f = f)

sim_df <- tibble( 
    from = seq(from =0, to = 1, length.out = 1000),
    to = lead(from)
    ) |> 
    na.omit() |>  
    mutate(
        midp = map2(from, to , function(f,t){ fn_mid_prule(a = f, b = t, f = f)})
    ) |> 
    unnest(midp) 




sim_df |> summarise( sum = sum(midp))

5*pi/4 + log(2)


## Compare the accurace

fn_midtpoint <- function(a,b, div){
    #
    interval <- seq(from = a, to = b, length.out = div)

    from <- interval[-div]
    to <- lead(interval) |> na.omit()

   int <- map2(from, to, \(x,y) c(x,y)fn_mc_i)
   
   sum(
    map_dbl( int, function(x) fn_mid_prule(a = x[1], b = x[2], f = f) )
   )
}

mean(fn_midtpoint(a = 0, b = 1, div = 4))
mean(fn_midtpoint(a = 0, b = 1, div = 10))
mean(fn_midtpoint(a = 0, b = 1, div = 100))
mean(fn_midtpoint(a = 0, b = 1, div = 1000))

df <- tibble( 
    drw = seq(from = 6, to = 100000, length.out = 40),
    mean_estimate = map_dbl(drw,\(x) fn_midtpoint(a = 0, b = 1, div = x) |> mean() )
)

df2 <-tibble( 
    drw = seq(from = 6, to = 100000, length.out = 40),
    mean_estimate = map_dbl(drw,\(x)  fn_mc_i(rep = 100, drw = x) |> mean() )
    )

# Comparing the two methods:
df |> 
    ggplot( aes( y = mean_estimate, x = drw) ) +
    geom_line() +
    geom_line( data = df2,
        aes( y = mean_estimate, x = drw),
        inherit.aes = F, 
        color = "red" 
    )

