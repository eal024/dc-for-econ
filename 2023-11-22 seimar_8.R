
#
library(tidyverse)

# data
df <- read_csv("data/accidents.csv")


ml_function <- function(y, x, alfa, beta){
    
    y*(alfa + beta*x) - exp(alfa + beta*x)
}


sum(
    map2_dbl( df$accidents,df$speed,
      function(y,x)
          ml_function(y = y, x = x, alfa =1, beta = .5)
          )
)



fn_derivates( f = ml_function(), )

