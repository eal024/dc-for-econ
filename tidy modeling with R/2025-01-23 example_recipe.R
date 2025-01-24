

library(tidyverse)
library(tidymodels)

vec_fk <- c(1,2)
time <- -5:5
income <- sample( x = seq(from = , to = 3, by = 0.1 ), size = length(time)*length(vec_fk), replace = T)

# Sim data
df <- tibble( fk = rep( x = vec_fk, each = length(time))  , time = rep(time, times = length(vec_fk)), income = income)


df1 <- df |> 
    filter( time < 2) |> 
    pivot_wider(
        names_from = time, values_from = income
    )

df1 


?step_dummy