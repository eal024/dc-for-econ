

# 
# install.packages("ineq")
library(tidyverse)
library(ineq)

df <- tibble( individ = c(1:3),
        income = sample( x = seq(10,50,10), replace = T, size = 3 )
        )

# The Gini calculated
ineq::Gini(df$income)

fn_gini <- function(vec_income, n){
    
    # The numerator
    sum <- sum(map_dbl(vec_income, \(x) sum(abs(x-vec_income))))
    # The denominator
    div <- 2*n^2*mean(vec_income)
    
    sum/div
}

fn_gini(vec_income = df$income, n = nrow(df))


# Incomes
inc <- readr::read_csv("data/incomes.csv")
fn_gini( inc$income, nrow(inc))
ineq::ineq(inc$income)


## 2) Alterantiv formula
fn_gini2 <- function(vec_income, n){
    (2*sum(1:n*sort(vec_income)))/(n^2*mean(vec_income))-(n+1)/n
}

fn_gini2(vec_income = df$income, n = nrow(df))
fn_gini2(vec_income = inc$income, n = nrow(inc))

## 3) Measuring time formula
microbenchmark::microbenchmark(
    gini1 = fn_gini(vec_income = inc$income, n = nrow(inc)),
    gini2 = fn_gini2(vec_income = inc$income, n = nrow(inc))
)


## Bootstrapping: Redraw data 200 resamplings
## Compute and store the data

tbl <- tibble( i = 1:200,
               data = map(1:200,\(x) sample( x = inc$income, size = nrow(inc), replace = T))
        ) |>  
    mutate( gini = map(data, \(x) fn_gini2( x, n = length(x)) ))


tbl |> 
    unnest( gini) |> 
    summarise( mean = mean(gini),
               bootstrap_sd = sd(gini)
               )














