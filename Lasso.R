

# Pakker og data
library(tidyverse)
library(tidymodels)
library(mlbench)
data(BostonHousing)

# Boston Housing data
df <- BostonHousing |> janitor::clean_names() |> as_tibble()

# Create a recipe. 
rec <- recipe( medv ~ ., data = df) |> 
    step_dummy( chas, one_hot = T)

# Lasso quick and dirty
lasso <- linear_reg( mixture = 1, penalty = .1) |>
    set_engine("glmnet")  |> 
    fit(  medv~., data = df)

model_lasso <- linear_reg( mixture = 1, penalty = .1) |> set_engine("glmnet")

# Fit with workflow
lasso_fit <- workflow( ) |> 
    add_recipe( rec) |> 
    add_model(model_lasso) |> 
    fit(data = df )

# Look at the variables
lasso_fit |> tidy()

# See how the coeffisients change from Penelty
fn_lasso <- function( p){
    model <- linear_reg( mixture = 1, penalty = p) |> set_engine("glmnet")
}

# Helper function. Do loop over the recipe for each penalty
fn_lasso_fit <- function( p){
    workflow( ) |> 
    add_recipe( rec) |> 
    add_model(fn_lasso( p = p)) |> 
    fit(data = df )
}

penalty_variation <- 10^seq(from=-4, to=2, length.out = 20)

list_lasso_fit <- map( penalty_variation, \(x) fn_lasso_fit(p = x) ) |> 
    set_names( penalty_variation )

df_term <- map(list_lasso_fit, \(x) x |> tidy()) |> 
    bind_rows( )

# Graph the result
df_term |> 
    filter( term %in% c("crim", "indus", "age")) |> 
    ggplot( aes( y = estimate, x = penalty, color = factor(term)) ) +
    geom_line()


## Extraction variables----------------------------------------------------------
lasso_fit |> 
    extract_fit_parsnip( ) |> 
    tidy()



# Predict function---------------------------------------------------------------
predict( lasso_fit, new_data = df) |> mutate( .true = df$medv)



