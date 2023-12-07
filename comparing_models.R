

# packages and data
library(tidyverse)
library(tidymodels)
library(mlbench)
data(BostonHousing)

df <- BostonHousing |> mutate( chas = as.numeric(chas)) |> as_tibble()


# Splitting data
split <- initial_split( df, prop = 0.8, strata = medv)
train <- training( split)
test <- testing(split)

#  Models
ols <- linear_reg( ) |> set_engine("lm")

# 
optimal_lambda <- 0.1
lasso <- linear_reg( 
        penalty = optimal_lambda,
        mixture = 1) |>
    set_engine("glmnet")

# Random Forest
optimal_mtry <- 10
optimal_min_n <- 4
rf <- rand_forest( 
    mode = "regression",
    trees = 400,
    mtry = optimal_mtry,
    min_n = optimal_min_n
    )  |> 
    set_engine("ranger")


# Fit XG Boost
xgb <- boost_tree(
    mode = "regression",
    trees = 50,
    mtry = 4 
    ) |> 
    set_engine("xgboost")


# Fiting models
lasso_fit <- workflow( ) |> 
    add_recipe( recipe( medv ~., data = train)) |> 
    add_model( lasso)  |> 
    fit( data = train)


# 
model_list <- list( 
    ols = ols,
    lasso = lasso,
    rf = rf,
    xgb = xgb
    )

# 
fn_fit <- function(model){ 
    workflow() |>
    add_recipe( recipe( medv ~., data = train)) |> 
    add_model( model )  |> 
    fit( data = train)
}


# Fit each model
model_fit <- map( model_list, \(x) fn_fit(x))

# predict 
df_pred <- map(model_fit, \(x){
    predict(x, new_data = test ) |> 
        mutate( .true = test$medv
        )
        } ) |> 
    bind_rows( .id = "model")


df_pred |> 
    ggplot( aes(  x = .true, y = .pred)) +
    geom_point( ) +
    geom_abline(intercept = 0,slope = 1) + 
    facet_wrap( ~ model)

