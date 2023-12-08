
# Packages
library(tidyverse)
library(tidymodels)

# II. Predicting high wealth (weight 30 %)

# a) Read data and construct a tibble from data
forbes <- vroom::vroom(here::here("data/forbes_2022_billionaires.csv"))

# Subsamples
forbes_sub <- forbes |>
    select(age, finalWorth, age, category, country, selfMade, gender)

# Find number of missiong values
forbes_sub |> summarise_all( \(x) sum(is.na(x)))

# Remove NA observations
forbes_sub2 <- forbes_sub |> na.omit()


# b) Use the data for prediction
rec <- recipe( finalWorth ~., data = forbes_sub2)  |> 
    step_other( country,  threshold = 10) |> 
    step_dummy( country)

# look at the data
var <- bake( rec |> prep( training = forbes_sub2), new_data = NULL) 

# Count new dummies
var[ str_detect(var, "countr")] |> length()

# Check in data
forbes_sub2 |> 
    group_by(country) |> 
    count(  ) |> 
    filter( n >= 10) |> 
    ungroup() |> 
    distinct() # 31


# c) Split the data:
# usefull for create a model with good out of sample properties 
split <- initial_split(forbes_sub2 , prop = 0.75, strata = finalWorth)

df_train <- training(split) 
df_test <- testing(split)

# Workflow for the model
wf <- workflow( )

# d) Train the model OLS
ols <- linear_reg( ) |> set_engine("lm")

# Fit the model
ols_fit <-  wf |> 
    add_model( ols) |> 
    fit( data = df_train )

# Selv made: Reduces the estimate by 300.
# Why: 
ols_fit |> tidy()  |> filter( term == "selfMadeTRUE")

# e) eXtreme Gradient Boosting 

# Need to adjust the recipe 
rec <- rec |> 
    step_dummy( all_nominal() ) |> 
    step_integer( selfMade)
# 
xgb <- boost_tree(
    mode = "regression",
    trees = 100,
    mtry = 5,
    min_n =  4) |> 
    set_engine("xgboost")

# 
xgb <- boost_tree( ) |> 
    set_mode("regression") |> 
    set_engine("xgboost")

wf <- workflow( ) |> 
    add_recipe( rec)

# 
xgb_fit <- wf |> 
    add_model( xgb) |> 
    fit( data = df_train ) 

my_metrics <- metric_set( rmse, rsq )

# Look at the models
list_pred <- map( 
    list(ols = ols_fit, xgb_fit = xgb_fit),
    \(x) predict(x, new_data = df_test) |> mutate( .true = df_test$finalWorth) 
    )

# Compare
map( list_pred, \(x) my_metrics(x,     truth = .true, estimate = .pred)) |> bind_rows( .id = "model")

## Tune the xgboost
 # Tune the model
vb_folds <- vfold_cv( df_train, strata = finalWorth)

# Alternativ tuning
xgb_grid <- grid_latin_hypercube(
  trees =  tree_depth(),
  min_n(),
  finalize(mtry(), vb_folds),
  size = 30
)


# Tuning
# For increase speed
doParallel::registerDoParallel()

# Set tuning paramter
# Model, set up for tuning
xgb <- boost_tree(
    mode = "regression",
    trees = tune(),
    mtry = tune(),
    min_n = tune()
    ) |> 
    set_engine("xgboost")


# Tune model
xgb_tuned <-  
    tune_grid(
        wf |> add_model( xgb),
        resamples = vb_folds,
        grid = xgb_grid
      )


# 
xgb_tuned

collect_metrics(xgb_tuned) |> 
    filter(.metric == "rmse") %>%
    select(mean, mtry, min_n) %>%
    pivot_longer(
      mtry:min_n,
      values_to = "value",
      names_to = "parameter"
       ) |> 
    ggplot( aes( value, mean, color = parameter) ) + 
    geom_point(size = 3) + 
    facet_wrap(~parameter, scales = "free_x") +
    theme_light( base_size = 14)


show_best(xgb_tuned, "rmse")

best_rmse <- select_best(xgb_tuned, "rmse")

# Select and run the best model
final_xgb <- finalize_workflow(
  wf |> add_model( xgb),
  best_rmse
)


final_xgb

final_xgb_fit <- final_xgb |> fit( data = df_train)

## Comparing the models
list_pred <- map( 
    list(ols = ols_fit, xgb_fit = xgb_fit, final_xgb =  final_xgb_fit),
    \(x) predict(x, new_data = df_test) |> mutate( .true = df_test$finalWorth) 
    )

## Comparing the models
list_pred <- map( 
    list(ols = ols_fit, xgb_fit = xgb_fit, final_xgb =  final_xgb_fit),
    \(x) predict(x, new_data = df_train) |> mutate( .true = df_train$finalWorth) 
    )


# Compare
map( list_pred, \(x) my_metrics(x,     truth = .true, estimate = .pred)) |> bind_rows( .id = "model")



