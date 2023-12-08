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

# Fit XG Boost
xgb <- boost_tree(
    mode = "regression",
    trees = 50,
    mtry = 4 
    ) |> 
    set_engine("xgboost")


# For tuning the model

# Define the tuning grid
xgb_grid <- expand.grid(
  trees = c(50, 100, 150),
  mtry = c(2, 3, 4),
  min_n = 5
)


# Recip.
rec <- recipe(medv ~ ., data = train)

# Model, set up for tuning
xgb <- boost_tree(
    mode = "regression",
    trees = tune(),
    mtry = tune(),
    min_n = tune()
    ) |> 
    set_engine("xgboost")



# Set up the workflow
wf <- workflow() |> 
  add_recipe(rec) |> 
  add_model(xgb ) 

 # Tune the model
vb_folds <- vfold_cv( train, strata = medv)

# Alternativ tuning
xgb_grid <- grid_latin_hypercube(
  trees =  tree_depth(),
  min_n(),
  finalize(mtry(), vb_folds),
  size = 30
)




# For increase speed
doParallel::registerDoParallel()

# Tune model
xgb_tuned <-  
    tune_grid(
        wf,
        resamples = vb_folds,
        grid = xgb_grid
      )


xgb_tuned

# Look at the result, choose the best model

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
  wf,
  best_rmse
)

# The final model
final_xgb

# Fit the best model, and look at performance
pred <- fit(final_xgb, data = train) |> predict( new_data = test) |> mutate( .true = test$medv)

# Set metric
my_metrics <- metric_set(rmse, rsq, mae)

# extract metrics
my_metrics(
    pred,
    truth = .true,
    estimate = .pred
    )

# Alternative display
final_res <- last_fit(final_xgb, split)

collect_metrics(final_res)


# Importance of variable
library(vip)

fit(final_xgb, data = train) |>
    pull_workflow_fit( ) |>
    vip( geom = "point")  + 
    theme_light( base_size = 16) 





