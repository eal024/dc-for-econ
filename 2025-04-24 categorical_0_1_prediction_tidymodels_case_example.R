
# A predictive modeling case study. Based on tidymodels example: 
# https://www.tidymodels.org/start/case-study/

library(tidymodels)
library(vip)

# Booking data: https://linkinghub.elsevier.com/retrieve/pii/S2352340918315191
# the dataset is only those who does not cancel the booking. 
# Goal for prediction: which hotel stay include chidren and/or babies based on other characteristics

# data import 
hotels <- readr::read_csv("https://tidymodels.org/start/case-study/hotels.csv")  |> 
  mutate(across(where(is.character), as.factor))

dim(hotels) # Look at the data : Dimention
str(hotels) # Structure of the data

# Outcome variable is children. 
hotels |> select( children)
hotels  |> count(children) |> mutate( andel = n/sum(n)) # Children in only 8 per cent.

# Importance! The imbalance can wreak havoc on the analysis. Search for upsample/downsample for balanced reprentation

## Data splitting and resampling
# Reserve 25% to the test set
set.seed(123)
splits <- initial_split( hotels, strata = children)

hotel_train <- training(splits)
hotel_test  <- testing(splits) 

## Look at the distribution of children in training and test set
hotel_train |> count(children) |> mutate( andel = n/sum(n)) # close to the complete data
hotel_test |> count(children) |> mutate( andel = n/sum(n)) # close to the complete data

## The cross validation:
# Using a single resample: validation set

val_set <- validation_split( 
    hotel_train,       # Data
    strata = children, # Stratifing the sampling to create the resample = The same propotion of children as the compl. data
    prop = 0.8         # 80% to train, 20 % to validation
)

# Model 1: Penalized logistic regression
# y (children) = categorical, logistic regression is a good first model

model1_log <- logistic_reg( 
    penalty = tune(), # To find the best value for making predictions with the data
    mixture = 1  # Set to 1 make the model potentiall remove irrelvant predictors
    ) |> 
    set_engine("glmnet") # estim. the logistic reg. using a penalty so that non relevant var are driven towards zero

## Recipe
# Prepering data for the regression

# Creating categorical variable
holidays <- c(
    "AllSouls", 
    "AshWednesday", 
    "ChristmasEve", 
    "Easter", 
    "ChristmasDay", 
    "GoodFriday", 
    "NewYearsDay",
    "PalmSunday")

# This var. is used in three steps in rec. see bellow
hotel_train |> select(arrival_date)

log_recipe <- 
    recipe( children ~ . , data = hotel_train) |> 
    step_date( arrival_date)  |> # Transform date
    step_holiday(arrival_date, holidays = holidays)  |>  # Mark holidays
    step_rm(arrival_date) |> # remove date
    step_dummy( all_nominal_predictors()) |>
    step_zv( all_predictors() )  |>  # Remove indicator var that only count 1 unique value (all 1 or 0)
    step_normalize( all_predictors() )  # Senter all scale all numeric variables 


# The workflow
log_wf <- 
    workflow() |> 
    add_model( model1_log) |> 
    add_recipe( log_recipe)


# Tuning the model
# The model has 1 hyperparamter to tune
# set the grid for penalty:
log_reg_grid <- tibble( penalty = 10^seq(-4, -1, length.out = 30) )


# Train the model (tune)
# 30 models 
# save the validation set predictions, to diagnostic info about the model fit
log_model_res <- log_wf |> 
    tune_grid(
        val_set,
        grid = log_reg_grid,
        control = control_grid( save_pred = T),
        metrics = metric_set(roc_auc)
    )

## Look at the validation
log_roc_auc <- log_model_res |> collect_metrics()

# Viz
theme_set(theme_light())

fig_roc_auc <- ggplot(
    data = log_roc_auc,
    aes( x = penalty, y = mean)
    ) + 
    geom_point() +
    geom_line() +
    scale_x_log10( labels = scales::label_number())  # For better viz

fig_roc_auc

# Value of 0.5 means the model does no better than chance of predicting the class
# The figure illustrate that the model is better with lower penalty = more predicters
# The model seems to plateua at the smaller penalty values

top_models <- log_model_res |> 
    show_best( metric = "roc_auc") |> 
    arrange( penalty)


best <- log_model_res |> select_best( metric = "roc_auc")
# But we want to select a penalty value further along the x-axis.

my_best <- log_model_res |> 
    collect_metrics( ) |> 
    arrange( penalty) |> 
    slice( 12)


fig_roc_auc + 
   geom_vline( xintercept = c(best$penalty, my_best$penalty), color = "red", linetype = c(1, 2) )


# How does the model perform?
# The ROC-curve.
lr_auc <- log_model_res |> 
    collect_predictions( parameters = my_best) |> 
    roc_curve( children, .pred_children)  |>  # From df (names/var) collect_predict
    mutate(
        model = "logistic Regression"
    )

# Plotting the ROC-curve
# validation set as ROC curve
autoplot(lr_auc)

# A second model: Tree based ensamle

# Parallel comptuing
cores <- parallel::detectCores()

rf_model <- rand_forest( 
    mtry = tune(), # hyperparamter set number of predicator variable that each node in the decision tree see and can learn about. From 1 to total number of featyres
    min_n = tune(),# the minimum nr. of hyperparam
    trees = 1000 
   ) |> 
   set_engine( 
    "ranger",
    num.threads = cores
   ) |> 
   set_mode( "classification")


# Recipe and workflow
rf_recipe <- recipe( children ~., data = hotel_train) |> 
    step_date( arrival_date) |> 
    step_holiday(arrival_date) |> 
    step_rm(arrival_date)


# Adding it all up
rf_wf <- workflow() |> 
    add_model( rf_model) |> 
    add_recipe(rf_recipe)

rf_model

# What will be tuned:
extract_parameter_set_dials(rf_model)

# Space filling design to tune, 25 candidate models 
# This part takes time
set.seed(345)
rf_res <- rf_wf |> 
    tune_grid(
        val_set,
        grid = 10, # 25
        control = control_grid(save_pred = T),
        metrics = metric_set(roc_auc)
    )

# See the best 5
rf_res |> show_best( metric = "roc_auc")

# Select the best model
rf_best <- rf_res |> select_best( metric = "roc_auc")

# Collect the data for ploting the ROC
rf_res |> collect_predictions() # Only possible after tungin with control_grid( save_pred = T)


rf_auc <- rf_res |> 
   collect_predictions( parameters = rf_best) |> 
   roc_curve( children, .pred_children) |> 
   mutate( model = "Random forest")

# Comparing Logistic and Random forest models
# The Random forest model perform much better.
bind_rows(
    rf_auc, lr_auc
    )  |>  
    ggplot( 
      aes( x = 1-specificity, y = sensitivity, col = model)
      ) + 
      geom_path( lwd = 1.5, alpha  = 0.8) +
      geom_abline( lty = 3) +
      coord_equal( ) + 
      scale_color_viridis_d( 
        option = "plasma", 
        end = .6
        )





