

# Exercise Two: Ranking choclate 

# Packages
library(tidyverse)
library(tidymodels)

# a) Read the data

chocolate_raw <- readr::read_tsv("data/chocolate.tsv")

chocolate <- chocolate_raw |> 
    mutate( rank = case_when(
        between(Rating, 4, 5)     ~ "Outstanding",
        between(Rating, 3.5, 3.9) ~ "Highly Recommended",
        between(Rating, 3, 3.94) ~ "Recommended",
        between(Rating, 2, 2.9) ~ "Disappointing",
        between(Rating, 1, 1.9) ~ "Unpleasant",
        T ~ "No rank"
    ))

# chocolate |>  filter( rank == "No rank") |> count() # Check for error

# b) Extract variable
df <- chocolate |>  select( Rating, Cocoa_Percent, Review_Date)


# Inital split
split <- initial_split( df, prob = 0.8, strata = Rating)

# Traing and testing
train <- training(split)
test  <- testing(split)

# The linear (OLS) model
# The goal is to built a model to predict rating, based on the X (Cocoa_Percent, Review_Date)
# A approach is to built a predictive model, from the training set (80% of the data), and test the model on the test set (the rest 20% of the data)
# Model selection from data splitting (training/testing set), has the porpose to prevent overfitting,
# and increase/improve the out of sample prediction properties.

# When using the OLS-estimator, the algorithm is simple minimaizes the difference between predicted values (haty) with observed values ∑𝑖=1 𝑁 (𝑦𝑖 − 𝑦)^2

# The setup in tidymodels
# Train the linar model
lm <- linear_reg( ) |> set_engine("lm")

rec1 <- recipe( Rating ~ ., data = train)

model_lm <- workflow() |> 
    add_recipe( rec1 ) |> 
    add_model( lm)

# Fit the model
lm_fit <- fit(model_lm, data = train)

lm_pred <- predict( lm_fit, new_data = test) |> mutate( .thrue = test$Rating, mse= (.pred - .thrue)^2) 

# Out of sample properties
lm_pred |> summarise( rmse = mean(mse)^0.5 )

# c) Explain what a random forest algorithm does and how it differs from the linear model

# A RF builts a model based on multiple trees (ensamble learining -- combinding prediction from multiple individuals moddels) 
# Ensamble learning and RF train models based on boostrap samples.
# A problem with boostrap samplings (and for example with boostrap aggregation, Bagging) is that there is a high
# risk that the trees are resemble each other.

# Random forest in tidymodels
library(vip)

# Random Forest
# Tune spec
rf_mod <- rand_forest( 
    mtry = tune(), # Choose number of tree based on tuning
    min_n = tune(), #5, #tune(), #  minimum number of data points in a node
    trees = 400
    )  |> 
    set_engine(
        "ranger",
         num.threads = parallel::detectCores() # Set threads based no number of cores (increase speed)
    )  |> 
    #set_mode("classification")  
    set_mode("regression")


rec2 <- recipe( Rating ~., data = train) 

# Workflow
rf_wf <- workflow( ) |> 
    add_model( rf_mod)  |> 
    add_recipe( rec2)   

# Validation set
# val_set <- validation_split(df,
#    strata = Rating, 
#    prop = 0.80
#     )

# 10 cv_fold
val_set <- vfold_cv(train, v=10)

# Tune grid
mtry_grid <- grid_regular(mtry(range = c(2,13)), levels = 10)

# Tune both mtry and n_min
double_grid <- grid_regular(
    mtry(range = c(2,13)),
    min_n(range = c(2, 8)),
    levels = 10
    )



# # Tune grid
rf_res <-  
    tune_grid( 
        object = rf_wf,
        resamples = val_set,  # df_folds
        #grid = mtry_grid #, # 25 
        grid = double_grid # ,
        #control = control_grid( save_pred = T),
        # metrics = yardstick::metric_set(rmse)
        )

# The message printed above “Creating pre-processing data to finalize unknown parameter: mtry” is related to the size of the data set. Since mtry depends on the number of predictors in the data set, tune_grid() determines the upper bound for mtry once it receives the data.

# The top 5 random forest models, out of the 25 candidates:

rf_res |> show_best(metric = "rmse")

# Plot the result
autoplot(rf_res)

# Best model
rf_best <- rf_res %>% select_best(metric = "rmse")


# Plot the best model-source in graph
# rf_res %>% 
#  collect_metrics() %>% 
#  filter(.metric == 'rmse') %>% 
#  ggplot(aes(x=mtry, y=mean)) +
#    geom_line()



# Fitting the model
best_rf_model <- 
    rand_forest( 
        mtry = 4, # Choose number of tree based on tuning
        min_n = 8, #  minimum number of data points in a node
        trees = 400
        )  |> 
        set_engine(
            "ranger",
            num.threads = parallel::detectCores() # Set threads based no number of cores (increase speed)
        )  |> 
        #set_mode("classification")  
        set_mode("regression")

# Fit the model
last_rf_fit <- rf_wf |> 
    update_model( best_rf_model ) 


last_rf_fit |>     last_fit(  split) 


last_rf_fit |> collect_metrics()



# last_rf_fit |> extract_fit_parsnip() |> vip(num_features = 2)

# d) RF
predlm  <- predict( lm_fit, new_data = test) |> mutate( .thrue = test$Rating, mse= (.pred - .thrue)^2) 

# Out of sample properties
predlm |> summarise( rmse = mean(mse)^0.5 )

predrf <- predict( last_rf_fit |> fit( data = train), new_data = test )  |> mutate( .thrue = test$Rating, mse= (.pred - .thrue)^2) 

# Out of sample properties
predrf |> summarise( rmse = mean(mse)^0.5 )

# e) Explain why splitting the initial sample into a training and a testing sample is important to
# evaluate the model in a good way
# Overfitting may be a problem. The out-of-sample properties is the importan part you want to estimate


# f) intial data transformation

# You can use the step_dummy
df <- chocolate |>  select( Rating, Cocoa_Percent, Review_Date, Bean_Origin)


# Inital split
split <- initial_split( df, prob = 0.8, strata = Rating)
train <- training(split)
test <- testing(split)

rec3 <- recipe( Rating ~., data = train) |> 
    step_other( Bean_Origin,  threshold = 20) |> 
    step_dummy( Bean_Origin)

bake( rec3 |> prep( training = train), new_data = NULL) 

lm <- linear_reg( ) |> set_mode("regression")

wf <- workflow( ) |> 
    add_recipe( rec3) 


lm_model <- wf |> 
    add_model(lm) |> 
    fit( data = train)

lm_model |> tidy() |> filter( str_detect(term, "Bean_")) 

lm_model_pred <- lm_model |>
    predict( new_data = test) |> 
    mutate( .thrue = test$Rating, mse= (.pred - .thrue)^2)  |> 
    summarise( rmse = mean(mse, na.rm = T)^0.5 )

# Random Forest

rf_mod <- rand_forest( 
    mtry = tune(), # Choose number of tree based on tuning
    min_n = tune(), #5, #tune(), #  minimum number of data points in a node
    trees = 400
    )  |> 
    set_engine(
        "ranger",
         num.threads = parallel::detectCores() # Set threads based no number of cores (increase speed)
    )  |>
    set_mode("regression")


# Workflow
rf_wf <- workflow( ) |> 
    add_recipe( rec3)   


# 10 cv_fold
val_set <- vfold_cv(train, v=10)
# Tune grid
mtry_grid <- grid_regular(mtry(range = c(2,13)), levels = 10)

# Tune both mtry and n_min
double_grid <- grid_regular(
    mtry(range = c(2,13)),
    min_n(range = c(2, 8)),
    levels = 10
    )

# # Tune grid
rf_res <-  
    tune_grid( 
        object = rf_wf |>  add_model( rf_mod),
        resamples = val_set,  # df_folds
        grid = double_grid # ,
        )
# The top 5 random forest models, out of the 25 candidates:

rf_res |> show_best(metric = "rmse")

# Best model
rf_best <- rf_res %>% select_best(metric = "rmse")

# Fitting the model
best_rf_model <- 
    rand_forest( 
        mtry = 4, # Choose number of tree based on tuning
        min_n = 8, #  minimum number of data points in a node
        trees = 1000
        )  |> 
        set_engine(
            "ranger",
            num.threads = parallel::detectCores() 
        )  |> 
        set_mode("regression")



fit_rf  <- rf_wf |>
    update_model( best_rf_model) |> 
    fit( data = train)


# Random Forest
rf_model_pred <- fit_rf |>
    predict( new_data = test) |> 
    mutate( .thrue = test$Rating, mse= (.pred - .thrue)^2)  |> 
    summarise( rmse = mean(mse, na.rm = T)^0.5 )

rf_model_pred

# Result for the lm
lm_model_pred


# Alternative compare models
my_metrics <- metric_set(rmse, rsq, mae)

rf_pred <- fit_rf |> predict( new_data = test)  |> mutate( .true = test$Rating)
lm_pred <- lm_model |> predict( new_data = test)|> mutate( .true = test$Rating)

#
my_metrics(lm_pred, 
           truth = .true,
           estimate = .pred)

my_metrics(lm_model_pred, 
           truth = medv,
           estimate = .pred
           )


model_list <- list( fit_rf, lm_model)

#
map(model_list, \(df) df |>
    predict( new_data = test)  |>
    mutate( .true = test$Rating) |> 
    my_metrics( 
        truth = .true,
        estimate = .pred
        )
    ) |> 
    set_names(c("RF", "LM"))  |> 
    bind_rows( .id = "model")
