

# c) Explain what a random forest algorithm does and how it differs from the linear model

# A Random Forest (RF) algorithm is an ensemble learning method that builds a predictive model by combining the outputs 
# of multiple individual decision trees. Each tree is trained on a random subset of the training data through a process called bootstrap sampling, where random samples with replacement are drawn from the original dataset.
# This helps introduce diversity among the trees in the forest.

# Ensemble learning, in general, aims to improve the overall performance and robustness of a model by combining predictions
# from multiple models. Random Forest, as a specific type of ensemble learning, leverages the strength of decision trees and mitigates their weaknesses, such as overfitting.

# Nature of the Model:

# Random Forest: Constructs a non-linear model through the combination of decision trees. Each tree is trained independently and then aggregated to make a final prediction.
# Linear Model: Constructs a linear relationship between the input features and the output variable. The model assumes that the relationship between variables is linear. Ensemble Learning:

# Random Forest: Utilizes ensemble learning by building multiple trees, which operate independently and collectively contribute to the final prediction. This helps improve generalization and robustness.
# Linear Model: Represents a single model that assumes a linear relationship between input features and the target variable.
# Bootstrap Sampling:

# Random Forest: Employs bootstrap sampling to create diverse subsets of the training data for each tree. This introduces randomness and decorrelates the trees in the ensemble.

# Handling Non-Linearity:

# Random Forest: Well-suited for capturing non-linear relationships and interactions between features, making it versatile for various types of data.
# Linear Model: Assumes a linear relationship, which may not be suitable for complex, non-linear patterns in the data.

# Risk of Overfitting:
# Random Forest: Tends to be more robust against overfitting, especially when the number of trees in the forest is controlled.
# Linear Model: Prone to overfitting if the model complexity is not appropriately managed.

# Example

# Data
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

# Packages
library(tidyverse)
library(tidymodels)

# Outcome: Rating, X-, Cocoa_Percent, Review_Date, Bean_Origin

# Subsample selected variable
df <- chocolate |>  select( Rating, Cocoa_Percent, Review_Date, Bean_Origin)

# Split the data
split <- initial_split(df)
train <- training(split)
test <- testing(split)

# Create a RF-model
rf <- rand_forest( 
    mtry = tune(),  # Choose number of tree based on tuning
    min_n = tune(), # minimum number of data points in a node
    trees = 400
    )  |> 
    set_engine(
        "ranger",
         num.threads = parallel::detectCores() # Set threads based no number of cores (increase speed)
    )  |>
    set_mode("regression")

# recipe
rec <- recipe( Rating ~., data = train) |>       # 
    step_other( Bean_Origin,  threshold = 20) |> # Only want 20 countries
    step_dummy( Bean_Origin) # create a dummy


# Workflow
wf <- workflow( ) |> 
    add_recipe( rec ) 


## Tune the model (example is cv, 10-fold, with out replacement)
val_set <- vfold_cv( train, v = 10)

# Tune grid
#mtry_grid <- grid_regular( mtry( range = c(2,13)), levels = 10)

# If tuning mtry and min_n  
double_grid <- grid_regular(
    mtry( range = c(2,13)),
    min_n( range = c(2,8)),
    levels = 10
    )

# Tuning the model
rf_tune_grid <- tune_grid( 
    object   = wf |> add_model(rf),
    resample = val_set, #
    grid = double_grid   
)

#  Show the top 5 models
rf_tune_grid |> show_best( metric = "rmse")

# Keep the best model
rf_best <- rf_tune_grid |> select_best( metric = "rmse")


# Fitting the best model ------------------------------------------------

# The best model
best_rf_model <- 
    rand_forest( 
        mtry = 5, # Choose number of tree based on tuning
        min_n = 7, #  minimum number of data points in a node
        trees = 1000
        )  |> 
        set_engine(
            "ranger",
            num.threads = parallel::detectCores() 
        )  |> 
        set_mode("regression")


# Fit the model
rf_fit <- wf |> 
    update_model( best_rf_model) |> 
    fit( data = train)


# Random Forest
rf_predict <- rf_fit |> predict( new_data = test) |> mutate( .true = test$Rating)


# Model evaluation-------------------------------------------------------

# Set metrics
my_metrics <- metric_set( rmse, rsq, mae)

my_metrics( 
    rf_predict, 
    truth = .true,
    estimate = .pred
    )


# Manualy
rf_fit |>
    predict( new_data = test) |>
    mutate( 
        .true = test$Rating,
        rmse = (.pred - .true)^2
    )  |> 
    summarise( rmse = mean(rmse)^0.5 )


## Compare with a linear model

lm <- linear_reg( "regression") |> set_engine("lm")

lm_fit <- wf |> 
    #add_recipe( rec) |> 
    add_model( lm ) |>  
    fit( data = train)

fn_pred <- function(model) { 
    predict <- model  |> predict( new_data = test) |> mutate( .true = test$Rating)
    
    my_metrics( 
        predict, 
        truth = .true,
        estimate = .pred
    )

    }

# Comparing the models
list( rf = rf_fit, lm = lm_fit) |>
    map( \(x) fn_pred(x) ) |>
    bind_rows( .id = "model") 













