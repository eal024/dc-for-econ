# The basic R -- chapter 7, workflows
library(tidyverse)
library(tidymodels)
tidymodels_prefer()

# data
ames <- modeldata::ames |> mutate(Sale_Price = log10(Sale_Price)) # 2930
ames_validation_split <- initial_split( ames, prop = .8, strata = Sale_Price) 
ames_train      <- training( ames_validation_split)     # 1758
ames_test       <- testing(ames_validation_split)       # 586
ames_validation <- validation(ames_validation_split)    # 586


# Model selected: 
lm_model <- linear_reg() |> set_engine("lm")

# Workflow-packages: Example

# Adding model
lm_wf <- workflow() |> 
    add_model(
        lm_model
    )

# Adding forumal
lm_wf1 <- lm_wf |> add_formula( Sale_Price ~ Longitude + Latitude)

#
model_fit <- fit( lm_wf1, ames_train)

#
predict(model_fit, ames_test |> slice(1:5))

# If some changes:
model_fit |> update_formula( Sale_Price ~Longitude)

# 7.3 Adding Raw varibales to the fw()
lm_wf <- lm_wf |> 
    remove_formula() |> 
    add_variables(
        outcome = Sale_Price, predictors = c(Longitude, Latitude)
    )


## Case:7.5 Multiple workflows at once----------------------
library(workflowsets)

# May be many different models.

# For min. coding -- create workflowset
location <- list(
  longitude = Sale_Price ~ Longitude,
  latitude = Sale_Price ~ Latitude,
  coords = Sale_Price ~ Longitude + Latitude,
  neighborhood = Sale_Price ~ Neighborhood
)

# set WF
location_models <- workflow_set( preproc = location, models = list( lm = lm_model) )

location_models$info[[1]]$workflow

# Extract model
extract_workflow( location_models , id = "longitude_lm")
extract_workflow( location_models , id = "coords_lm")



# creating model fit for all models
fit( 
    object = location_models$info[[1]]$workflow[[1]], 
    data  = ames_train
    )

# Fit for all models
location_models1 <- location_models |> 
    mutate(
        fit = map( info, \(x) fit(x$workflow[[1]], ames_train))
    )


location_models1$fit[[1]]

# 7.6 Evaluation the test set---------------------------------

# last_fit: fit the model to the entire traingin set and evaluate it
final_lm_res <- last_fit( lm_wf, ames_validation_split)


# Collecting performance
collect_metrics( final_lm_res)

# Collecting predictions
collect_predictions(final_lm_res) |> head(5)
