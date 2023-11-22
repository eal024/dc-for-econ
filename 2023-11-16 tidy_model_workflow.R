
# https://www.tmwr.org/workflows
# Workflow: encourage good methodolgy and +organize project

library(tidyverse)
library(tidymodels)

tidymodels_prefer( )

# Data 
ames <- janitor::clean_names(ames)

split <- initial_split(ames)

ames_train <- training(split)
ames_test <- testing(split)


# Model
lm_model <- linear_reg() |> set_engine("lm")

# Include model object: Parsnip model object
lm_wf <- workflow( ) |> add_model( lm_model )

# Preprocessor: None
lm_wf <- lm_wf |> add_formula( sale_price ~ longitude + latitude)

# Create the model
lm_fit <- fit( lm_wf, ames_train)

lm_predict <- predict( lm_fit, new_data = ames_test)

lm_fit <- lm_fit |> update_formula( sale_price ~ longitude)

fit(lm_wf, ames_train)
