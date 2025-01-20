
library(tidyverse)
library(tidymodels)
tidymodels_prefer()

# data
ames <- modeldata::ames |> mutate(Sale_Price = log10(Sale_Price)) # 2930
ames_validation_split <- initial_split( ames, prop = .8, strata = Sale_Price) 
ames_train      <- training( ames_validation_split)     # 1758
ames_test       <- testing(ames_validation_split)       # 586
ames_validation <- validation(ames_validation_split)    # 586

# Computational differnet methods
# Example: Lineaer regression
linear_reg() |> set_engine("lm") # lm
linear_reg() |> set_engine("glmnet") #glment
linear_reg() |> set_engine("stan") # stan

# Translate for detals how the parsnip converts the user code to the packages syntax
linear_reg() |> set_engine("lm")  |> translate()
linear_reg( penalty = 1) |> set_engine("glmnet") |> translate()
linear_reg() |> set_engine("stan") |> translate()


# Prediting the sale price
lm_model <- linear_reg() |> set_engine("lm")

lm_form_fit <- lm_model  |> fit(Sale_Price ~ Longitude + Latitude, data = ames_train)
 
lm_model |> 
    fit_xy(
    x = ames_train  |>  select(Longitude, Latitude),
    y = ames_train  |>  pull(Sale_Price)
    )

# Use the tranalte to see the name-conection between org. pacakges and the parsnipe

# Get the result/use the result
lm_form_fit |> 
    extract_fit_engine() |> 
    summary()

# Extraction the result
tidy(lm_form_fit) # broom pacakges

## 6.3 Make prediction

