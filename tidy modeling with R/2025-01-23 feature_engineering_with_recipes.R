
# 8. Feature Engineering
# Feature engineering entails reformatting predictor values to make them easier for a model

# Some models use geometric distance metrics and, consequently, numeric predictors should be centered and scaled so that they are all in the same units. Otherwise, the distance values would be biased by the scale of each column.

# Data and packages
library(tidyverse)
library(tidymodels)
tidymodels_prefer()

# data
ames <- modeldata::ames |> mutate(Sale_Price = log10(Sale_Price)) # 2930
ames_validation_split <- initial_split( ames, prop = .8, strata = Sale_Price) 
ames_train      <- training( ames_validation_split)     # 1758
ames_test       <- testing(ames_validation_split)       # 586
ames_validation <- validation(ames_validation_split)    # 586

# recipies packages: can be used to comnie different feature engineering and preprocessing tasks

# data and variables focused on:
# - neighborhood (qualitative, with 29 neighborhoods in the training set)
# - gross above-grade living area (continuous, named Gr_Liv_Area)
# - year built (Year_Built)
# - ype of building (Bldg_Type)

# Look at the variabels
df_desc <- ames |> select( Neighborhood, Gr_Liv_Area, Year_Built, Bldg_Type) 

df_desc |> skimr::skim() 
df_desc |> glimpse()

# consept:
lm(Sale_Price ~ Neighborhood + log10(Gr_Liv_Area) + Year_Built + Bldg_Type, data = ames) |> summary()

# Recipe
simple_ames <- 
    recipe(
        # outcomce ~ Predicters
        Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
        data = ames_train # data
    ) |> 
    # data transformations
    step_log( Gr_Liv_Area, base = 10) |> # Alternativ base = exp(1) 
    step_dummy( all_nominal_predictors()
    )


# factor/char-transformations: step_dummy

# 8.2 using recipes as part of the workflow------------------

lm_model <- linear_reg() |> set_engine("lm") 

# set the workflow
lm_wflow <- workflow() |>
    add_model( lm_model)  |> 
    # formula and data
    add_variables( 
        outcome = Sale_Price,
        predictors = c(Longitude, Latitude) 
    )


# using the recipe
lm_wflow |> add_recipe(simple_ames) # gives an error

# Must first remove variables
lm_wflow <- lm_wflow |>
    remove_variables() |> 
    add_recipe(simple_ames) 

 # fit the model y ¨~ .
lm_fit <- fit(lm_wflow, ames_train)

predict( lm_fit, ames_test |> slice(1:3))

# If we need the model object:
lm_fit |> extract_recipe( estimated = T)

# 
lm_fit |> extract_fit_parsnip() |> tidy() |> slice(1:5)

# 8.4: Example of recipe steps----------

# How data is encoding

# factors: reducting number of levels
simple_ames <- recipe( 
    Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
    data = ames_train
    ) |> 
    step_other( Neighborhood, threshold = 0.01) |>  # Collapsing factor levels
    step_dummy( all_nominal_predictors() ) |> 
    step_log( Gr_Liv_Area, base = 10) 


# look at the prep. data
prep_data <- prep( simple_ames, training = ames_train) |> bake( new_data = NULL) 

lm( data = prep_data, Sale_Price ~.) |> summary()

## 8.4.2 intercations terms

# f.example: buliding types
# See plot -- differnet slopes
# relevant to see at different
ames_train |>
    ggplot( aes( x = Gr_Liv_Area, y = 10^Sale_Price) ) +
    geom_point( alpha = .2) + 
    facet_wrap( ~Bldg_Type) + 
    geom_smooth(
        method = "lm", formula = y~x, se = F, color = "lightblue"
    ) +
    scale_x_log10() +
    scale_y_log10() +
    theme_bw()

# Interactions: x*z in base
recipe( 
    Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
    data = ames_train
    ) |> 
    step_other( Neighborhood, threshold = 0.01) |>  # Collapsing factor levels
    step_dummy( all_nominal_predictors() ) |> 
    step_log( Gr_Liv_Area, base = 10)  |> 
    step_interact( ~Gr_Liv_Area:starts_with("Bldg_Type_")) # Interactions


# If not Bldg_Type_ made to dummy:
# step_interact( ~Gr_Liv_Area:Bldg_Type)

ames_rec <- recipe( 
    Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
    data = ames_train
    ) |> 
    step_other( Neighborhood, threshold = 0.01) |>  # Collapsing factor levels
    step_dummy( all_nominal_predictors() ) |> 
    step_log( Gr_Liv_Area, base = 10)  |> 
    step_interact( ~Gr_Liv_Area:starts_with("Bldg_Type_"))  |> # Interactions
    step_ns( Latitude, deg_free = 20) # Creating spline


# Identifying the steps
tidy(ames_rec)

## Codeblock:
ames_rec <- recipe( 
    Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + Latitude + Longitude,
    data = ames_train
    ) |> 
    step_other( Neighborhood, threshold = 0.01) |>  # Collapsing factor levels
    step_dummy( all_nominal_predictors() ) |> 
    step_log( Gr_Liv_Area, base = 10)  |> 
    step_interact( ~Gr_Liv_Area:starts_with("Bldg_Type_"))  |> # Interactions
    step_ns( Latitude, deg_free = 20) 


lm_model <- linear_reg() |> set_engine("lm")

lm_wf <- workflow() |> 
    add_model( lm_model) |> 
    add_recipe( ames_rec)

# lm model
lm_fit <- fit(lm_wf, ames_train)

# Look at the predicted value
predict( lm_fit, new_data = ames_test |> slice(1:5)) |> bind_cols( ames_test |> select( Sale_Price) |> slice(1:5) )
