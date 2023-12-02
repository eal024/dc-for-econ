
# Load the packages
library(tidymodels)
library(tidyverse)

# Data: Width based on foot regime

urchins <-
  # Data were assembled for a tutorial 
  # at https://www.flutterbys.com.au/stats/tut/tut7.5a.html
  read_csv("https://tidymodels.org/start/models/urchins.csv") %>% 
  # Change the names to be a little more verbose
  setNames(c("food_regime", "initial_volume", "width")) %>% 
  # Factors are very helpful for modeling, so we convert one column
  mutate(food_regime = factor(food_regime, levels = c("Initial", "Low", "High")))


# Case: Urchins -- width from inital_volum
ggplot( 
    aes(x = initial_volume, y = width, color = factor(food_regime) ),
    data = urchins) + 
    geom_point( size =2) +
    geom_smooth( method = "lm", se = F
    )

# data splitting---------------------------------------------------------

# test/train, test/train -> analysis/assessments 

# 1) split the data into train and test set
split <- initial_split(urchins, prob = 0.8, strata = width)

train <- training(split)
test <- testing(split)

# 2) Validation: 

# Partition into traning and validation sets
set.seed(123)

# 10 cv-fold 
cv_fold <- vfold_cv( urchins, v = 10)

cv_fold$splits[[1]]
cv_fold$splits[[1]] |> analysis() # assessment() 

# Repeated CV
# R-repeated V-fold CV (V*R)
vfold_cv( urchins, v = 10, repeats = 5) # 10*5

# Monte Carlo CV
mc_cv( urchins, prop = 9/10, times = 20)

# Bootsrapping: sample of training -- sample with the same size as the traning set, trawn with replacement.
bootstraps( train, times = 5)

# BS: low variance (unlike cross-validation) but have significant pessimistic bias
# Used in Random forest model.

# Simple Model set ut -------------------------------------------------------

# 1) Recipe
rec <- recipe( data= train)


# linear model
lm <- linear_reg( ) |> set_engine("lm")

# Workflow
wf <- workflow() |> 
    add_recipe( rec) 
    add_model( lm ) |> 

# Linear model
model_lm <- wf |>
    add_model(lm) |>


## Fit the model
lm_fit <- model_lm |> fit( data = train)

# Predict
predict(lm_fit, new_data = test)

## CV-----------------------------

folds <- vfold_cv(urchins, v = 10)

# Workflow
wf <- workflow()

# Linear model
lm_cv_wf <- wf |> 
    add_model( lm) |> 
    add_formula( width~.) 

# Fit the data
lm_fit <- lm_cv_wf |> 
    fit_resamples(folds)

# Alternativ approach
# folds |> mutate( fit = map(splits, \(x)lm_cv_wf |> fit(data = x |> analysis())))

metric_set( rmse, rsq)
collect_metrics(lm_fit, summarize = F) |> 
    arrange( .metric)


# Evaulating model // using the model for prediction

