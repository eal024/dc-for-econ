

# chapter 10 Evaluate performance
# understand the performance of a model before using the test set

library(tidyverse)
library(tidymodels)

# resubstituted: measure performance on training data


# Prerequest Ames analysis -----------------------------------------------------------

data(ames)

# clean and transform
ames <- ames |> janitor::clean_names() |> mutate( sale_price = log10(sale_price))


split <- initial_split(ames, prop = 0.8, strata = sale_price)
train <- training(split)
test <- testing(split)

rec <-
    recipe( sale_price ~ neighborhood + gr_liv_area + year_built + bldg_type + 
                latitude + longitude,
            data = train
            ) |> 
    # Log transform data
    step_log( gr_liv_area, base = 10) |> 
    # pool infrequently occuring values to other
    step_other( neighborhood, threshold = 0.01 ) |> 
    step_dummy(all_nominal_predictors() ) |> 
    step_ns( latitude, longitude, deg_free = 20)

# Neighboorhood -
tibble( names = train$neighborhood |> table() |> names(),
           value = train$neighborhood |> table()
           ) |> 
    mutate( andel = value/sum(value)) |> 
    arrange( andel) |>
    head(20) 


#
lm_model<- linear_reg() |> set_engine("lm")

# Set a workflow
lm_wflow <- workflow( ) |>
    add_model(lm_model ) |> 
    add_recipe( rec )
 
# Fit
lm_fit <- fit( lm_wflow, data = train)

# predict( lm_fit, new_data = train)

# How compare performance between diff. models?  --------------------------------------

# 1) First fit the RF model

# mode
rf_model <- rand_forest(trees = 1000) |>
    set_engine("ranger") |>
    set_mode("regression")


# workflow
rf_wflow <- 
    # workflow
    workflow( ) |> 
    # add formula
    add_formula(
        sale_price ~ neighborhood + gr_liv_area + year_built + bldg_type + latitude + longitude
    ) |> 
    # add mode
    add_model( rf_model )

# Random forest
rf_fit <- fit( rf_wflow, data = train)

# Linear regression
lm_fit <- fit( lm_fit, data = train)

# How compare lm with RF
lm_predict <- predict(lm_fit, new_data = train)

# apparent metric: Check the model on the train set

reg_metrics <- metric_set( rmse, rsq)

# RMSE linear regression
my_metrics(
    data = predict(lm_fit, new_data = train) |> mutate( sale_price = train$sale_price),
    truth = sale_price,
    estimate = .pred
           )

# Random Forest prediction
rf_pred <- predict(rf_fit, new_data = train) |>
    mutate( sale_price = train$sale_price)


my_metrics(
    data = rf_pred,
    truth = sale_price,
    estimate = .pred
)

rf_pred |> 
    mutate( e2 = (.pred - sale_price)^2) |> 
    summarise( rmse = mean(e2)
               )

# Check the linaer regression
lm_predict |> 
    mutate( sale_price = train$sale_price) |> 
    mutate( e2 = (.pred - sale_price)^2) |> 
    summarise( rmse = mean(e2)
    )

my_metrics(
    data = lm_predict |> mutate( sale_price = train$sale_price) ,
    truth = sale_price,
    estimate = .pred
)


# -------------------------------------------------------------------------

reg_metrics <- metric_set(rmse, rsq)

estimate_perf <- function(model, data){
    
    model |> 
        predict( new_data = data) |> 
        bind_cols( data |> select( sale_price) 
        ) |> 
        reg_metrics( sale_price, .pred)     
} 


# RF is much more caple to predict
estimate_perf( lm_fit, train)
estimate_perf( rf_fit, train)


map(list(lm = lm_fit, rf = rf_fit), function(x) estimate_perf(model = x,
                                                    data = test
                                                    )
    ) |> 
    bind_rows( .id = "model")


## RF -> Low Bias model

## How select the best model? Use Validation


## Resampling methods

# Resampling only on training data

# Resample i: Data are partitioned into two subsaples
## 1) Analysis: model is fit
## 2) Assessment: Model is evaluated


# Cross-validation --------------------------------------------------------

# V-fold

# Randomly portioned int V sets equal size (folder)

# each iteration holds out one fold for assessment statistics


## Example V-fold cross validation
set.seed(1001)

split <- initial_split(ames)
train <- training(split)
test <- testing(split)

# V-fold
df_folds <- vfold_cv( train, v = 10)

train |> nrow() # 1977+220

# Manually retrive the data:
df_folds$splits[[1]] |> analysis() |> dim()
df_folds$splits[[1]] |> assessment() |> dim()

#df_folds$splits[[1]]$data |> str()

## THer are a variety of cross-validations variations. Below is some of the ost important ones


# Repeted CV --------------------------------------------------------------
# The most important CV: Repeated V-fold-validatiion

# Repeated vfold_cv:

vfold_cv( train, v = 10, repeats = 5)


# 10.2.2 Validation sets ---------------------------------------------------------

# no testing (60,20)% testing 20%
ames_val_split <- initial_validation_split( ames, prop = c(0.6 , 0.2))

# look at the validation set
validation_set( ames_val_split)




# Bootstrapping (BS) --------------------------------------------------------

# BS-sample (analysis): same size as the training data. Drawn with replacement.
    # Some training data points are selected multiple times
# assessment data: all of the observation that was not selected for the analysis set
 # May there vary in size 



bootstraps( train, times = 5) 

# + produce performance estimate with low variance
# - have significant pessimistic bias -- if true accurace to model is 90%, the BS-method gives a estimate that is lower




# 10.3 Performance --------------------------------------------------------

# Any of mention can be used to evalutate the modeling process
# Effective , because: Different data is used to train and assess the model

rf_wflow
lm_wflow

# fit_resample (same as fit(), but with resample)

# Example

# CV 10-fold
ames_folds <- vfold_cv( ames, v = 10, strata = sale_price)

# Random forest with 10 CV-V-fold
rf_res <- 
    rf_wflow |> 
        fit_resamples(resample = ames_folds,
                      control = control_resamples(save_pred = T,
                                                  save_workflow = T
                                                  )
                    )

# .metrics: list of the assessmet set performance statistics
# .notes: warnings or errro
# .predictions is present wehn save_pred = T
rf_res

collect_metrics(rf_res, summarize = F) |> 
    filter( .metric == "rmse")

# This RF-model, is model fit 10 times (with analysis data)
# The model is assessed 10 times and gives the average metrics of:
collect_metrics(rf_res)

predict( rf_fit, new_data = test) |>
    bind_cols( p = test$sale_price) |>
    mutate( e2 = (.pred-p)^2) |>
    summarise( er = mean(e2)^0.5 )

reg_metrics(
    data = predict(rf_fit, new_data = test) |> mutate( sale_price = test$sale_price),
    truth = sale_price,
    estimate = .pred
)


## Obtain assessemt set prediction

assess_res <- collect_predictions(rf_res)

## A plot help understand can help understand the model better:

assess_res |> 
    ggplot( aes( y = .pred, x = sale_price) ) +
    geom_point( alpha = 0.3 ) +
    geom_abline(color = "red")

## Look at outliers
over_prediced <- assess_res |> 
    mutate( residuals = abs(.pred -sale_price) 
            ) |> 
    arrange( desc(residuals)) |> 
    head(2)


# Inspection of example like this can help improve the model
train |> 
    slice( over_prediced$.row) |> 
    select( gr_liv_area, neighborhood, year_built, bedroom_abv_gr, full_bath)



## How can we use a validation set instead of cross validation?

initial_split <- initial_validation_split( ames, prop = c(0.6, .2) )

val_set <- validation_set(initial_split)

val_res <- rf_wflow |> 
    fit_resamples( resamples = val_set)

# Result (mean) closer to the test set
collect_metrics( val_res )
  
# each model could be fit simultaneously without issues
# Do parallell procesising
parallel::detectCores(logical = T)

library(doMC)
registerDoMC(cores = 2)

# Do the fit_resample
val_res <- rf_wflow |> 
    fit_resamples( resamples = val_set)


registerDoSEQ()





# 10.5 Saving the resampled object ----------------------------------------

ames_rec <- 
    recipe(sale_price ~ neighborhood + gr_liv_area + year_built + bldg_type + 
               latitude + longitude, data = train) %>%
    step_other(neighborhood, threshold = 0.01) %>% 
    step_dummy(all_nominal_predictors()) %>% 
    step_interact( ~ gr_liv_area:starts_with("bldg_type_") ) %>% 
    step_ns(latitude, longitude, deg_free = 20)

lm_wflow <-  
    workflow() %>% 
    add_recipe(ames_rec) %>% 
    add_model(linear_reg() %>% set_engine("lm")) 

lm_fit <- lm_wflow %>% fit(data = train)

lm_fit


#
extract_recipe(lm_fit, estimated = T)

get_model <- function(x) {
    extract_fit_parsnip(x) %>% tidy()
}

ctrl <- control_resamples(extract = get_model)

lm_res <- lm_wflow %>%  fit_resamples(resamples = ames_folds, control = ctrl)


lm_res$.extracts[[1]][[1]]

all_coef <- map_dfr(lm_res$.extracts, ~ .x[[1]][[1]])

all_coef |> 
    filter( term == "year_built") 



















































