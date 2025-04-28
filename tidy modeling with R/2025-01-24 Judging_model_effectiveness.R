

# Chapter 9: Evaluating the model
# Comparing differet models


# Data and packages
library(tidyverse)
library(tidymodels)
tidymodels_prefer()
theme_set( theme_bw())

# data
ames <- modeldata::ames |> mutate(Sale_Price = log10(Sale_Price)) # 2930
ames_validation_split <- initial_split( ames, prop = .8, strata = Sale_Price) 
ames_train      <- training( ames_validation_split)     # 1758
ames_test       <- testing(ames_validation_split)       # 586
ames_validation <- validation(ames_validation_split)    # 586



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
df_pred__res <- predict( 
    lm_fit,
    new_data = ames_test |> select( -Sale_Price)
    ) |> 
    # Binding the true Y
    bind_cols(
        ames_test  |>  select(Sale_Price)
        )

# graph the predicted Price and the acutal Price
df_pred__res |> 
    ggplot(
        aes( y = .pred, x = Sale_Price)
    ) +
    geom_point() +
    geom_abline( lty = 2, color = "black", size = 1) +
    coord_obs_pred() 


# The Root mean square error
rmse( df_pred__res, truth = Sale_Price, estimate = .pred)

# RMSE manual
df_pred__res |> 
    mutate(
        rmse = (.pred-Sale_Price)^2
    ) |> 
    summarise(
        rmse = (sum(rmse)/n())^0.5
    )


# Several measures of metrics---------------------------

#To compute multiple metrics at once, we can create a metric set. Let’s add   R2 and the mean absolute error:
df_metric <- metric_set( rmse, rsq, mae)
df_metric( df_pred__res, truth = Sale_Price, estimate = .pred)


# 9.3 Binary classifications metrics

# predicting class 1 or class 2.

data(two_class_example)
# Convert the data to a tibble
df_two_class <- two_class_example |> as_tibble()

# Explaining the variables: True - predict 1 - predict 2 and the predicted otucome
df_two_class 


# Confusion matrix:
conf_mat(df_two_class , truth = truth, estimate = predicted)

# accuracy
accuracy( df_two_class, truth, predicted)

# Matthew correlations coeff
mcc( df_two_class, truth, predicted)

# F1 metric
f_meas( df_two_class, truth, predicted)

# The Matthews correlation coefficient and F1 score both summarize the confusion matrix, 
# but compared to mcc(), which measures the quality of both positive and negative examples, 
# the f_meas() metric emphasizes the positive class, i.e., the event of interest.

classification_metrics <- metric_set( accuracy, mcc, f_meas)

classification_metrics( df_two_class, truth = truth, estimate = predicted)

# Receiver operating characteristic (ROC) cursve computes the sensitivity and specificity over a continuum of different event thresholds

roc_curve( df_two_class, truth, Class1) |> autoplot()

# If the curve was close to the diagonal line, 
# then the model’s predictions would be no better than random guessing. 
# Since the curve is up in the top, left-hand corner, 
# we see that our model performs well at different thresholds.



