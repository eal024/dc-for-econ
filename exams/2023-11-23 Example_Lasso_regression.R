
#
library(tidyverse)
library(tidymodels)
library(mlbench)
data("BostonHousing")

# Want to predict median value owner occupied home
# medv: median value of owner-occupied homes in USD 1000's
df <- BostonHousing |>
    mutate( chas = as.numeric(chas) ) |> 
    as_tibble()



# OLS ---------------------------------------------------------------------

ols <- linear_reg( ) |> 
    set_engine("lm") |> 
    fit( medv~., data = df)

summary(ols$fit)
lm( data = df, medv~. ) |> summary()


#  Lasso -----------------------------------------------------------------

# mixture: Pure Lasso
lasso <- linear_reg( mixture = 1, penalty = .1) |> 
    set_engine("glmnet") |> 
    fit( medv~., data = df)


# Look at the coefficients
lasso |>
    tidy() |> 
    select(-penalty) |> 
    left_join(
        ols |> tidy() |> 
            select(term, estimate_0ls = estimate),
        join_by(
            term
        )
    )


## How the penalty affects the result.

sq <- seq(from = -4, to = 0, length.out = 30)

fn_lasso <- function(p){
    linear_reg( mixture = 1, penalty = 10^p) |> 
        set_engine("glmnet") |> 
        fit( medv~., data = df) |> 
        tidy()
}

ls_terms <- map(sq, \(x) fn_lasso(x) )

# Plotting the terms
ls_terms |> 
    bind_rows( ) |> 
    filter( term %in% c("crim", "zn", "age","b", "tax"  )
            ) |> 
    ggplot( aes(y = estimate, x = penalty, color = term) 
            ) +
    geom_line()


# In sample prediction
predict( lasso, df) |> 
    rename( lasso = .pred) |> 
    bind_cols( actual = df$medv,
               ols = predict(ols, df)$.pred
               ) |>
    pivot_longer( -actual) |> 
    ggplot( aes( x = actual, y = value) ) +
    geom_point() +
    geom_abline( color =  "red", linetype = 2) +
    facet_wrap(~name)



# tidymodels set up --------------------------------------------------------

# 1) Recipe
rec <- recipe( medv~., data = df)

# 2) model
lasso_mod <- linear_reg( mixture = 1, penalty = 0.1) |> 
    set_engine("glmnet")

# 3) workflow
wf <- workflow() |> 
    add_model( lasso_mod) |> 
    add_recipe( rec)

# 4) Fit the model
lasso_fit <- wf |> fit( data = df)

# Look at the model
lasso_fit |> extract_fit_parsnip( ) |> tidy()


# Tuning ------------------------------------------------------------------

# 10-fold cross validation.
# Repeated 2
df_fold <- vfold_cv( df, v = 10, repeats = 2)


# tune penalty
tune_spec <- linear_reg( penalty = tune(), mixture = 1 ) |> 
    set_engine( "glmnet")

# Set the grid of lambda
#tibble( penalty = seq(from = 10^-10, to = 1, length.out = 50))

lambda_grid <- grid_regular( penalty() , levels = 50)

# WF
wf_tune <- workflow() |> 
    add_recipe( rec)


# Run the differenet models
lasso_grid <- tune_grid(
    object = wf_tune |> add_model(tune_spec),
    resamples = df_fold,
    grid = lambda_grid
)

# Look at the models
g <- lasso_grid |>
    collect_metrics() |>
    ggplot(aes(penalty, mean, color = .metric)) +
    geom_errorbar(aes(
        ymin = mean - std_err,
        ymax = mean + std_err
    ),
    alpha = 0.5
    ) +
    geom_line(size = 1.5) +
    facet_wrap(~.metric, scales = "free", nrow = 2) +
    scale_x_log10() +
    theme(legend.position = "none")

g


# The best value
lasso_grid |> select_best( metric = "rmse")









