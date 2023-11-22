

# packages and data
library(tidyverse)
library(tidymodels)

data( package = "mlbench", BostonHousing)

df <- as_tibble(BostonHousing) |> 
    mutate( chas = as.numeric(chas))


# Split
set.seed(123456)
split <- initial_split(df, prop = 0.8, strata = medv, breaks = 4)

train <- training(split)
test <- testing(split)

hist(train$medv)

df_folds <- vfold_cv(train, v = 10)


# Look at the data
df_folds$splits[[1]] |> analysis()

# Lasso
lasso <- linear_reg( mixture = 1, penalty = 0.1) |> set_engine("glmnet")

# Recipe
rec <- recipe( medv~ ., data = train)

# Lasso CV
lasso_cv <- fit_resamples( lasso, rec, df_folds)

# Measure performance
collect_metrics( lasso_cv ) # 4.5
collect_metrics( lasso_cv, summarize = F ) 


collect_metrics( lasso_cv, summarize = F ) |> 
    summarise( mean = mean(.estimate), .by = c(.config,.metric) )

# Tuning ------------------------------------------------------------------

# Define the regression to be tuned
# tune( ) is the parameter to tune
tune_spec <- linear_reg( penalty = tune() , mixture = 1) |> set_engine("glmnet")

# Workflow
wf_tune <- workflow() |> 
    add_recipe( rec )

# Run tuning
lasso_grid <- 
    tune_grid(
        wf_tune |> add_model( tune_spec),
    resamples = df_folds
)


lasso_grid |> collect_metrics() 

# Plot the result
lasso_grid |>
    collect_metrics() |> 
    ggplot(
        aes( penalty, mean, color = .metric)
    ) +
    geom_errorbar( 
        aes(
            ymin = mean - std_err,
            ymax = mean+ std_err
        )
    ) + 
    geom_line( size = 1.5) + 
    facet_wrap( ~.metric , scale = "free", nrow = 2) +
    scale_x_log10( ) + 
    theme( legend.position = "none")


# Pick he bes value
lasso_grid |> 
    unnest( .metrics) |> 
    select( id, .metric, .estimate, penalty, .config ) |> 
    filter( 
        .metric == "rmse"
    ) |> 
    filter(
        .estimate == min(.estimate)
        )

lasso_grid |> unnest(.metrics) |> filter( .config == "Preprocessor1_Model01")

lasso_grid |> select_best("rmse")

### Gird of lambda values to search through
lambda_grid <- grid_regular( penalty(), levels = 50)

lasso_grid <- tune_grid(
    wf_tune |> add_model( tune_spec ),
    resamples = df_folds,
    grid = lambda_grid
)

# The best value
lasso_grid |> select_best("rmse")


























