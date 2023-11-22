
# Packages
library(tidyverse)
library(tidymodels)


# data
data(BostonHousing, package = 'mlbench')

# as_tibble
df <- as_tibble(BostonHousing) |>
    mutate( chas = as.numeric(chas) 
            )


# split data
set.seed(12345)

# 80% training data
df_split <- initial_split( df, prop = .8)

# as tbl
df_train <- training(df_split)
df_test <- testing(df_split)


df_train

# models
ols <- linear_reg( ) |> set_engine("lm") |> fit( medv~. , data = df_train)
lasso <- linear_reg( mixture = 1, penalty = 0.034) |> set_engine("glmnet") |> fit( medv~. , data = df_train)


# Within sample prediction ------------------------------------------------

data <- list( ols = ols, lasso = lasso) |>
    map(\(x) predict(x, new_data = df_train) |> 
            mutate( medv = df_train$medv)
        )

rmse <- map( data, \(x) rmse( x, truth = medv, estimate = .pred) )

# lm_ols <- lm( data = df_train, medv ~.) 
# 
# glance(lm_ols)
# augment(lm_ols)
# 
# data[[1]] |> 
#     mutate( error2 = (.pred -medv)^2 ) |> 
#     summarise( rmse = (mean(error2))^0.5 )

my_metrics <- metric_set(rmse, rsq, mae)

my_metrics(data[[1]],
           truth = medv,
           estimate = .pred
           )

map(data,
    \(x) my_metrics(x,
                   truth = medv,
                   estimate = .pred)
    ) |> 
    bind_rows( .id = "model")


# Out of sample prediction ------------------------------------------------

out_of_sample_pred <- list( ols = ols, lasso = lasso) |>
    map(\(x) predict(x, new_data = df_test) |> 
            mutate( medv = df_test$medv)
    )


out_of_sample_pred

map(out_of_sample_pred,
    \(x) my_metrics(x,
                    truth = medv,
                    estimate = .pred)
    ) |> 
    bind_rows( .id = "model")










