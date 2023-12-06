# Pakker og data
library(tidyverse)
library(tidymodels)
library(mlbench)
data(BostonHousing)

# Boston Housing data
df <- BostonHousing |> janitor::clean_names() |> as_tibble()


# Split the data
split <- initial_split(df, prop = .8, strata = medv)
train <- training(split)
test <- testing(split)

# Cross validation V-fold, and repeating
df_vfold_repeated <- vfold_cv( df, v = 10, repeats = 2)

# Set the model
lasso <- 
    linear_reg(
        penalty = tune(),
        mixture = 1
        ) |>
    set_engine("glmnet")

# Recipe
rec <- recipe( medv~., data = df)  |> 
    step_dummy( chas,  one_hot = T)

# Workflow
wf <- workflow() |> 
    add_recipe( rec)

# For tuning the model 
grid_p <- grid_regular( penalty() , levels = 50)

# Execute the tuning
lasso_grid <- tune_grid(
    wf |> add_model(lasso),
    resamples = df_vfold_repeated,
    grid = grid_p
)

# Collect metrics
lasso_metrics <- lasso_grid |> collect_metrics()

# A first look
plot <- lasso_metrics |> 
    select( penalty, .metric , mean) |>  
    ggplot( aes( 
        x = penalty,
        y = mean,
        color  = .metric ) ) +
    geom_line() +
    scale_x_log10() +
    theme(legend.position = "none") +
    facet_wrap(~ .metric,
    scales = "free",
    ncol= 1)

plot
## Get the est RMSE
lasso_grid  |> select_best("rmse")
pen <- lasso_grid  |> select_best("rmse") |> pull( penalty )
lasso_grid |> collect_metrics() |> filter( penalty == pen)


# Show were the best model is at the plot
plot + 
    geom_point( 
        data = tibble( 
            penalty = pen,
            mean = 4.8,
            .metric = "rmse"
            ),
        aes( x = penalty, y = mean),
        inherit.aes = F,
        size = 2
        )


lasso_grid %>%
  collect_metrics() %>%
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
