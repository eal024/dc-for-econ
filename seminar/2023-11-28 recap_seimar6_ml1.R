
# Excersice 2 Prediction school performance

# Packages and data
library(tidyverse)
library(tidymodels)

df_stundent <- vroom::vroom("data/student-mat.csv") |> 
    select( sex, age, Mjob, Fjob, traveltime, studytime, failures, absences, grade = G3) |> 
    mutate_if( is.character, as.factor) 

# 1)  ----------------------------------------------------------------

split <- initial_split(df_stundent,  prop =0.8, strata = grade)

train <- training(split)    
test <- testing(split)

# Why do we split the data ?




# 3) ----------------------------------------------------------------

rec <- recipe( grade ~. ,data = train)  |> 
     step_dummy( c(sex, Mjob, Fjob)) 
     

# Look at the data
bake( rec |> prep( training = train),  new_data = NULL )

# 4) Train the model

lm      <- linear_reg( )  |> set_engine( "lm")
lasso   <- linear_reg(penalty = 0.1, mixture = 0.95)  |>  set_engine("glmnet")
rf      <- rand_forest( mtry = 4, trees = 10)  |> set_engine( "ranger")  |> set_mode( "regression")
xgboost <- boost_tree( ) |> set_engine("xgboost") |> set_mode("regression") 

# Workflow
wf <- workflow() |>  add_recipe( rec)


model_lm    <-  wf |> add_model(lm) |> fit( data = train) 
model_lasso <- wf |> add_model(lasso) |> fit( data = train)
model_rf    <- wf |> add_model(rf) |> fit(data = train)
model_xgb   <- wf |> add_model(xgboost) |> fit(data = train)

model_list <- list(lm, lasso, rf, xgboost )
model_list <- set_names( model_list, map_chr(model_list, \(x) x$engine))


# Fit all of the models
model_fit_list <- 
    model_list |> 
    map( \(x) wf |> add_model(x) |> fit(data = train) )


# 5 prediction  of grades
predict <- map(model_fit_list, \(x) predict( x, new_data = test) |> 
    mutate( .true = test$grade,
            rmse = (.pred - .true)^2
        ) 
     )


# Set the RMSE in a table 
predict[[1]]

rmse(  predict[[1]], truth = .true, estimate = .pred)

tbl_rmse <- predict |>
  map( \(x) x |> summarise( rmse = mean( rmse)^0.5 ) ) |> 
    bind_rows( .id = "var")

tbl_rmse
## Alternativ
map(predict, \(x) rmse( x, truth = .true, estimate = .pred)) |> bind_rows()

## More
my_metrics <- metric_set(rmse, rsq, mae)

map( predict, \(x) my_metrics(x, truth = .true, estimate = .pred ) )  |>
    bind_rows(.id = "model") |>
    arrange( .metric)




 From tidymodels
predict[[1]]  |> collect_metrics()


# Look at the variable
# tidy(lm)
# lm( data = train, formula = grade ~ .) |> summary()

# Performance testing packages -------------------------------------

library(performance)
library(see)

ext_fit_engine <- lm |> extract_fit_engine()

ext_fit_engine |> check_model()


# 4