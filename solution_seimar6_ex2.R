
library(tidyverse)

## Exercise 2, seminar 6

# 1) 

student <- vroom::vroom("data/student-mat.csv") |> 
    select( sex, age, Mjob, Fjob, traveltime, studytime, failures, absences, grade = G3) |> 
    mutate_if(is.character, as.factor)


# 2)
library(tidymodels)
set.seed(1234)

split <- initial_split( student, prop = 0.8)

train <- training(split)
test <- testing(split)

# Why partition the data
# to test out of sample prediction
# The ML alogrithm must never see out of sample data. Since we are testing the model on the test set
# If they were included before, the model will be construction predicted

# Now do the split?
# No should first construct/clean data

rec <- recipe(grade ~., data = train)

dummies <- rec |> step_dummy( c(sex, Mjob, Fjob))

dummie_data  <- bake( dummies |> prep(training = train), new_data = NULL ) 

#4.  Training data, using fit()

ols <- linear_reg( ) |> set_engine("lm")
lasso <- linear_reg( penalty = 0.1 ) |> set_engine( "glmnet")
rf <- rand_forest( ) |> set_mode( "regression") |> set_engine("ranger")
gbt <- boost_tree( ) |> set_mode("regression") |> set_engine("xgboost")

# Workflow, using dummies
wf <- workflow( ) |> 
    add_recipe( dummies )

?workflow

# OLS
model_ols <- wf |> 
    add_model( ols) |> 
    fit(train)



# Inspection of the coeff
wf |> 
    add_model(lasso) |> 
    fit(train) |> 
    extract_fit_parsnip( ) |> 
    tidy()

# Random forest
wf |> 
    add_model(rf) |> 
    fit(train)

# Predict
tbl <- tibble( name  = c("ols", "lasso", "rf"),
               model = list(ols,lasso,rf )
               ) |>
    mutate( 
        fit = map( model, \(x) wf |> add_model(x) |> fit( train) 
                   ),
        predict = map(fit, \(x) predict(x, test) |> 
                          mutate( y = test$grade)
                          )
    )
                   
# df by .pred and y
tbl1 <- tbl |> 
    select(name, predict) |> 
    unnest( predict)

# Compare MSE between the models
tbl1 |> 
    mutate( 
            error = (.pred -y),
            e2 =  error^2,
            abserr = abs(error)
            ) |> 
    group_by(name) |> 
    summarise( bias = mean(error),
               MSE = mean(e2),
               MAE = mean(abserr)
               )

# Plotting density error
tbl1 |> 
    mutate( error = (.pred -y )) |> 
    ggplot(
        aes( error)
    ) +
    geom_histogram( fill= "white",
                    aes(y = ..density..),
                    colour = 1,
                    alpha = 0.5) + 
    geom_density() +
    facet_wrap( ~name)












