

# Exercise 2 – Predicting school performance
# In this exercise, we are going to use a sample of Portuguese secondary school pupils.

library(tidyverse)
library(tidymodels)

# 1) 
df_school <- vroom::vroom("data/student-mat.csv") |> 
    # Keep only some of the variable
    select( sex, age, Mjob, Fjob, traveltime, studytime, failures, absences, grade = G3) |> 
    mutate_if(is.character, as.factor )




# Create dummy for the factors

# 2) Splitting data

tidymodels_prefer()
set.seed(123)

df_school_split <- initial_split(df_school, prop = .8)



#  Make sure you construct two separate data frames for the two  purposes
train <- training(df_school_split)
test <- testing(df_school_split)

# Why do we partition the data? To test the model (with the test set)
#  Is this the place in the process to perform the split? Before we have seen the data

# 3) Factors to dummy variable

# Female is converted to 1, male to 0
df <- df_school |> 
    select( Mjob, Fjob) |> 
    mutate( original = Mjob,
            original2 = paste0(Mjob, "-", Fjob)
    ) 

df_rec <- recipe( ~., data = df)

ref_cell3 <- df_rec |> 
    step_dummy( Mjob, Fjob) |> 
    prep( training = df)

# Get a row for eavh factor level
dummies <- bake( ref_cell3, new_data = NULL, original2 , starts_with( c("Mjob", "Fjob") )) |>
    distinct() |> 
    mutate( across(.cols = matches("Mjob|Fjob") , .fns = function(x) as.integer(x)))

## Convert to dummies
train <- train |> 
    mutate( sex = ifelse(sex == "F", 1, 0) |> as.integer(),
            original2 = paste0(Mjob, "-", Fjob)
            ) |> 
    select( -c(Mjob, Fjob) ) |> 
    left_join(
        dummies,
        join_by(original2)
    ) |> 
    select(-original2 )
    
    
# 4) Train model

# a) OLS
lm_model <- linear_reg() |> set_engine("lm")

lm_model |> translate()

# OLS
ols <- lm_model |>
    fit_xy( y = train |> pull(grade),
            x = train |> select(-grade)
            )

# lm( data = train, formula = grade ~ .)
lambda <- .05
lasso_model <- linear_reg( mixture = 1, penalty = lambda)

# Lasso
lasso <- lasso_model |> 
    fit_xy( y = train |> pull(grade),
            x = train |> select(-grade)
    )


# Random Forrest
random_forrest <- rand_forest( trees = 10, min_n = 5) |>
    set_engine("ranger") |> 
    set_mode("regression")

# random_forrest |> translate()
randomforrest <-  random_forrest |> 
    fit_xy( y = train |> pull(grade),
            x = train |> select(-grade)
    )

#
xgboost <- boost_tree(
    mode = "unknown",
    engine = "xgboost",
    mtry = NULL,
    trees = NULL,
    min_n = NULL,
    tree_depth = NULL,
    learn_rate = NULL,
    loss_reduction = NULL,
    sample_size = NULL,
    stop_iter = NULL
)


# Predict
test <- test |> 
    mutate( sex = ifelse(sex == "F", 1, 0) |> as.integer(),
            original2 = paste0(Mjob, "-", Fjob)
    ) |> 
    select( -c(Mjob, Fjob) ) |> 
    left_join(
        dummies,
        join_by(original2)
    ) |> 
    select(-original2 )


predict(ols, new_data = test)

df_predict <- tibble( model = list(ols = ols,
                     lasso = lasso,
                     rf = randomforrest
                     )
        ) |> 
    mutate( predict = map(model, \(x) predict(x, new_data = test) |> 
                              mutate( index = 1:n(),
                                      grades = test$grade
                                      ) 
                          )
            ) 

df_predict$predict


df_pred2 <- df_predict |> 
    mutate( predict = map(predict, \(x) 
                          x |> 
                              mutate( rmse = ((.pred-grades)^2)^0.5 )
            )
        ) |> 
    mutate( names = names(df_pred2$model) ) 
                          
                          
df_pred2 |>
    mutate( sum_rmse = map(predict, \(x) x |> 
                                    summarise( rmse = sum(rmse))
                                          )
                           ) |> 
    select(names, sum_rmse) |> 
    unnest(sum_rmse)
    
## Plot the density of the residuals
df_pred2 |> 
    select( names, predict) |> 
    mutate( e2 = map(predict, \(x) x$rmse)) |> 
    select(names, e2) |> 
    unnest( e2) |> 
    ggplot( aes(e2)) +
    geom_histogram( ) +
    facet_wrap(~names)































