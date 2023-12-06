

#
library(tidyverse)
library(tidymodels)

monks <- vroom::vroom("data/monks.csv")

hist(monks$class)

# Classifiction problem

# 1) Split the data. Balanced on variable class
split <- initial_split(monks, prop = .8, strata = class)

train <- training(split)
test <- testing(split)

# 2) Train a simplelinear model


# Linear regression -------------------------------------------------------


lm_model <- linear_reg( ) |> set_engine("lm")

rec <- recipe( class ~., data = train)

# Workflow
lm_wf <- workflow( ) |> 
    add_model( lm_model) |> 
    add_recipe( rec )


lm_fit <- fit( lm_wf, data = train)

lm_pred <- predict(lm_fit, new_data = test) |>
    mutate( true_class = test$class)


# b) To obtain a single predicted value, give .pred < 0.5 = 0, else 1

lm_pred <- lm_pred |> 
    mutate( .pred_adj = ifelse(.pred < 0.5,0,1))

# c) Table
lm_pred1 <- lm_pred |> select( actual = true_class, predict = .pred_adj) 


conf_matrix <- caret::confusionMatrix( as.factor(lm_pred1$actual), as.factor(lm_pred1$predict) )


lm_pred1 |> 
    mutate_all( as.factor ) |> 
    table()

    
lm_pred1 |> 
    mutate_all( as.factor ) |> 
    group_by( actual, predict) |> 
    count() |> 
    pivot_wider( names_from = predict, values_from =n) |> 
    replace_na( list( `1`= 0))
    

# 3)  Extreme Gradient Boosting model -------------------------------------

split_factor <- initial_split( monks |> mutate( class = as.factor(class)),
                               prop = c(0.8),
                               strata = class
                               ) 

rec <- recipe( formula = class ~. , data = training(split_factor) 
                          ) 

xgboots_spec <- 
    boost_tree() |> 
    set_mode("classification")  |> 
    set_engine("xgboost")

#
xgboost_wf <- workflow() |> 
    add_recipe( rec) |> 
    add_model( xgboots_spec)


xgboost_train <- fit( xgboost_wf, data = training(split_factor) )

xgboost_pred <- predict(xgboost_train, new_data = testing(split_factor) ) |> 
    mutate( class = testing(split_factor)$class)


xgboost_pred |> 
    rename( predicted = .pred_class, actual = class) |> 
    mutate_all(as.factor) |> 
    table()
    


# 4. The Lasso model: Can the Lasso mdel do better tahn the lin --------

# Model
lasso_model <- 
    linear_reg( mixture = 1,
                penalty = tune()
                ) |>
    set_engine( "glmnet") 

# recipe
lasso_rec <-
    recipe( class~., data = train ) |> 
    step_num2factor( attr1, levels = unique(monks$attr1) |> as.character() ) |> 
    step_num2factor( attr2, levels = unique(monks$attr2) |> as.character() ) |>
    step_num2factor( attr3, levels = unique(monks$attr3) |> as.character() ) |>
    step_num2factor( attr4, levels = unique(monks$attr4) |> as.character() ) |>
    step_num2factor( attr5, levels = unique(monks$attr5) |> as.character() ) |>
    step_num2factor( attr6, levels = unique(monks$attr6) |> as.character() ) |> 
    step_dummy( starts_with("attr") 
                ) |> 
    prep()


# workflow 
wf_lasso <- workflow( ) |> 
    #add_formula( class ~. ) |> 
    add_recipe( lasso_rec ) |> 
    add_model( lasso_model)  
        

# Specify the tuning grid
grid <- tibble( penalty = 10^(seq(-2,-1, length.out = 10))
                )  

# Set up the resampling method
vfold <- vfold_cv( train, v = 10, strata = class)  # Replace with your strata column if applicable

# Perform the tuning
lasso_tune <- 
    tune_grid(
        wf_lasso,
        resamples = vfold,
        grid = grid,
        metrics = metric_set(rmse),  # Use the appropriate metric for your problem
        control = control_grid( verbose = F, save_pred = T)
    )

lasso_tune |>
    collect_metrics() |> 
    ggplot( aes(
        penalty, mean, color = .metric
                )
            ) +
    geom_point() +
    geom_line() +
    geom_errorbar( 
        aes(
            ymin = mean - std_err,
            ymax = mean+ std_err
        )
    ) +
    labs( y = "mean rmse")


# Set the optimal model
best_mod <- lasso_tune %>% select_best("rmse")

lasso_tune |> 
    select(.metrics) |> 
    unnest( .metrics) |> 
    group_by(penalty) |> 
    summarise( mean = mean(.estimate)
               ) |> 
    filter( mean == min(mean))



final_wf <- finalize_workflow(wf_lasso, best_mod)

print(final_wf)

final_fitted <- fit(final_wf, data = train) 

predict(final_fitted, train)







