

library(tidyverse)
library(tidymodels)

monks <- vroom::vroom("data/monks.csv")

split <- initial_split(monks)
monk_train <- training(split)
monk_test <- testing(split)


lasso_mod <- linear_reg( penalty = .1 ) |> 
    set_engine("glmnet")

rec <-recipe( class ~., data = monk_train) |> 
    step_num2factor( attr1, levels = unique(monks$attr1) |> as.character() ) |> 
    step_num2factor( attr2, levels = unique(monks$attr2) |> as.character() ) |>
    step_num2factor( attr3, levels = unique(monks$attr3) |> as.character() ) |>
    step_num2factor( attr4, levels = unique(monks$attr4) |> as.character() ) |>
    step_num2factor( attr5, levels = unique(monks$attr5) |> as.character() ) |>
    step_num2factor( attr6, levels = unique(monks$attr6) |> as.character() ) |> 
    step_dummy( starts_with("attr") 
    )

# Inspect the data
bake(rec |> prep(training = monk_train),
     new_data = NULL
     ) 


# WF
wf <- workflow() |> 
    add_recipe( rec)

fit <- wf |> 
    add_model( lasso_mod) |> 
    fit( monk_train)


#
pred <- predict(fit, test) |> 
    rename( 
        lasso_pred = 1
        ) |> 
    mutate( pred_pos = lasso_pred > .5,
            class = test$class,
            result = ifelse(pred_pos == class, 1, 0 ) 
            ) 

table(pred$pred_pos,pred$class)

