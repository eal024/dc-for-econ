

library(tidyverse)
library(tidymodels)

# The monks of Corsendonk Priory

# data
monks <- vroom::vroom("data/monks.csv")

# 1) split the data
data_split <- initial_split(monks, prob = 0.8, strata = class)

train <- training(data_split)
test <- testing(data_split)

# Distribition
hist(monks$class)
hist(train$class)

ggplot(train , aes(x = class) ) + geom_histogram()

# 2) Train a simple linear model

# a)
rec <- recipe( class ~., data = train)  

# Linear regression
lm <- linear_reg( ) |>  set_engine("lm")

wf <- workflow( ) |>  add_recipe(rec) 

# Model
model_lm <- wf |> add_model( lm)  |> fit( data = train)

tidy(model_lm)
lm_pred <- predict(model_lm, new_data = test) |> 
    mutate( .pred = ifelse(.pred > 0.5, 1, 0),
            .truth= test$class
            ) 

# conf. matrix
table(lm_pred$.pred,lm_pred$.truth) 
caret::confusionMatrix( factor(lm_pred$.pred),factor(lm_pred$.truth) )


# 3) Gradient boosting model
# Classification: Transform the y to factor

# Split
split_two <-
    initial_split( 
        monks |> mutate( class = as.factor(class)),
        strata = class,
        prob = 0.8   
    )

# 
train_two <- training(split_two)
test_two <- testing(split_two)

xgb <- 
    boost_tree() |>
            set_engine("xgboost") |> 
            set_mode( "classification")

rec_two <- 
    recipe( class ~., data = train_two) 


wf2 <- workflow() |> 
    add_recipe( rec_two )  |> 
    add_model( xgb)

xgt_fit <- wf2 |> fit( data = train_two)

# The confusion matrix

df_predcit_xtg <- predict(xgt_fit , new_data = test_two) |> 
    mutate( .thrue_class = test_two$class) 

# 
caret::confusionMatrix(df_predcit_xtg$.pred_class,df_predcit_xtg$.thrue_class )

table( df_predcit_xtg$.pred_class,df_predcit_xtg$.thrue_class)

# 4) The Lasso model

# a)
rec <- recipe( class ~., data = train_two) |> 
    step_num2factor( attr1 , levels = c("1", "2", "3") ) |>
    step_num2factor( attr2 , levels = as.character(unique(train_two$attr2)) ) |> 
    step_num2factor( attr3 , levels = as.character(unique(train_two$attr3)) ) |> 
    step_num2factor( attr4 , levels = as.character(unique(train_two$attr4)) ) |> 
    step_num2factor( attr5 , levels = as.character(unique(train_two$attr5))  ) |> 
    step_num2factor( attr6 , levels = as.character(unique(train_two$attr6)) 
    ) |> 
    step_dummy( starts_with("attr") )
    
bake( rec |> prep() , new_data = NULL )

# b) Train the Lasso
lasso <- logistic_reg( mixture = 0.9, penalty = 0.1 ) |>
    set_engine("glmnet")  |> 
    set_mode('classification')

lasso_wf <- workflow( ) |> 
    add_recipe( rec) |> 
    add_model(lasso)  
    
    
lasso_fit <- lasso_wf |> 
    fit( data = train_two)


#lasso_predict <- 

lasso_pred <- predict(lasso_fit, new_data = test_two) |> 
    mutate( .thrut = test_two$class)


caret::confusionMatrix(lasso_pred$.pred_class, lasso_pred$.thrut)

# c) Cross validation ----------------------------------------

tidymodels_prefer()

inint_split <- initial_split(monks |> mutate(class = as.factor(class)),     prop = 0.8, strata = class )

train_factor <- training(inint_split)
test_factor <- testing(inint_split) 

# 10 Fold cross validation
train_vfold <- vfold_cv(train_factor, v = 10)

# model
lasso <- logistic_reg() |>
    set_engine("glmnet") |>
    set_mode("classification") |> 
    set_args( mixture = 1, penalty = tune() 
    ) 

# Recipe
rec

# workflow (recipe + model)
lasso_wf <- workflow( ) |> 
    add_recipe(rec) |> 
    add_model( lasso)


# Tune (with a variety of  values)
penalty_grid <-  grid_regular(
    penalty( range = c(-3, 1) ),
    levels = 30  # Log transforming
)

# 
tuning_output <- tune_grid(
    lasso_wf,
    resamples = train_vfold,
    metrics = metric_set(roc_auc),
    grid = penalty_grid
)

# Visulaize 
autoplot( tuning_output)  +theme_bw()


best_model_lasso <- select_by_one_std_err( tuning_output, metric = "roc_auc", desc(penalty))


# Fit final model
finalize_model(
    lasso_wf,
    best_model_lasso
)

lass_best_fit <-
    logistic_reg() |>
    set_engine("glmnet") |>
    set_mode("classification") |> 
    set_args( mixture = 1, penalty = 0.00489 
    ) 

ll_best_model <- 
    lasso_wf |> 
    update_model( lass_best_fit)


pred_lasso <- fit(ll_best_model, data = train_two) |> 
    predict( new_data = test_two) |> 
    mutate( .true = test_two$class)

table(pred_lasso$.pred_class, pred_lasso$.true)
table(lasso_pred$.pred_class,lasso_pred$.thrut)

caret::confusionMatrix(pred_lasso$.pred_class, pred_lasso$.true)
caret::confusionMatrix(lasso_pred$.pred_class,lasso_pred$.thrut)

#










