
# Case study, using tidymodels with classification

# complete examplpe of classification, using tidymodels.
# Based on this blogpost: https://www.tidymodels.org/start/case-study/

# A predictive modeling case study

# Data and pacakages
library(tidymodels)
library(vip)

# https://www.sciencedirect.com/science/article/pii/S2352340918315191?via%3Dihub
hotels <- vroom::vroom("https://tidymodels.org/start/case-study/hotels.csv")  |> 
    mutate(across(where(is.character), as.factor))

# Only not canceled stay in the data.
dim(hotels)

# Goal: to predict if the stay included children  
hotels |> 
    count(children) |> 
    mutate( prop = n/sum(n))

# outcome is a factor: Children 1 or 0

# 1) Data splitting a resampling
set.seed(123)

# outcome children is imbalanced, use stratified random sample.
# default 25% test
split <- initial_split( hotels, strata = children)

hotel_other <- training(split)
hote_test <- testing(split)

# Training has the same distrib. of children as the main data
hotels|> count(children) |> mutate( prop = n/sum(n))
hotel_other |> count(children) |> mutate( prop = n/sum(n))

# Validation split:---------------------------------------
set.seed(234)

val_set <- validation_split( 
    hotel_other,
    strata = children,
    prop = 0.8 # 20% validation, 80% training set
    )

val_set

# A first model: Logistic regression
lr_mod <- logistic_reg( 
        penalty =  tune(), # placeholder for now.
        mixture = 1) |>    # The model will be tuned to find the best value for making predictions with our data
    set_engine("glmnet")


# Create the recipe: Steps needed to prepare the data for the model.
holidays <- c("AllSouls", "AshWednesday", "ChristmasEve", "Easter", 
    "ChristmasDay", "GoodFriday", "NewYearsDay", "PalmSunday")


lr_recipe <- 
    recipe(children ~ ., data = hotel_other) |> 
        step_date(arrival_date) |>           # Create predictor for year, month, day
        step_holiday(
            arrival_date,
            holidays = holidays
            ) |>                              # Generate indicators for specific holidays
        step_rm(arrival_date) |>              # Remove original date variable
        step_dummy(all_nominal_predictors()) |>  # Convert char/factors into numeric binary terms
        step_zv(all_predictors()) |>          # Remove variables with zero variance
        step_normalize(all_predictors())      # Center and scale numeric variables

# look at the data
look <- lr_recipe |> prep() |>  bake( new_data =  slice(hotel_other, 1:10) )  

View(look)

# The workflow
lr_workflow <- 
    workflow() |> 
    add_model( lr_mod) |> 
    add_recipe( lr_recipe)

# Creating grid for tunig
# Example: one hyperparamter to tune: Look at the penalty param: https://glmnet.stanford.edu/articles/glmnet.html
lr_reg_grid <- tibble( penalty = 10^seq(-4, -1, length.out = 30))

# Look at the penelty param
# lr_reg_grid |> top_n(-5)
# lr_reg_grid |> top_n(5)

# Train and tune the model
# The area under the ROC curve will be used to quantify how well the model performs
lr_res <- 
    lr_workflow |> 
    tune::tune_grid( 
        val_set,
        grid = lr_reg_grid,
        control = tune::control_grid( save_pred = T),
        metric = metric_set( roc_auc)
    )

# visualize the validationl set metrics.
# Area under the ROC curve against the range of penalty values.

lr_res |> 
    collect_metrics() |> 
    filter( .metric == "roc_auc") |> 
    ggplot( 
        aes( x = penalty, y = mean)
    ) +
    geom_point() +
    geom_line() +
    scale_x_log10( labels = scales::label_number())

# This plots shows us that model performance is generally better at the smaller penalty values. 
# This suggests that the majority of the predictors are important to the model. 
# We also see a steep drop in the area under the ROC curve towards the highest penalty values. 
# This happens because a large enough penalty will remove all predictors from the model, 
# and not surprisingly predictive accuracy plummets with no predictors in the model 
# (recall that an ROC AUC value of 0.50 means that the model does no better than chance at predicting the correct class).

lr_res |> show_best( metric = "roc_auc", n  = 15)

# However, we may want to choose a penalty value further along the x-axis, closer to where we start to see the decline in model performance. For example, candidate model 12 with a penalty value of 0.00174 has effectively the same performance as the numerically best model, but might eliminate more predictors.

lr_best <- lr_res |> collect_metrics() |> arrange( penalty) |> slice(12)  

# 
lr_auc <- lr_res |>
    collect_predictions( parameters = lr_best) |> 
    roc_curve( children, .pred_children) |> 
    mutate( model = "logistic regression")

#
lr_auc |> autoplot()


## glm-model 2.------------------------------

# model
hotel_other_two <- hotel_other |> mutate( child = ifelse( children == "children", 1, 0))

model_glm <- linear_reg() |> set_engine("glm", family = stats::binomial(link = "logit"))


recipe_glm <-  recipe(child ~ ., data = hotel_other_two) |> 
    step_date(arrival_date) |>           
    step_holiday(arrival_date, holidays = holidays) |> 
    step_rm(arrival_date) |>              
    step_dummy(all_nominal_predictors()) |>  
   # step_mutate(children = as.numeric(children)) |>  # Convert children to numeric
    step_zv(all_predictors()) |>          
    step_normalize(all_predictors())  


wf <- workflow() |> 
    add_model( model_glm)  |> 
    add_recipe( recipe_glm) 
    
#
fit <- wf |> fit(  hotel_other_two) 



# No penelty for the logistic regression  
lr_glm_grid <- wf |> 
    tune_grid( 
        val_set,
        grid = 1,
        control = tune::control_grid( save_pred = T),
        metric = metric_set( roc_auc)
    )


# Collect metrics from tuning results
lr_glm_metrics <- lr_glm_grid |> collect_metrics()

# Print the metrics
lr_glm_metrics


lr_glm_grid |> collect_predictions()
