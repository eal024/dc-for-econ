
# Packages and library-----------------------------------------------
library(tidyverse)
library(tidytext)
library(SnowballC)

# 1) data of norwegian palement speeches.
speech <- vroom::vroom("data/speech1516.csv")

# Preper the data
df <- speech |> 
    filter( 
        language == "nob",
        !is.na(party_name),
        party_name %in% c("Arbeiderpartiet", "Høyre") # Keep only AP and Høyre
    ) |>
    select(id, party = party_name, text) |> 
    unnest_tokens( word, text ) |> 
    anti_join(
        get_stopwords( language = "norwegian") 
    )  |> 
    mutate( 
        stem = wordStem(word, language = "norwegian"),
        stem = iconv(stem, 'UTF-8',  "ASCII//TRANSLIT"),   # Transform norwegian letters 
        party = as.factor(party)  # Need the party (y) to be factor
    )  |> 
    rename( speech_id= id ) # Because id is also a word (for spread)

# Word: We only keep word above 10 
word_count <- df |> 
    group_by(stem) |> 
    count( sort = T) |> 
    filter( n >= 10)


# Prepere 
df_cast <- df |>
    select(-word) |>  # Only stem words
    # Use only words that is counted above 10
    filter( stem %in% word_count$stem)  |>  
    group_by( speech_id, party, stem) |> 
    count() |> 
    pivot_wider( names_from = stem, values_from = n, values_fill = 0) |>  # For creating dummies
    ungroup() |> 
    select(-speech_id) # Dont need this


# The LM model. classify the politican by party based on text 

# Splitting data
df_split <- initial_split( df_cast, prop = 0.8, strata = party)
df_train <- training(df_split) # Train
df_test <- testing(df_split) # Test

# recipe
rec <- recipe(party ~ ., data = df_train)

# Model
xgb <- boost_tree(
    mode = "classification",
    trees = 50, # Does not need to spesify this  (This could also be tuned)
    mtry = 4  # Does not need to spesify this
    )

# Workflow
xgb_model <- workflow() |> 
    add_recipe( rec) |> 
    add_model( xgb) |> 
    fit( data = df_train)


# Predict
predict <- xgb_model |> predict( new_data = df_test) |> mutate( .true = df_test$party)

table( predict$.pred_class, predict$.true)

# or
caret::confusionMatrix(predict$.pred_class, predict$.true)

# Accurary
predict |>
    mutate( 
        correct = ifelse(.pred_class   ==  .true, 1, 0 )
    ) |> 
    summarise( accuracy = sum(correct)/n())


# Use the vip-packages
vip::vip( xgb_model)

# Tuning a xgb classification model-----------------------------------

# Define the tuning grid
xgb_grid <- expand.grid(
  trees = c(50, 100, 150),
  mtry = c(2, 3, 4),
  min_n = 5
)

# Model, set up for tuning
xgb_tune <- boost_tree(
    mode = "classification",
    trees = tune(),
    mtry = tune(),
    min_n = tune()
    ) |> 
    set_engine("xgboost")

# Set up the workflow
wf_tune <- workflow() |> 
  add_recipe(rec) |> 
  add_model(xgb_tune ) 

 # Tune the model
vb_folds <- vfold_cv( df_train, strata = party)

# Alternativ tuning
xgb_grid <- grid_latin_hypercube(
  trees =  tree_depth(),
  min_n(),
  finalize(mtry(), vb_folds),
  size = 30
)

# For increase speed
doParallel::registerDoParallel()

# Tune model
xgb_tuned <-  
    tune_grid(
        wf_tune,
        resamples = vb_folds,
        grid = xgb_grid
      )


xgb_tuned

show_best(xgb_tuned, "roc_auc")

best_rmse <- select_best(xgb_tuned, "roc_auc")

# Select and run the best model
final_xgb <- finalize_workflow(
  wf_tune,
  best_rmse
)

# The final model
final_xgb

# Fit the best model, and look at performance
pred_tune <- fit(final_xgb, data = df_train) |>
    predict( new_data = df_test) |>
    mutate( .true = df_test$party)

# conf. matrix
table( pred_tune$.pred_class, pred_tune$.true)

table( predict$.pred_class, predict$.true)












