
library(tidyverse)
library(tidymodels)

# Comparing tidymodels and the glm for a logistic model

# Data from the ecls-project: Catholic student and non-catholic stundents.
ecls <- vroom::vroom("C:/Users/L158017/OneDrive - NAV/Documents/vscode_new/Econometrics/data/ecls.csv") |> 
    mutate(
        w3income_1k = w3income / 1000,
        catholic = as.integer(catholic)
    )


vars <- c("catholic", "race_white", "w3income_1k", "p5hmage", "p5numpla", "w3momed_hsb")

#
ecls2 <- ecls |>
    select( all_of(vars)  ) |> 
    na.omit()


# The glm-------------------------------------
model1 <- glm( 
    data = ecls2,
    formula = catholic ~ race_white + w3income_1k + p5hmage + p5numpla + w3momed_hsb,
    family = binomial(link = "logit")
)

summary(model1)

fitted( model1, data = ecls)  |> head()

# tidymodels----------------------------------

# recipe
rec <- recipe( catholic ~. , data = test_ecls ) |> 
    step_dummy(all_nominal_predictors())

# Look at the data
# rec |> prep() |> bake( new_data = NULL) |> View()

# model
model <- linear_reg() |> set_engine("glm", family = stats::binomial(link = "logit"))

wf <- workflow() |> 
    add_recipe(rec) |> 
    add_model( model)


# fit 
model2 <- wf |> fit(data = ecls)


# Gives the same result
summary(model1)
tidy(model2)




# Prediction tidymodels
predict( model2 , new_data = ecls2 |> slice(1:10)) |> add_column( truth = ecls2$catholic[1:10])

# Predictions glm
tibble( pred. = predict( model1, type = "response") )


# evaluating the model
df_classification <- tibble(
    truth = factor(pred$truth),
    .pred_value = pred$.pred
    ) |> 
    mutate(pred = factor(ifelse(.pred_value > 0.5, 1, 0), levels = c(0,1)))


View(df_classification)


# Beregner klassifikasjonsmetrikker
df_classification |> metrics(truth = truth, estimate = pred)

# ROC-kurve
df_classification |> 
  roc_curve(truth, .pred_value) |> 
  autoplot()