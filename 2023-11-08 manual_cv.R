
#
library(mlbench)
library(data.table)
library(tidyverse)
data("BostonHousing")
set.seed(12345)


dt <- BostonHousing |> as_tibble() |> mutate(chas = as.numeric(chas))
dim(dt)

dt_cv <- dt |> 
    mutate( fold = sample(1:10, size = nrow(dt), replace = T) )

# 10 parts about equal size
dt_cv |> summarise( n = n(),.by  = fold ) |> mutate( andel = n/sum(n))

train <- dt_cv |> filter(fold != 1) 
test  <- dt_cv |> filter(fold == 1)

# The lasso steps:
# Fit
lasso_fit <- linear_reg(mixture = 1, penalty = 0.1) |> 
    set_engine("glmnet") |> 
    fit( medv~. , data = dt_cv |> filter(fold != 1) |> select(-fold)
         )

lasso_pred <- predict(lasso_fit, test |> select(-fold) )
mse <- sum( (lasso_pred - test$medv)^2)

## Look at the coef
lasso_fit |> tidy()

# 
lambda <- 0.05
mse <- 0
for( f in 1:10){
    
    train <- dt_cv |> filter( fold != f)
    test  <- dt_cv |> filter( fold == f)
    
    lasso_fit <- linear_reg( mixture = 1, penalty = lambda) |> 
        set_engine("glmnet") |> 
        fit( medv~, data = train)
    
}



