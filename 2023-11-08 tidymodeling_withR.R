

# https://www.tmwr.org/ames

library(tidyverse)
library(modeldata)
library(tidymodels)
theme_set(theme_classic())
data(ames)

# 
dim(ames)
head(ames)

qplot(x = Sale_Price/1000, data = ames, bins = 50 )
hist(ames$Sale_Price/1000, breaks = 50)

# From the data
#' right skewed
#' the y should be log-form

qplot(x = Sale_Price/1000, data = ames, bins = 50 )  + scale_x_log10()
hist(log(ames$Sale_Price / 1000),
     breaks = 50,
     main = "log sales price house, 1000")


## 5.1 common methods for splitting the data

# Training and test set.
## Allocation of test/train

# Spliting
ames1 <- ames |> mutate( Sale_Price = log(Sale_Price))
tidymodels_prefer()

#
set.seed(123)

# Save split info 80/20
ames_split <- initial_split(ames1, prop = .8)

ames_split # 2344+586 (Train+test = sum data (N))

# Get the object train
ames_train <- training(ames_split)
ames_test <- testing(ames_split)

dim(ames_train) #

hist(ames_train$Sale_Price, prob = T)
lines( density(ames_train$Sale_Price), lwd = 2, col = "red")

# To get a representativ set in the training set of sales price
ames_split <- initial_split( ames1, prop = 0.8, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test <- testing(ames_split)

ggplot( ames_train, aes( x = Sale_Price)) + geom_histogram(color = "white")

## Validation set
ames_val_split <- initial_validation_split(ames1, prop = c(0.6, 0.2))

training(ames_val_split)
testing(ames_val_split)
validation(ames_val_split)


# 5.3 Multilevel Data -----------------------------------------------------
# independent experimental unit: It is safe to assume that, statistically, the data from a property are independent of other properties

# The problem of information leakage occurs when data outside of the training set are used in the modeling process.



















