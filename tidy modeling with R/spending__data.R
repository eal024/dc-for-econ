
library(tidyverse)
library(tidymodels)
tidymodels_prefer()

## chapter 5: https://www.tmwr.org/splitting
# This chapter demonstrates the basics of splitting (i.e., creating a data budget) for our initial pool of samples for different purposes.
# The idea of data spending is an important first consideration when modeling, especially as it relates to empirical validation.

#  split the existing pool of data into two distinct sets, the training set and the test set. 

# training set as the substrate to develop the model.
# The test set is then used as the final arbiter to determine the efficacy of the model. 
# It is critical to look at the test set only once; otherwise, it becomes part of the modeling process.

# The rsample package has tools for making data splits:

# For reproducing the result
set.seed(501)

# data
ames <- modeldata::ames

ames_split <- initial_split( ames, prop = .8) 

# <Training/Testing/Total>
# <2344/586/2930>

ames_train <- training(ames_split)
ames_test <- testing(ames_split)

dim(ames_train)

# May cause a problem: class imbalance in classification problems, one class occurs much less frequently than another.
# For regression problems, the outcome data can be artificially binned into quartiles and then stratified sampling can be conducted four separate times. This is an effective method for keeping the distributions of the outcome similar between the training and test set.

hist(ames_train$Sale_Price)
hist(ames_test$Sale_Price)

# For even out the sale price between the test and traning data
ames_split <- initial_split( ames, prop = .8, strata = Sale_Price) 
ames_train <- training(ames_split)
ames_test <- testing(ames_split)

hist(ames_train$Sale_Price) # May be more eqv.
hist(ames_test$Sale_Price)

# Time data: Simple random cant be used. Use initla_time_split()
# prop argument denotes what proportion of the first part of the data should be used as the training set;

# Overfitting -- the model fits the traning data to well -- when tested it perform poorly
# The solution is to use validation set.

# The validation set was a means to get a rough sense of how well the model performed prior to the test set

# splitting functins

ames_validation_split <- initial_validation_split( ames, prop = c(0.6, 0.2))

# Splitting
ames                                                    # 2930
ames_train      <- training( ames_validation_split)     # 1758
ames_test       <- testing(ames_validation_split)       # 586
ames_validation <- validation(ames_validation_split)    # 586
# ames_validation_split T/V/Test/Total
# 1758/586/586/2930


