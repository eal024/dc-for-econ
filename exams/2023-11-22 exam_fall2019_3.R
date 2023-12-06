

#
library(tidyverse)
library(tidymodels)

# a) What is CV. How use it?

#' ML is about predicting outcomes, based on data.
#' You there fore try to create a model, which can predict outcome, based on data.
#' The relevant performance for the model is there fore "out of sample" prediction.
#' 
#' To test out of sample prop. you can divide data into training/testing part.
#' From the training data, you built a model, based on in sample properites.
#' When the model is selected, it is tested on the test data.
#' There is several alternativ methods within the conspet of CV.
#' One is N-fold-cross validation.
#' 
#' 
#' 
#' Example Lasso regression. 

  