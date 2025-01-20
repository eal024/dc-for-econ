

# Chapter 4 tidy modeling with R: https://www.tmwr.org/ames
# From: De Cock, D. 2011. “Ames, Iowa: Alternative to the Boston Housing Data as an End of Semester Regression Project.” Journal of Statistics Education 19 (3).

library(tidymodels)
library(tidyverse)

# Goal of using the data: Predict the sale price of a hopuse based on other info

ames <- modeldata::ames

dim(ames) # obs and variables
str(ames) # names and structur

# The sale price y:
hist( ames$Sale_Price, main =  "Sale price",   xlab = "sale price")

# Data are right-skewed: more inexpensive houses than expensive.
ames |>
    ggplot( aes( x = Sale_Price )) +
    geom_histogram(color = "white")

summary(ames$Sale_Price) # mean 180 796

# When modeling: log-transformed
# - no negative modeled sales prive
# - errors in predicting expensive houses will not have an undue influence on the model
# - the log trans will stabilized the variance

hist(log(ames$Sale_Price), breaks = 30)

# geographical locations
# Neighborhood: named 
# + longithude and latitude data: with values



