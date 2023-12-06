

# III. Analysis of speeches (weight 20%)

# 1) What steps do you need to go through to 

library(tsibble)

# Set seed for reproducibility
set.seed(123)

# Generate quarterly time index for 10 years
time_index <- seq(as.Date("2013-01-01"), as.Date("2022-12-31"), by = "3 months")

# Create a tsibble object with dummy financial data
financial_data <- tsibble(
  Time = time_index,
  Asset1_Price = 50 + 2 * time_trend + rnorm(length(time_index), sd = 10),
  Asset2_Price = 50 + 2 * time_trend + rnorm(length(time_index), sd = 10),
  Asset3_Price = 50 + 2 * time_trend + rnorm(length(time_index), sd = 10)
)

library(forecast)
library(tidyverse)

financial_data |> 
    pivot_longer(-Time) |> 
    ggplot( 
        aes(x = Time, 
            y = value,
            color = factor(name)        
        )
    ) +
    geom_line()

#
read_speech_bank <- readr::read_file("data/speech_chatGPT.txt") 

library(tidytext)

lines <- read_speech_bank[[1]] |> strsplit("\\.")  

text_data <- tibble( text = lines )  |> 
    unnest( text) 

df_tidy <- text_data |> 
    unnest_tokens(word, text)


# Bag of words
words_count <-df_tidy |>
    anti_join( get_stopwords( ) )  |> 
    group_by(word)  |> count( sort =T) |> 
    head(20)


words_count |> ggplot(aes( x = word, y = n) )  + geom_col() + coord_flip()



