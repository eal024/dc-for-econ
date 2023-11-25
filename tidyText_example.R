
# 
library(tidyverse)
library(tidytext)
library(hcandersenr)

# Data
data(stop_words)

# Prepering data
duckling <- hcandersen_en |> 
    filter( book == "The ugly duckling")

# Removing stop words
duckling |>
    select(book, text) |>
    unnest_tokens( 
        word,
        text
    ) |>
    anti_join( stop_words
    )



# Stemming 
library( SnowballC)
getStemLanguages()

duckling_tidy <- duckling |>
    unnest_tokens(word, text) |>
    anti_join( stop_words ) |>
    # Remove "ing", "s", "ed"
    mutate( stem = wordStem(word) 
    )

duckling_tidy |>
    left_join( get_sentiments("bing")
    ) |>
    group_by( book, sentiment) |>
    summarise( n = n()
    )







