


# 
library(tidyverse)
library(tidytext)
library(wordcloud)
library(RColorBrewer)

wordcloud(
    "Many years ago the great British explorer George Mallory, who 
    was to die on Mount Everest, was asked why did he want to climb 
    it. He said, \"Because it is there.\"
    Well, space is there, and we're going to climb it, and the 
    moon and the planets are there, and new hopes for knowledge 
    and peace are there. And, therefore, as we set sail we ask 
    God's blessing on the most hazardous and dangerous and greatest 
    adventure on which man has ever embarked.",
        ,random.order=FALSE
        )


# streng
streng <- "Many years ago the great British explorer George Mallory, who 
    was to die on Mount Everest, was asked why did he want to climb 
    it. He said, \"Because it is there.\"
    Well, space is there, and we're going to climb it, and the 
    moon and the planets are there, and new hopes for knowledge 
    and peace are there. And, therefore, as we set sail we ask 
    God's blessing on the most hazardous and dangerous and greatest 
    adventure on which man has ever embarked."

# string spliting
string_div <- str_split(streng,  "\n")[[1]] |> str_trim( "both")

tbl <- tibble( i = 1:length(string_div), line = string_div ) |>
    unnest_tokens( word, line) |>
    anti_join(
        get_stopwords(  )
        )



wordcloud(tbl$word)