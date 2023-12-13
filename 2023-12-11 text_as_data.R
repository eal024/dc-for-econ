
# Packages and library-----------------------------------------------
library(tidyverse)
library(tidytext)

# 1) data and preperations
speech <- vroom::vroom("data/speech1516.csv")

# Preperations
df_long <- speech |> 
    filter( language == "nob") |>
    unnest_tokens( word, text )


dim(df_long) # Dimention
str( df_long) # Structur

# Count words
df_long |> group_by(word) |> count( sort = T)

# 2) Remove stop words

get_stopwords( language = "norwegian")

df_long1 <- df_long |> 
    anti_join( 
        get_stopwords( language = "norwegian")
        )

#  
df_long1 |> 
    group_by( word) |> 
    count(sort = T)


# 3) Stemming: Treat differnt grammatical forms of the same word as the same
library(SnowballC)

# Mutate the word using wordstem function
df_long2 <- df_long1 |> 
    mutate( word_stem = wordStem(word , language = "norwegian" )
    )

df_long2 |> 
    group_by(word_stem) |> 
    count( sort = T)

# 5) Word cloud
library(wordcloud)
library(tm)


word_to_cloud <- df_long2 |> 
    group_by(word_stem) |> 
    count( sort = T) |> 
    ungroup() |> 
    mutate( freq = n/sum(n))


wordcloud( word_to_cloud$word_stem  |> head(200), 
     word_to_cloud$freq  |> head(200),
     random.order = F,
     rot.per = .00,
     max.words = 100,
     min.freq = 0.1,
     
)

# 6) Comparing the occurrence of words between Høyre and AP

# a) Tibble only with words from AP- and H- representatives
unique(df_long2$party_name)
df_analysis <- df_long2 |> filter( party_name %in% c("Høyre","Arbeiderpartiet") )

# b) 
df_analysis_count <- df_analysis |> 
    group_by( party_name, word_stem ) |> 
    count( sort = T) |>
    group_by( party_name) |> 
    mutate( freq = n/sum(n))  |>  
    arrange( party_name, -freq) 

df_analysis_count_wider <- df_analysis_count |> 
    select(word = word_stem, party_name, freq) |> 
    pivot_wider( names_from = party_name, values_from = freq)


df_analysis_count_wider


# At least 10 times in total
word_total <- df_analysis |> 
    group_by(word_stem) |> 
    count( sort = T) |> 
    ungroup() |> 
    filter( n > 50)


# Word above 10
df_analysis_count_wider_filtered <- df_analysis_count_wider |> 
    filter( word %in% word_total$word_stem )


# 
df_analysis_count_wider_filtered |> 
    ggplot( 
        aes( Arbeiderpartiet, Høyre) 
    ) +
    geom_point() +
    geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
    geom_abline( color= "gray40", lty = 2
        ) +
    scale_x_log10(labels = label_percent()) +
    scale_y_log10(labels = label_percent())


# 7) Lexica for positive and negative words

# Construct a tibble with + and - words
dir("data")
neg <- read.delim("data/Fullform_Negative_lexicon.txt") 
pos <- read.delim("data/Fullform_Positive_lexicon.txt")

df_sentiment <- 
    tibble( word = c(
        neg$abnorm,
        pos$absolutt),
        sentiment = c(
            rep(x = -1, length(neg$abnorm)),
            rep(x = 1, length(pos$absolutt))
            )
        )

df_neg <- as_tibble(neg) |> rename( word = 1)
df_pos <- as_tibble(pos) |> rename( word = 1)

df_neg |> filter( word %in% df_pos$word)

df_analysis_count_sentiment <- df_analysis_count |>
    rename( word = word_stem) |> 
    left_join( df_sentiment , join_by(word) )

#
df_analysis_count_sentiment

df_analysis_sentiment <- df_analysis |> 
    select( id, rep_last_name, party_name, word) |> 
    left_join( df_sentiment, join_by(word)
    )

hoyre_sentiemnt <- df_analysis_sentiment |> 
    filter( party_name == "Høyre") |> 
    group_by(party_name,id, rep_last_name) |> 
    summarise( 
        tot_word = n(),
        mean_sentiemt = mean(sentiment, na.rm= T)
    ) |> 
    filter( tot_word > 10)

# 
hoyre_sentiemnt |> 
    arrange( desc(tot_word) ) |> 
    head(20)


# Machine learning model
