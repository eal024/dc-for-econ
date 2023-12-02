
# data from: https://github.com/ltgoslo/talk-of-norway

library(tidyverse)
library(tidytext)

# data

Sys.setlocale("LC_CTYPE")
df <- vroom::vroom("data/speech1516.csv" ) |>
    filter( language == "nob")


# Tidy data
df_tidy  <- df |>
    unnest_tokens( word, text)


# a) Inspect structur
str(df_tidy)

# b) count words
df_tidy |>
    group_by( party_name) |>
    count( sort = T)


# 2
get_stopwords( language = "norwegian" ) |> str()

# b) Remove stop words in df
df_tidy1 <- df_tidy |>
    anti_join(
        get_stopwords( language = "norwegian" )
        )

# Alternative
# df_tidy |>  filter( ! word %in% get_stopwords( language = "norwegian" )$word )



stop_words <- stop_words_pos |>
    rename( word = 1) |>
    mutate( sign = "positive") |>
    bind_rows( 
        stop_words_neg |>
    rename( word  = 1)|>
    mutate( sign = "negative")
    )

# 

# 3) Stemming
library(SnowballC) 
getStemLanguages( )

df_tidy2 <- df_tidy1 |>
    mutate( stem = wordStem(word)
    )


df_tidy2 |>
    group_by(stem) |>
    count( sort = T) -> must_freq_used_word

# 4) Wordcloud 
library(wordcloud)
library(tm)

must_frq <- head(must_freq_used_word, 200)
wordcloud::wordcloud(must_frq$stem, must_frq$n )

wordcloud::wordcloud(must_frq$stem, must_frq$n,  rot.per =.5, colors = "red")


# 6) Comparing occurence of words in speeches
# Keeping only Høyre and AP
df_tidy2$party_name |> unique()

df_tidy3 <- df_tidy2 |>
    filter( party_name %in% c("Høyre", "Arbeiderpartiet") 
    )

# b) Frequenced used each word
df_tidy4 <- df_tidy3 |>
    group_by( party_name, word ) |>
    count( sort = T) |>
    ungroup( ) |>
    filter( n > 10) |>
    group_by( party_name ) |>
    mutate( andel = n/sum(n) ) |>
    arrange( party_name) 


df_tidy5 <- df_tidy4 |>
    select(-n) |>
    pivot_wider( 
        names_from = party_name, 
        values_from = andel
    )



# Scatter plot, word that appear most often
df_tidy5|>
    ggplot(  aes( Arbeiderpartiet, Høyre)
    ) + 
    geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
    geom_abline( color= "gray40", lty = 2
        ) +
    scale_x_log10(labels = label_percent()) +
    scale_y_log10(labels = label_percent()) + 
    scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +  
    guides(color = FALSE)


# 7 Lexica Sentiment analysis
positive <- read.delim("data/Fullform_Positive_lexicon.txt") |> as_tibble()
negative <- read.delim("data/Fullform_Negative_lexicon.txt") |> as_tibble()

df_sentiment <- tibble( 
    word = positive$absolutt,
    sentiment = "positive"
    ) |> 
    bind_rows(
        tibble(word = negative$abnorm,
        sentiment = "negative"
        )
    ) |>
    mutate( value = ifelse( sentiment == "positive", 1, -1)
    )

# c) Add sentiment
df_tidy6 <- df_tidy5 |>
    left_join( df_sentiment |> select(word, sentiment = value),
        join_by(word)
        ) |>
    relocate( sentiment,  .after = word)
    
# Keep only words that with sentiment
df_tidy7 <- df_tidy6 |>
    filter( ! is.na(sentiment) 
        ) |>
    mutate( Arbeiderpartiet = sentiment*Arbeiderpartiet,
            Høyre = sentiment*Høyre
            ) 

# Statistics
df_tidy7 |> 
    group_by(sentiment) |>
    summarise_at( vars(Arbeiderpartiet, Høyre), function(x) mean(x, na.rm = T)
    )

# 8 ML model to predict which political party the speecher comes from, based on text

# i) a tibble with the speech id, the speaker’s party, and the count of each word.
df_tidy8 <- df_tidy |>
    mutate( word = wordStem(word) ) |> 
    select(id, party = party_name, word, rep_gender, county) |>
    mutate( word = ifelse(word == " ", "0", word )
        ) |> 
    mutate( wcount = n(), .by = word 
        ) |>
    mutate( sp_len = n(), .by = id)  |>
    filter( wcount > 50, 
            !is.na(party)
    ) |>
    na.omit( ) |>
    select(-id)

# b)
library(tidymodels)

df_split <- initial_split(df_tidy8 |> mutate( party = as.factor(party)), strata = party)

train <- training(df_split)
test <- testing(df_split)

#
rec <- recipe(  party ~., data = train) |>
     step_dummy( c(word,rep_gender, county)
     )  #|> 
     #step_string2factor(all_nominal()
    #)

# bake( rec |> prep(training = head( test, 100) ), new_data = NULL ) 
gbt <- boost_tree(
    mode = "classification",  # For classification tasks
    trees = 7,              # Number of trees
    tree_depth = 3) |>
   #
   set_mode("classification") |>
    set_engine("xgboost")


wf <- workflow( ) |>
    add_recipe( rec ) |>
    add_model( gbt ) 

#
model_fit <- fit(wf, data = train |> sample_n(10^3)  )

predictions <- predict(model_fit, new_data = test |> slice(1:1000) )

predictions1 <- predictions |>
    mutate( true = test |> slice(1:1000)  |> select(party) |> pull(party) 
    ) |> 
    rename( true_labels  = 2)

# confusion matrix
caret::confusionMatrix(predictions1$.pred_class, predictions1$true_labels)







