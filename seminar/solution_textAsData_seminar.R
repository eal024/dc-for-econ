
library(tidyverse)
library(tidytext)
library(tidymodels)
library(stopwords)
library(SnowballC)

# Set up data

speech_dta <- read_csv("data/speech1516.csv")

speech <- speech_dta %>% 
  unnest_tokens(word,text)


speech <- speech %>% 
  anti_join(get_stopwords("no"))

speech <- speech %>%
  mutate(stem=wordStem(word, language = "norwegian"))

ApH <- speech %>%
  filter(party_name %in% c("Arbeiderpartiet","Høyre")) %>% 
  mutate(party = substr(party_name,1,1))


# a)
#install.packages("quanteda") may be needed

dtm <- ApH %>% 
  group_by(id,party,stem) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  cast_dtm(id, stem, count)


dfm <- ApH %>% 
  group_by(id,party,stem) %>% 
  summarise(n = n()) %>% 
  ungroup() |> 
  spread(stem, n, fill = 0)


  pivot_wider( names_from = stem, values_from = count, values_fill = 0) 
  cast_dfm(id, stem, count)


a <- ApH %>% 
  group_by(id,party,stem) %>% 
  summarise(count = n()) %>% 
  ungroup()


recipe( data = a) |> 
  step_dummy(  stem)