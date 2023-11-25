

# library
library(janeaustenr)
library(tidytext)
library(tidyverse)
library(gutenbergr)

# Jane Austen
austen <- austen_books() %>% 
  select(-book) %>% 
  mutate(author = "Jane Austen")

# Bronte
bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767)) %>%
  select(-gutenberg_id) %>% 
  mutate(author = "Brontë Sisters")

# H.Gwells
hgwells <- gutenberg_download(c(35, 36, 5230, 159)) %>% 
  select(-gutenberg_id) %>% 
  mutate(author = "H.G. Wells" )

# Function tidy book
tidy_book <- function(author) {
  author %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stop_words)
}

# Cleaning
books <- 
    bind_rows(tidy_book(austen),
          tidy_book(bronte),
          tidy_book(hgwells)
          ) %>% 
  mutate(word = str_extract(word, "[:alpha:]+")) %>% 
  count(author, word, sort = TRUE )


# Comparing words
comparison_df <- books %>%
  add_count(author, wt = n, name = "total_word") %>% 
  mutate(proportion = n / total_word) %>% 
  select(-total_word, -n) %>% 
  pivot_wider(names_from = author, values_from = proportion, 
              values_fill = list(proportion = 0)) %>%
  relocate( `Jane Austen`, .before = `Brontë Sisters` ) |>
  pivot_longer(3:4, names_to = "other", values_to = "proportion")

library(scales)

comparison_df %>% 
  filter(proportion > 1 / 1e5) %>% 
  ggplot(aes(proportion, `Jane Austen`)) +
  geom_abline(color = "gray40", lty = 2) +
  #geom_jitter(aes(color = abs(`Jane Austen` - proportion)),
  #            alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) + 
  scale_x_log10(labels = label_percent()) +
  scale_y_log10(labels = label_percent()) + 
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") + 
  facet_wrap(~ other) + 
  guides(color = FALSE)


