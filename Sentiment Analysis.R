# R packages

library(tidytext)
library(textdata)
library(tidyverse)

# Get text data
text_raw <- read_lines(
  file = "https://gutenberg.net.au/ebooks/m00011.txt",
  skip_empty_rows = TRUE,
  skip = 22,
  n_max = 9090
)
head(text_raw)

# craete chapter variable
text_df <- tibble(text = text_raw) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^_chapter [1-31]_", ignore_case = TRUE)))) %>%
  filter(!str_detect(text, regex("^_chapter [1-31]_", ignore_case = TRUE))) %>%
  rownames_to_column(var = "line") %>%
  mutate(line = as.integer(line)) %>%
  mutate(index = line %/% 50)
text_df   

# get sentiment lexicon
bing <- tidytext::get_sentiments("bing")     #words are positive negative
afinn <- tidytext::get_sentiments("afinn")   #score between minus5 plus5
nrc <- tidytext::get_sentiments("nrc")       #classify in different emotions

#tokenize and join lexicon 
text_df_bing <- text_df %>%
  unnest_tokens(word, text) %>%
  inner_join(bing, by = "word")

text_df_nrc <- text_df %>%
  unnest_tokens(word, text) %>%
  inner_join(nrc, by = "word") 

text_df_afinn <- text_df %>%
  unnest_tokens(word, text) %>%
  inner_join(afinn, by = "word")  

# General Stats

text_df_bing %>%
  count(sentiment, sort = TRUE)

text_df_nrc %>%
  count(sentiment, sort = TRUE)

text_df_afinn %>%
  summarise(overall_score = sum(value, na.rm = TRUE))


# Visualization using ggplot2

# top 5 words in every sentiment

text_df_nrc %>%
  count(word, sentiment, sort = TRUE) %>%
  filter(!word %in% c("words", "feeling")) %>%
  group_by(sentiment) %>%
  slice_max(n, n=5) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE)+
  facet_wrap(~sentiment, scales = "free_y") +
  theme_light() +
  labs(title = "Emotional Words")

# emotional trend on text

text_df_afinn_index_score <- text_df_afinn %>%
  group_by(index) %>%
  summarise(sentiment = sum(value)) %>%
  ungroup()

text_df_afinn_index_score %>%
  mutate(positive = sentiment > 0) %>%
  ggplot(aes(index, sentiment)) +
  geom_col(aes(fill = positive), show.legend = FALSE) +
  theme_minimal() +
  labs(title = "Sentiment Analysis of Novel")



           