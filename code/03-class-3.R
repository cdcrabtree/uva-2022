###############
### Setup R ###
###############

### Clear terminal
cat("\014")

### Clear space
rm(list = ls())

### Load library packages
library(stringi) # For text manipulation
library(tm) # Framework for text mining
library(tidytext) # Sentiment dictionaries
library(quanteda)
library(tidyr)
library(dplyr)
data("data_corpus_inaugural")

setwd('~/Dropbox/cope-crabtree/text analysis course/2022/data/')

##########################
### Sentiment Analysis ###
##########################

summary(data_corpus_inaugural)

inaug.td <- tidy(data_corpus_inaugural) # tidy text the corpus

inaug.words <- inaug.td %>% # convert the corpus to document-word
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# Look at the dataframe
inaug.words

# Plot top 15 most frequently used words in each category
inaug.words %>%  
  inner_join(get_sentiments("nrc"), by = "word") %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  ungroup() %>%
  group_by(sentiment) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = NULL, x = NULL) +
  coord_flip()

# Now try this for 'afinn' and 'nrc'. Hint: check ?get_sentiments. 
# Are there differences across these dictionaries?

### Let's do a word cloud version of this
library(reshape2)
library(wordcloud)

inaug.words %>%
  anti_join(stop_words) %>%
  dplyr::count(word) %>%
  with(wordcloud(word, n, max.words = 100))

inaug.words %>%
  inner_join(get_sentiments("bing")) %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 10)

# Now let's look at specific sentiment
inaug.words %>%
  inner_join(get_sentiments("nrc"), by = "word") %>% 
  dplyr::count(word, sentiment, sort = TRUE) %>% 
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(pos_neg = ifelse(sentiment %in% c("positive", "anticipation", "joy", "trust", "surprise"), 
                          "Positive", "Negative")) %>%
  ggplot(aes(reorder(sentiment, n), n)) +
  geom_col(aes(fill = pos_neg), show.legend = FALSE) +
  scale_fill_manual(values = c("red2", "green3")) +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip()

# Now let's look at specific sentiment
inaug.words %>%
  inner_join(get_sentiments("nrc"), by = "word") %>% 
  dplyr::count(word, sentiment, Year, sort = TRUE) %>% 
  group_by(sentiment, Year) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(pos_neg = ifelse(sentiment %in% c("positive", "anticipation", "joy", "trust", "surprise"), 
                          "Positive", "Negative")) %>%
  ggplot(aes(reorder(sentiment, n), n)) +
  geom_col(aes(fill = pos_neg), show.legend = FALSE) +
  scale_fill_manual(values = c("red2", "green3")) +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip() +
  facet_wrap(~ Year)

# Now let's see what our results are if we change our tokens
inaug.words.five <- inaug.td %>%
  tidytext::unnest_tokens(five_gram, text, token = "ngrams", n = 5) %>%
  dplyr::count(five_gram, sort = TRUE) %>%
  top_n(30) %>%
  mutate(five_gram = reorder(five_gram, n)) %>%
  ggplot(aes(five_gram, n)) +
  geom_col(fill = "red", show.legend = FALSE) +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip()
inaug.words.five

# Now make this plot for bigrams. Hint: check ?unnest_tokens.

