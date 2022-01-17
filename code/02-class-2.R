## 1. Prepare packages

## 1.1 

# Download and install the following packages to get started:

setwd('~/Dropbox/cope-crabtree/text analysis course/2022/data/')
rm(list=ls())
require(tm)
require(matrixStats) # for statistics

## 1.2

# Today we're going to find distinctive words in the speeches of Obama.

### 1.3 Visualizing texts

# Before we look for distinctive words, we'll first take an in-depth look at a corpus, visualizing it in different ways. To this, we'll use the excellent package, 'quanteda'.
library(quanteda)
library(tidytext)
library(tidyr)
library(dplyr)
install.packages("dplyr")
data("data_corpus_inaugural")

inaug.td <- tidy(data_corpus_inaugural) # tidy text the corpus
inaug.td
summary(inaug.td)
head(inaug.td$text)

inaug.words <- inaug.td %>% # convert the corpus to document-word
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

inaug.words

inaug.freq <- inaug.words %>% # check how word use changes over time
  dplyr::count(Year, word) %>%
  complete(Year, word, fill = list(n = 0)) %>%
  group_by(Year) %>%
  mutate(year_total = sum(n),
         percent = n / year_total) %>%
  ungroup()

inaug.freq
tail(inaug.freq)

library(broom) # regress year on each word
models <- inaug.freq %>%
  group_by(word) %>%
  filter(sum(n) > 50) %>%
  do(tidy(glm(cbind(n, year_total - n) ~ Year, .,
              family = "binomial"))) %>%
  ungroup() %>%
  filter(term == "Year")

models

models %>% # organize regression output
  filter(term == "Year") %>%
  arrange(desc(abs(estimate)))

library(scales)
library(ggplot2)

models %>% # plot regression output
  top_n(6, abs(estimate)) %>%
  inner_join(inaug.freq) %>%
  ggplot(aes(Year, percent)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ word) +
  scale_y_continuous(labels = percent_format()) +
  ylab("Frequency of word in speech")

data.corpus.inaugural.subset <- # subset corpus to post-1949 speeches
  corpus_subset(data_corpus_inaugural, Year > 1949)
head(data.corpus.inaugural.subset)

token.info <- summary(data.corpus.inaugural.subset) # examine tokens
plot(token.info$Year, token.info$Tokens, type = 'l')

inaug.dfm <- dfm(data.corpus.inaugural.subset) # convert to dfm
textplot_wordcloud(inaug.dfm) # create word cloud

kwic(data.corpus.inaugural.subset, pattern = "american") %>% # create lexical dispersion plot
  textplot_xray()

textplot_xray( # create multiple lexicon dispersion plots
  kwic(data.corpus.inaugural.subset, pattern = "american"),
  kwic(data.corpus.inaugural.subset, pattern = "people"),
  kwic(data.corpus.inaugural.subset, pattern = "freedom")
)

## 2. Measuring "distinctiveness"

# Oftentimes scholars will want to compare different corpora by finding the words (or features) distinctive to each corpora. But finding distinctive words requires a decision about what “distinctive” means. As we will see, there are a variety of definitions that we might use. 

# Run the following code to:
# 1. Import the corpus
# 2. Create a DTM

# import corpus
docs <- Corpus(DirSource("sotu"))
docs
# preprocess and create DTM
dtm <- DocumentTermMatrix(docs,
                          control = list(tolower = TRUE,
                                         stopwords = TRUE,
                                         removeNumbers = TRUE,
                                         removePunctuation = TRUE,
                                         stemming = TRUE))
# print the dimensions of the DTM
dim(dtm)
# take a quick look
inspect(dtm[,100:104])

### 2.1 Unique usage

# The most obvious definition of distinctive is "exclusive". That is, distinctive words are those are found exclusively in texts associated with a single author (or group). For example, if Trump uses the word “bigly” and Obama never does, we should count “bigly” as distinctive. 

# Finding words that are exclusive to a group is a simple exercise. All we have to do is sum the usage of each word use across all texts for each author, and then look for cases where the sum is zero for one author.

# turn DTM into dataframe
dtm.m <- as.data.frame(as.matrix(dtm))
dtm.m$that <- NULL # fix weird stopword issue.
# Subset into 2 dtms for each author
obama.index <- grep("Obama", rownames(dtm.m))
obama <- dtm.m[obama.index, ]
dim(obama)
everyone.else <- dtm.m[-obama.index,]
dim(everyone.else)
# Sum word usage counts across all texts
obama <- colSums(obama)
head(obama)
everyone.else <- colSums(everyone.else)
head(everyone.else)
# Put those sums back into a dataframe
df <- data.frame(rbind(obama, everyone.else))
df[,1:5]
# Get words where one author's usage is 0
solelyobama <- unlist(df[1, everyone.else == 0]) 
solelyobama <- solelyobama[order(solelyobama, decreasing = T)] # order them by frequency
head(solelyobama, 10) # get top 10 words for obama
notobama <- unlist(df[2, obama==0])
notobama <- notobama[order(notobama, decreasing = T)] # order them by frequency
head(notobama, 10) # get top 10 words for trump

### 2.2 Removing unique words

# As we can see, these words tend not to be terribly interesting or informative. So we will remove them from our corpus in order to focus on identifying distinctive words that appear in texts associated with every author.

# subset df with non-zero entries
df <- df[, everyone.else > 0 & obama > 0]
# how many words are we left with?
ncol(df)
df[,1:5]

### 2.3 Differences in frequences

# Another basic approach to identifying distinctive words is to compare the frequencies at which authors use a word. If one author uses a word often across his or her oeuvre and another barely uses the word at all, the difference in their respective frequencies will be large. We can calculate this quantity the following way:
  
# take the differences in frequences
diffFreq <- obama - everyone.else
# sort the words
diffFreq <- sort(diffFreq, decreasing = T)
# the top obama words
head(diffFreq, 20)
# the top eveyoneelse words
tail(diffFreq, 20)

### 2.4 Differences in averages

# This is a good start. But what if one author uses more words *overall*? Instead of using raw frenquncies, a better approach would look at the average *rate* at which authors use various words. 

# We can calculate this quantity the following way:
  
# 1. Normalize the DTM from counts to proportions
# 2. Take the difference between one author's proportion of a word and another's proportion of the same word.
# 3. Find the words with the highest absolute difference.

# normalize into proportions
rowTotals <- rowSums(df) #create vector with row totals, i.e. total number of words per document
head(rowTotals) # notice that one author uses more words than the other
# change frequencies to proportions
df <- df/rowTotals #change frequencies to proportions
df[,1:5]
# get difference in proportions
means.obama <- df[1,]
means.everyoneelse <- df[2,]
score <- unlist(means.obama - means.everyoneelse)
# find words with highest difference
score <- sort(score, decreasing = T)
head(score,30) # top obama words
tail(score,30) # top words for everyone else

# This is a start. The problem with this measure is that it tends to highlight differences in very frequent words. For example, this method gives greater attention to a word that occurs 30 times per 1,000 words in Obama and 25 times per 1,000 in Trump than it does to a word that occurs 5 times per 1,000 words in Obama and 0.1 times per 1,000 words in Trump. This does not seem right. It seems important to recognize cases when one author uses a word frequently and another author barely uses it.

# As this initial attempt suggests, identifying distinctive words will be a balancing act. When comparing two groups of texts, differences in the rates of frequent words will tend to be large relative to differences in the rates of rarer words. Human language is variable; some words occur more frequently than others regardless of who is writing. We need to find a way of adjusting our definition of distinctive in light of this.

### 2.5 Difference in averages, adjustment

# One adjustment that is easy to make is to divide the difference in authors’ average rates by the average rate across all authors. Since dividing a quantity by a large number will make that quantity smaller, our new distinctiveness score will tend to be lower for words that occur frequently. While this is merely a heuristic, it does move us in the right direction.

# get the average rate of all words across all authors
means.all <- colMeans(df)
# now divide the difference in authors' rates by the average rate across all authors
score <- unlist((means.obama - means.everyoneelse) / means.all)
score <- sort(score, decreasing = T)
head(score,30) # top obama words
tail(score,30) # top words for everyone else

