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
library(dplyr) # Data preparation and pipes $>$
library(ggplot2) # for plotting word frequencies
library(wordcloud) # wordclouds!

###############################
### Import and Inspect Data ###
###############################

setwd('~/Dropbox/cope-crabtree/text analysis course/2022/data/')

# Here we'll be reading documents from an .RData file. Each row being a document, and columns for text and metadata (information about each document). This is the easiest option if you have metadata.
load("state-constitutions.RData")

# How many rows are there? How many columns? 
dim(df.states)

# What are the column names?

# Whats the time covered? Hint: data_conv is a Date vector.

# Inspect content vector
summary(nchar(df.states$text))
summary(stri_count_words(df.states$text)) # How many words are in the corpus in total?

# Inspect some press releases
set.seed(1991)
sample(df.states$text, 3)

############################
### Create and Setup DTM ###
############################
docs <- Corpus(VectorSource(df.states$text))
docs

# Once we have the corpus, we can inspect the documents using inspect()
inspect(docs[16])

# Many text analysis applications follow a similar 'recipe' for preprecessing, involving (the order of these steps might differ as per application):
#   
# 1. Tokenizing the text to unigrams (or bigrams, or trigrams)
# 2. Converting all characters to lowercase
# 3. Removing punctuation
# 4. Removing numbers
# 5. Removing Stop Words, inclugind custom stop words
# 6. "Stemming" words, or lemmitization. There are several stemming alogrithms. Porter is the most popular.
# 7. Creating a Document-Term Matrix
# 
# `tm` lets us convert a corpus to a DTM while completing the pre-processing steps in one step.

dtm <- DocumentTermMatrix(docs,
                          control = list(stopwords = TRUE,
                                         tolower = TRUE,
                                         removeNumbers = TRUE,
                                         removePunctuation = TRUE,
                                         stemming=TRUE))

# One common pre-processing step that some applicaitons may call for is applying tf-idf weights. The [tf-idf](https://en.wikipedia.org/wiki/Tf%E2%80%93idf), or term frequency-inverse document frequency, is a weight that ranks the importance of a term in its contextual document corpus. The tf-idf value increases proportionally to the number of times a word appears in the document, but is offset by the frequency of the word in the corpus, which helps to adjust for the fact that some words appear more frequently in general. In other words, it places importance on terms frequent in the document but rare in the corpus.

dtm.weighted <- DocumentTermMatrix(docs,
                                   control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE),
                                                  stopwords = TRUE,
                                                  tolower = TRUE,
                                                  removeNumbers = TRUE,
                                                  removePunctuation = TRUE,
                                                  stemming = TRUE))

# Compare first 5 rows and 5 columns of the `dtm` and `dtm.weighted`. What do you notice?
inspect(dtm[1:5,1:5])
inspect(dtm.weighted[1:5,1:5])

# Let's look at the structure of our DTM. Print the dimensions of the DTM. How many documents do we have? How many terms?
# how many documents? how many terms?
dim(dtm)

# We can obtain the term frequencies as a vector by converting the document term matrix into a matrix and using `colSums` to sum the column counts:
# how many terms?
freq <- colSums(as.matrix(dtm))
freq[1:5]
length(freq)

# By ordering the frequencies we can list the most frequent terms and the least frequent terms.
# order
sorted <- sort(freq, decreasing = T)
# Least frequent terms
head(sorted)
# most frequent
tail(sorted)

# Let's make a plot that shows the frequency of frequencies for the terms. (For example, how many words are used only once? 5 times? 10 times?)
# frequency of frenquencies
head(table(freq),15)
tail(table(freq),15)
# plot
plot(table(freq))

#What does this tell us about the nature of language?

#We can reorder columns of DTM to show most frequent terms first:

dtm.ordered <- dtm[,order(freq, decreasing = T)]
inspect(dtm.ordered[1:5,1:5])

# The TM package has lots of useful functions to help you explore common words and associations:

# Have a look at common words
findFreqTerms(dtm, lowfreq=100) # words that appear at least 100 times

# Which words correlate with "law"?
findAssocs(dtm, "law", 0.3)

# We can even make wordclouds showing the most commons terms:
set.seed(1991)
wordcloud(names(sorted), sorted, max.words=100, colors=brewer.pal(6,"Dark2"))

# Somtimes we want to remove sparse terms and thus inrease efficency. Look up the help file for the function `removeSparseTerms`. Using this function, create an objected called `dtm.s` that contains only terms with <.9 sparsity (meaning they appear in more than 10% of documents).
dtm.s <- removeSparseTerms(dtm,.9)
dtm 
dtm.s 

# We can convert a DTM to a matrix or data.frame in order to write to a csv, add meta data, etc.

# First create an object that converts the `dtm` to a dataframe (we first have to convert to matrix, and then to dataframe)

# coerce into dataframe
dtm <- as.data.frame(as.matrix(dtm))
names(docs)  # names of documents

# Export the dataframe as a csv.

# not run because it's a big file.
# write.csv(dtm, "dtm.csv")