# Data management and cleaning

###############
### Setup R ###
###############

### Clear space ####
rm(list = ls())

### Clear terminal ####
cat("\014")

setwd('~/Dropbox/cope-crabtree/text analysis course/2022/data/hr-reports/')

### Load library packages and custom functions ####
library(readtext)
library(rvest)
library(textclean)
library(stringr)

### Import data ####
hr <- readtext(list.files(pattern = ".txt",
                          recursive = T))
head(hr)

### Create meta-data ####
hr$country <- NA
hr$year <- NA
hr$org <- NA

for (i in 1:nrow(hr)) {
  hr$country[i] <- str_split(hr$doc_id, "_")[[i]][1]
  hr$year[i] <- str_split(hr$doc_id, "_")[[i]][2]
  hr$org[i] <- str_split(hr$doc_id, "_")[[i]][3]
}


# check_text(hr$text), takes a while to process but generates great helpful output https://rpubs.com/WulanAndriyani/TextPreprocessing

### Clean texts ####
hr$text <- replace_html(hr$text, symbol = FALSE)
hr$text <- replace_email(hr$text)
hr$text <- replace_url(hr$text)
hr$text <- replace_tag(hr$text)
hr$text <- replace_number(hr$text)
hr$text <- replace_word_elongation(hr$text)
