library(tidyverse)
library(readr)
library(stringr)
library(dplyr)

words <- read.delim("data/words_alpha.txt") 
colnames(words) <- "word"
words_char <- words %>% mutate("letters" = nchar(word)) %>% 
  filter(letters == 8)

m <- str_subset(words_char$word, pattern = "^m[a-z]") 

first_word <- substr(m, 1,4) 
second_word <- substr(m, 5, 7)
last_letter <- substr(m, 8, 8)

new_last_word <- paste0(last_letter, second_word)

phrase <- paste(first_word, new_last_word) 

phrase <- tibble(c(first_word), c(new_last_word)) 

colnames(phrase) <- c("word", "second word")

phrase <- inner_join(phrase, words, by = "word")

phrase <- phrase %>% filter(`second word` %in% words$word)
