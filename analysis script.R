library(tidyverse)
library(tidytext)
library(ggplot2)


##################################################################
#
# Removing stop words and cleaning data
#
###################################################################

movies_2023 <- scripts_2023 %>% group_by(movie) %>% 
  summarise(total_lines = n())


maverick <- scripts_2023 %>%  filter(.$movie == "Top Gun: Maverick")
maverick <- maverick[,-2]

maverick <- maverick %>% unnest_tokens(word, line) %>% 
  mutate(word = str_extract(word, "[a-z]+")) %>% filter(word != 'NA') %>% 
  anti_join(stop_words)

View(maverick)

#######################################################################
#
# Using NRC
#
###############################################################################

nrc_anger <- get_sentiments("nrc") %>% filter(sentiment == 'anger')

nrc_anticipation <- get_sentiments("nrc") %>% 
  filter(sentiment == 'anticipation') 

nrc_disgust <- get_sentiments("nrc") %>% filter(sentiment == 'disgust') 

nrc_fear <- get_sentiments("nrc") %>% filter(sentiment == 'fear') 

nrc_joy <- get_sentiments("nrc") %>% filter(sentiment == 'joy') 

nrc_negative <- get_sentiments("nrc") %>% filter(sentiment == 'negative') 

nrc_positive <- get_sentiments("nrc") %>% filter(sentiment == 'positive') 

nrc_sadness <- get_sentiments("nrc") %>% filter(sentiment == 'sadness') 

nrc_surprise <- get_sentiments("nrc") %>% filter(sentiment == 'surprise') 

nrc_trust <- get_sentiments("nrc") %>% filter(sentiment == 'trust') 

maverick_anger <- maverick %>% inner_join(nrc_anger) %>% 
  count(word, sort = TRUE)
head(maverick_anger)

maverick_anticipation <- maverick %>% inner_join(nrc_anticipation) %>% 
  count(word, sort = TRUE)
head(maverick_anticipation)

maverick_disgust <- maverick %>% inner_join(nrc_disgust) %>% 
  count(word, sort = TRUE)
head(maverick_disgust)

maverick_fear <- maverick %>% inner_join(nrc_fear) %>% 
  count(word, sort = TRUE)
head(maverick_fear)

maverick_joy <- maverick %>% inner_join(nrc_joy) %>% 
  count(word, sort = TRUE)
head(maverick_joy)

maverick_negative <- maverick %>% inner_join(nrc_negative) %>% 
  count(word, sort = TRUE)
head(maverick_negative)

maverick_positve <- maverick %>% inner_join(nrc_positive) %>% 
  count(word, sort = TRUE)
head(maverick_positve)

maverick_sadness <- maverick %>% inner_join(nrc_sadness) %>% 
  count(word, sort = TRUE)
head(maverick_sadness)

maverick_surprise <- maverick %>% inner_join(nrc_surprise) %>% 
  count(word, sort = TRUE)
head(maverick_surprise)

maverick_trust <- maverick %>% inner_join(nrc_trust) %>% 
  count(word, sort = TRUE)
head(maverick_trust)
