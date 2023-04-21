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

head(movies_2023)

clean_scripts_2023 <- scripts_2023 %>% unnest_tokens(word, line) %>% 
  mutate(word = str_extract(word, "[a-z]+")) %>% filter(word != 'NA') %>% 
  anti_join(stop_words)

clean_scripts_2022 <- scripts_2022 %>% unnest_tokens(word, line) %>% 
  mutate(word = str_extract(word, "[a-z]+")) %>% filter(word != 'NA') %>% 
  anti_join(stop_words)

clean_scripts_2021 <- scripts_2021 %>% unnest_tokens(word, line) %>% 
  mutate(word = str_extract(word, "[a-z]+")) %>% filter(word != 'NA') %>% 
  anti_join(stop_words)

clean_scripts_2020 <- scripts_2020 %>% unnest_tokens(word, line) %>% 
  mutate(word = str_extract(word, "[a-z]+")) %>% filter(word != 'NA') %>% 
  anti_join(stop_words)

clean_scripts_2019 <- scripts_2019 %>% unnest_tokens(word, line) %>% 
  mutate(word = str_extract(word, "[a-z]+")) %>% filter(word != 'NA') %>% 
  anti_join(stop_words)

#2023
bing_scripts_2023 <- clean_scripts_2023 %>% 
  inner_join(get_sentiments("bing")) %>%
  group_by(movie) %>% 
  mutate(index = row_number() %/% 10) %>% 
  count(movie, index, sentiment) %>% 
  pivot_wider(names_from = "sentiment", values_from = "n") %>%
  mutate(sentiment = positive - negative)

ggplot(bing_scripts_2023, aes(index, sentiment, fill=movie)) + 
  geom_bar(stat = "identity") + facet_wrap(~movie, ncol=2) +
  geom_smooth(span=.15)+
  theme(legend.position = "none")

##2022
bing_scripts_2022 <- clean_scripts_2022 %>% 
  inner_join(get_sentiments("bing")) %>%
  group_by(movie) %>% 
  mutate(index = row_number() %/% 10) %>% 
  count(movie, index, sentiment) %>% 
  pivot_wider(names_from = "sentiment", values_from = "n") %>%
  mutate(sentiment = positive - negative)

ggplot(bing_scripts_2022, aes(index, sentiment, fill=movie)) + 
  geom_bar(stat = "identity") + facet_wrap(~movie, ncol=2) +
  geom_smooth(span=.15)+
  theme(legend.position = "none")

##2021
bing_scripts_2021 <- clean_scripts_2021 %>% 
  inner_join(get_sentiments("bing")) %>%
  group_by(movie) %>% 
  mutate(index = row_number() %/% 10) %>% 
  count(movie, index, sentiment) %>% 
  pivot_wider(names_from = "sentiment", values_from = "n") %>%
  mutate(sentiment = positive - negative)

ggplot(bing_scripts_2021, aes(index, sentiment, fill=movie)) + 
  geom_bar(stat = "identity") + facet_wrap(~movie, ncol=2) +
  geom_smooth(span=.15)+
  theme(legend.position = "none")

##2020
bing_scripts_2020 <- clean_scripts_2020 %>% 
  inner_join(get_sentiments("bing")) %>%
  group_by(movie) %>% 
  mutate(index = row_number() %/% 10) %>% 
  count(movie, index, sentiment) %>% 
  pivot_wider(names_from = "sentiment", values_from = "n") %>%
  mutate(sentiment = positive - negative)

ggplot(bing_scripts_2020, aes(index, sentiment, fill=movie)) + 
  geom_bar(stat = "identity") + facet_wrap(~movie, ncol=2) +
  geom_smooth(span=.15)+
  theme(legend.position = "none")

##2019

bing_scripts_2019 <- clean_scripts_2019 %>% 
  inner_join(get_sentiments("bing")) %>%
  group_by(movie) %>% 
  mutate(index = row_number() %/% 10) %>% 
  count(movie, index, sentiment) %>% 
  pivot_wider(names_from = "sentiment", values_from = "n") %>%
  mutate(sentiment = positive - negative)

ggplot(bing_scripts_2019, aes(index, sentiment, fill=movie)) + 
  geom_bar(stat = "identity") + facet_wrap(~movie, ncol=2) +
  geom_smooth(span=.15)+
  theme(legend.position = "none")








