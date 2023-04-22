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

plot_1 <- ggplot(bing_scripts_2023, aes(index, sentiment, fill=movie)) + 
  geom_bar(stat = "identity") + facet_wrap(~movie, ncol=2) +
  geom_smooth(span=.15)+ 
  labs(title = "Sentiments through the Movies of 2023") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))
ggsave("./graphs/sentiment_analysis/bing_sentiments_2023.png", plot_1, 
       width = 15, height = 10, dpi=300)

##2022
bing_scripts_2022 <- clean_scripts_2022 %>% 
  inner_join(get_sentiments("bing")) %>%
  group_by(movie) %>% 
  mutate(index = row_number() %/% 10) %>% 
  count(movie, index, sentiment) %>% 
  pivot_wider(names_from = "sentiment", values_from = "n") %>%
  mutate(sentiment = positive - negative)

plot_1<- ggplot(bing_scripts_2022, aes(index, sentiment, fill=movie)) + 
  geom_bar(stat = "identity") + facet_wrap(~movie, ncol=2) +
  geom_smooth(span=.15) + 
  labs(title = "Sentiments through the Movies of 2022") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))


ggsave("./graphs/sentiment_analysis/bing_sentiments_2022.png", plot_1, 
       width = 15, height = 10, dpi=300)

##2021
bing_scripts_2021 <- clean_scripts_2021 %>% 
  inner_join(get_sentiments("bing")) %>%
  group_by(movie) %>% 
  mutate(index = row_number() %/% 10) %>% 
  count(movie, index, sentiment) %>% 
  pivot_wider(names_from = "sentiment", values_from = "n") %>%
  mutate(sentiment = positive - negative)

plot_1 <- ggplot(bing_scripts_2021, aes(index, sentiment, fill=movie)) + 
  geom_bar(stat = "identity") + facet_wrap(~movie, ncol=2) +
  geom_smooth(span=.15) + 
  labs(title = "Sentiments through the Movies of 2021") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))
ggsave("./graphs/sentiment_analysis/bing_sentiments_2021.png", plot_1, 
       width = 15, height = 10, dpi=300)

##2020
bing_scripts_2020 <- clean_scripts_2020 %>% 
  inner_join(get_sentiments("bing")) %>%
  group_by(movie) %>% 
  mutate(index = row_number() %/% 10) %>% 
  count(movie, index, sentiment) %>% 
  pivot_wider(names_from = "sentiment", values_from = "n") %>%
  mutate(sentiment = positive - negative)

plot_1 <- ggplot(bing_scripts_2020, aes(index, sentiment, fill=movie)) + 
  geom_bar(stat = "identity") + facet_wrap(~movie, ncol=2) +
  geom_smooth(span=.15)+ 
  labs(title = "Sentiments through the Movies of 2020") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

ggsave("./graphs/sentiment_analysis/bing_sentiments_2020.png", plot_1, 
       width = 15, height = 10, dpi=300)

##2019

bing_scripts_2019 <- clean_scripts_2019 %>% 
  inner_join(get_sentiments("bing")) %>%
  group_by(movie) %>% 
  mutate(index = row_number() %/% 10) %>% 
  count(movie, index, sentiment) %>% 
  pivot_wider(names_from = "sentiment", values_from = "n") %>%
  mutate(sentiment = positive - negative)

plot_1 <- ggplot(bing_scripts_2019, aes(index, sentiment, fill=movie)) + 
  geom_bar(stat = "identity") + facet_wrap(~movie, ncol=2) +
  geom_smooth(span=.15)+ 
  labs(title = "Sentiments through the Movies of 2019") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust=0.5))

ggsave("./graphs/sentiment_analysis/bing_sentiments_2019.png", plot_1, 
       width = 15, height = 10, dpi=300)

#######################################################
# Drilling down to winning movies
##########################################################

#2023

bing_winner_2023 <- clean_scripts_2023 %>% 
  filter(movie=="Everything Everywhere All at Once") %>% 
  inner_join(get_sentiments("bing")) %>%
  group_by(movie) %>% 
  mutate(index = row_number() %/% 25) %>% 
  count(movie, index, sentiment) %>% 
  pivot_wider(names_from = "sentiment", values_from = "n") %>%
  replace_na(.,list(positive=0, negative=0)) %>% 
  mutate(sentiment = positive - negative)

plot_1 <- ggplot(bing_winner_2023, aes(index, sentiment, fill=movie)) + 
  geom_bar(stat = "identity") +
  geom_smooth(span=.15) + 
  labs(title = "Sentiments through Everything Everywhere All at Once") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

ggsave("./graphs/sentiment_analysis/bing_sentiments_2023_winner.png", plot_1, 
       width = 15, height = 10, dpi=300)


#2022
bing_winner_2022 <- clean_scripts_2022 %>% 
  filter(movie=="CODA") %>% 
  inner_join(get_sentiments("bing")) %>%
  group_by(movie) %>% 
  mutate(index = row_number() %/% 15) %>% 
  count(movie, index, sentiment) %>% 
  pivot_wider(names_from = "sentiment", values_from = "n") %>%
  replace_na(.,list(positive=0, negative=0)) %>% 
  mutate(sentiment = positive - negative)

plot_1 <- ggplot(bing_winner_2022, aes(index, sentiment, fill=movie)) + 
  geom_bar(stat = "identity") +
  geom_smooth(span=.15) + 
  labs(title = "Sentiments through CODA") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

ggsave("./graphs/sentiment_analysis/bing_sentiments_2022_winner.png", plot_1, 
       width = 15, height = 10, dpi=300)

#2021

bing_winner_2021 <- clean_scripts_2021 %>% 
  filter(movie=="Nomadland") %>% 
  inner_join(get_sentiments("bing")) %>%
  group_by(movie) %>% 
  mutate(index = row_number() %/% 15) %>% 
  count(movie, index, sentiment) %>% 
  pivot_wider(names_from = "sentiment", values_from = "n") %>%
  replace_na(.,list(positive=0, negative=0)) %>% 
  mutate(sentiment = positive - negative)

plot_1 <- ggplot(bing_winner_2021, aes(index, sentiment, fill=movie)) + 
  geom_bar(stat = "identity") +
  geom_smooth(span=.15) + 
  labs(title = "Sentiments through Nomadland") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

ggsave("./graphs/sentiment_analysis/bing_sentiments_2021_winner.png", plot_1, 
       width = 15, height = 10, dpi=300)

#2020

bing_winner_2020 <- clean_scripts_2020 %>% 
  filter(movie=="Parasite") %>% 
  inner_join(get_sentiments("bing")) %>%
  group_by(movie) %>% 
  mutate(index = row_number() %/% 20) %>% 
  count(movie, index, sentiment) %>% 
  pivot_wider(names_from = "sentiment", values_from = "n") %>%
  replace_na(.,list(positive=0, negative=0)) %>% 
  mutate(sentiment = positive - negative)

plot_1 <- ggplot(bing_winner_2020, aes(index, sentiment, fill=movie)) + 
  geom_bar(stat = "identity") +
  geom_smooth(span=.15) + 
  labs(title = "Sentiments through Parasite") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

ggsave("./graphs/sentiment_analysis/bing_sentiments_2020_winner.png", plot_1, 
       width = 15, height = 10, dpi=300)


#2019

bing_winner_2019 <- clean_scripts_2019 %>% 
  filter(movie=="Green Book") %>% 
  inner_join(get_sentiments("bing")) %>%
  group_by(movie) %>% 
  mutate(index = row_number() %/% 15) %>% 
  count(movie, index, sentiment) %>% 
  pivot_wider(names_from = "sentiment", values_from = "n") %>%
  replace_na(.,list(positive=0, negative=0)) %>% 
  mutate(sentiment = positive - negative)

plot_1 <- ggplot(bing_winner_2019, aes(index, sentiment, fill=movie)) + 
  geom_bar(stat = "identity") +
  geom_smooth(span=.15) + 
  labs(title = "Sentiments through Green Book") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

ggsave("./graphs/sentiment_analysis/bing_sentiments_2019_winner.png", plot_1, 
       width = 15, height = 10, dpi=300)
