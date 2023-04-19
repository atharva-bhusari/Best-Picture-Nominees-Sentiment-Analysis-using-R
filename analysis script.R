library(tidyverse)
library(tidytext)


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





