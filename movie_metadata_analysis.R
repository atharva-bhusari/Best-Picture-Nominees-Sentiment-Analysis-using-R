library(tidyverse)
library(ggplot2)
library(stringr)
library(lubridate)


movies_df <- read.csv("./Data/oscar_movies.csv")

###################################################################
#  MPAA rating by year
##################################################################
movies_df %>% filter(Rated != "Unrated") %>% 
ggplot(., aes(x = Rated, fill = Rated)) + 
  geom_bar() + 
  facet_wrap(~Year, scales = "free") +
  labs(x = "MPAA Raitngs", y = "Number of Movies", 
       title = "MPAA Ratings by Year") + 
  theme_bw() +
  theme(panel.spacing = unit(0.5, "cm"),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 10), 
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(hjust=0.5))  +
  geom_text(stat = "count", aes(label=..count.., y=..count..), 
            position = position_stack(vjust=0.5),size=3)

#####################################################################
# Release by month, May be receny bias
######################################################################

movies_df$Date <- strptime(movies_df$Released, format = '%d-%b-%Y')
movies_df$Month <- format(movies_df$Date,format='%b')

months_ordered <- c("Jan","Feb","Mar","Apr","May","Jun", 
                               "Jul", "Aug", "Sep", "Oct", "Nov", 
                               "Dec")

movies_df$Month <- factor(movies_df$Month, levels = months_ordered)

  ggplot(movies_df, aes(x = Month, fill = Month)) + 
  geom_bar() + 
  facet_wrap(~Year, scales = "free") +
  labs(x = "Month of Release", y = "Number of Movies", 
       title = "Release by Month") + coord_flip() +
  theme_bw() +
  theme(panel.spacing = unit(0.5, "cm"),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 10), 
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(hjust=0.5))  +
  geom_text(stat = "count", aes(label=..count.., y=..count..), 
            position = position_stack(vjust=0.5),size=3)


######################################################################
## Average Runtime by Year
#######################################################################

movies_df <- movies_df %>%
    mutate(Runtime = str_replace(Runtime,"min$",""))

movies_df <- movies_df %>%
  mutate(Runtime = str_replace(Runtime," ",""))

movies_df$Runtime = as.numeric(movies_df$Runtime)

movies_df %>% group_by(Year) %>% summarise(mean(Runtime)) %>% 
  ggplot(aes(x=Year, y = `mean(Runtime)`, fill=Year)) + 
  geom_bar(stat = "identity") + 
  labs(x="Year", y="Average Runtime", title="Average Runtime by Year") +
  theme(plot.title = element_text(hjust=0.5), legend.position = "none")+
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150,by=30))

##############################################################
## Genre by Year
################################################################

movies_df$First_Genre <- sapply(strsplit(movies_df$Genre, ","), 
                                function(x) trimws(x[1]))


ggplot(movies_df, aes(x = First_Genre, fill = First_Genre)) + 
  geom_bar() + 
  facet_wrap(~Year, scales = "free") +
  labs(x = "Genre", y = "Number of Movies", 
       title = "Genre by Year") + coord_flip() +
  theme_bw() +
  theme(panel.spacing = unit(0.5, "cm"),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 10), 
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(hjust=0.5))  +
  geom_text(stat = "count", aes(label=..count.., y=..count..), 
            position = position_stack(vjust=0.5),size=3)

###################################################################
# Average Metascore by Year
##################################################################

movies_df %>% group_by(Year) %>% summarise(mean(Metascore)) %>% 
  ggplot(aes(x=Year, y = `mean(Metascore)`, fill=Year)) + 
  geom_bar(stat = "identity") + 
  labs(x="Year", y="Average Metascore", 
       title="Average Metascore by Year") +
  theme(plot.title = element_text(hjust=0.5), 
        legend.position = "none")

####################################################################
# Averange imdb rating by year
######################################################################

movies_df %>% group_by(Year) %>% summarise(mean(imdbRating)) %>% 
  ggplot(aes(x=Year, y = `mean(imdbRating)`, fill=Year)) + 
  geom_bar(stat = "identity") + 
  labs(x="Year", y="Average IMDb Rating", 
       title="Average IMDb Rating by Year") +
  theme(plot.title = element_text(hjust=0.5), 
        legend.position = "none")

###################################################################
#
#################################################################3

movies_df <- movies_df %>% 
  mutate(BoxOffice = str_remove_all(BoxOffice, "[$,]"))

movies_df$BoxOffice <-as.numeric(movies_df$BoxOffice)


movies_df %>% group_by(Year) %>% 
  summarise(mean(BoxOffice, na.rm=TRUE)) %>% 
  ggplot(aes(x=Year, y = `mean(BoxOffice, na.rm = TRUE)`, fill=Year)) + 
  geom_bar(stat = "identity") + 
  labs(x="Year", y="Average Box Office Earning",
       title="Average Box Office Earning by Year") +
  theme(plot.title = element_text(hjust=0.5), 
        legend.position = "none")

# TODO: Insert data for sound of metal and Judas, also scale box office
# amount  x10 million






