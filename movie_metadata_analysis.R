library(tidyverse)
library(ggplot2)
library(stringr)
library(lubridate)
library(wordcloud2)
library(webshot2)
library(RColorBrewer)



movies_df <- read.csv("./Data/oscar_movies.csv")

###################################################################
#  MPAA rating by year
##################################################################
plot_1 <- movies_df %>% filter(Rated != "Unrated") %>% 
ggplot(., aes(x = Rated, fill = Rated)) + 
  geom_bar() + 
  facet_wrap(~Year, scales = "free") +
  labs(x = "MPAA Raitngs", y = "Number of Movies", 
       title = "MPAA Ratings by Year") + 
  theme_bw() +
  theme(panel.spacing = unit(0.5, "cm"),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 10, color = 'white'), 
        axis.text.x = element_text(size = 8, color = 'white'),
        axis.title = element_text(size = 8, color='white'),
        plot.title = element_text(hjust=0.5, color = 'white'),
        panel.background = element_rect(fill = "#1b1b1b"),
        plot.background = element_rect(fill = "#1b1b1b"),
        axis.text = element_text(color = "white"),
        legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())  +
  geom_text(stat = "count", aes(label=..count.., y=..count..), 
            position = position_stack(vjust=0.5),size=3)

ggsave("./graphs/metadata_analysis/MPAA_Rating_by_Year.png", plot_1, width = 6, height = 4,dpi=300)
#####################################################################
# Release by month, May be recency bias
######################################################################

movies_df$Date <- strptime(movies_df$Released, format = '%d-%b-%Y')
movies_df$Month <- format(movies_df$Date,format='%b')

months_ordered <- c("Jan","Feb","Mar","Apr","May","Jun", 
                               "Jul", "Aug", "Sep", "Oct", "Nov", 
                               "Dec")

movies_df$Month <- factor(movies_df$Month, levels = months_ordered)

  plot_2 <- ggplot(movies_df, aes(x = Month, fill = Month)) + 
  geom_bar() + 
  facet_wrap(~Year, scales = "free") +
  labs(x = "Month of Release", y = "Number of Movies", 
       title = "Release by Month") + coord_flip() +
  theme_bw() +
    theme(panel.spacing = unit(0.5, "cm"),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 10, color = 'white'),
          axis.text.x = element_blank(),
          axis.title = element_text(size = 8, color='white'),
          plot.title = element_text(hjust=0.5, color = 'white'),
          panel.background = element_rect(fill = "#1b1b1b"),
          plot.background = element_rect(fill = "#1b1b1b"),
          axis.text = element_text(color = "white"),
          legend.position = "none",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) + 
  geom_text(stat = "count", aes(label=..count.., y=..count..), 
            position = position_stack(vjust=0.5),size=3)

  ggsave("./graphs//metadata_analysis/Release_by_month.png", plot_2, width = 6, height = 4,dpi=300)
######################################################################
## Average Runtime by Year
#######################################################################

movies_df <- movies_df %>%
    mutate(Runtime = str_replace(Runtime,"min$",""))

movies_df <- movies_df %>%
  mutate(Runtime = str_replace(Runtime," ",""))

movies_df$Runtime = as.numeric(movies_df$Runtime)

plot_3<- movies_df %>% group_by(Year) %>% summarise(mean(Runtime)) %>% 
  ggplot(aes(x=Year, y = `mean(Runtime)`)) + 
  geom_bar(stat = "identity", fill = 'steelblue') +
  labs(x="Year", y="Average Runtime", title="Average Runtime by Year") +
  theme(plot.title = element_text(hjust=0.5, color = "white"), 
        legend.position = "none",
        panel.background = element_rect(fill = "#1b1b1b"),
        plot.background = element_rect(fill = "#1b1b1b"),
        axis.title = element_text(size = 10, color='white'),
        axis.text = element_text(color = "white")
  )+
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150,by=30))
ggsave("./graphs//metadata_analysis/Average_Runtime_by_Year.png", plot_3, width = 6, height = 4,dpi=300)
##############################################################
## Genre by Year
################################################################

movies_df$First_Genre <- sapply(strsplit(movies_df$Genre, ","), 
                                function(x) trimws(x[1]))


plot_4 <- ggplot(movies_df, aes(x = First_Genre, fill = First_Genre)) + 
  geom_bar() + 
  facet_wrap(~Year, scales = "free") +
  labs(x = "Genre", y = "Number of Movies", 
       title = "Genre by Year") + coord_flip() +
  theme_bw() +
  theme(panel.spacing = unit(0.5, "cm"),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 10, color = 'white'),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 8, color='white'),
        plot.title = element_text(hjust=0.5, color = 'white'),
        panel.background = element_rect(fill = "#1b1b1b"),
        plot.background = element_rect(fill = "#1b1b1b"),
        axis.text = element_text(color = "white"),
        legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())  +
  geom_text(stat = "count", aes(label=..count.., y=..count..), 
            position = position_stack(vjust=0.5),size=3)
ggsave("./graphs//metadata_analysis/Genre_by_year.png", plot_4, width = 6, height = 4,dpi=300)
###################################################################
# Average Metascore by Year
##################################################################
#2023
plot_5 <- movies_df %>% filter(Year == 2022) %>% 

ggplot(., aes(x = Metascore, y = imdbRating, label = Title, color = Title)) +
  geom_point(size = 5) +
  scale_color_discrete(guide = guide_legend(title = "Movie Name")) +
  geom_text(hjust=0.5,vjust=2, size = 5.5) + 
  labs(x="Critic Rating", y="Viewer Rating",
       title = "Viewer Rating vs Critic Rating for Year 2023") + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust=0.5, color = 'white'),
        panel.background = element_rect(fill = "#1b1b1b"),
        plot.background = element_rect(fill = "#1b1b1b"),
        axis.title = element_text(size = 15, color='white'),
        axis.text = element_text(color = "white")) + xlim(55,100)

ggsave("./graphs//metadata_analysis/Ctitic_Rating_vs_Viewer_rating_2023.png", 
       plot_5, width = 10, height = 10,dpi=300)

#2022
plot_5 <- movies_df %>% filter(Year == 2021) %>% 
  
  ggplot(., aes(x = Metascore, y = imdbRating, label = Title, color = Title)) +
  geom_point(size = 5) +
  scale_color_discrete(guide = guide_legend(title = "Movie Name")) +
  geom_text(hjust=0.5,vjust=2, size=5.5) + 
  labs(x="Critic Rating", y="Viewer Rating",
       title = "Viewer Rating vs Critic Rating for Year 2022") + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust=0.5, color = 'white'),
        panel.background = element_rect(fill = "#1b1b1b"),
        plot.background = element_rect(fill = "#1b1b1b"),
        axis.title = element_text(size = 15, color='white'),
        axis.text = element_text(color = "white")) + xlim(45,100)

ggsave("./graphs/metadata_analysis/Ctitic_Rating_vs_Viewer_rating_2022.png", 
       plot_5, width = 10, height = 10,dpi=300)

plot_5 <- movies_df %>% filter(Year == 2020) %>% 
  
  ggplot(., aes(x = Metascore, y = imdbRating, label = Title, color = Title)) +
  geom_point(size = 5) +
  scale_color_discrete(guide = guide_legend(title = "Movie Name")) +
  geom_text(hjust=0.5,vjust=2, size=5.5) + 
  labs(x="Critic Rating", y="Viewer Rating",
       title = "Viewer Rating vs Critic Rating for Year 2021") + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust=0.5, color = 'white'),
        panel.background = element_rect(fill = "#1b1b1b"),
        plot.background = element_rect(fill = "#1b1b1b"),
        axis.title = element_text(size = 15, color='white'),
        axis.text = element_text(color = "white")) + xlim(45,100)

ggsave("./graphs/metadata_analysis/Ctitic_Rating_vs_Viewer_rating_2021.png", 
       plot_5, width = 10, height = 10,dpi=300)

plot_5 <- movies_df %>% filter(Year == 2019) %>% 
  
  ggplot(., aes(x = Metascore, y = imdbRating, label = Title, color = Title)) +
  geom_point(size = 5) +
  scale_color_discrete(guide = guide_legend(title = "Movie Name")) +
  geom_text(hjust=0.5,vjust=2,size=5.5) + 
  labs(x="Critic Rating", y="Viewer Rating",
       title = "Viewer Rating vs Critic Rating for Year 2020") + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust=0.5, color = 'white'),
        panel.background = element_rect(fill = "#1b1b1b"),
        plot.background = element_rect(fill = "#1b1b1b"),
        axis.title = element_text(size = 15, color='white'),
        axis.text = element_text(color = "white")) + xlim(45,100)

ggsave("./graphs/metadata_analysis/Ctitic_Rating_vs_Viewer_rating_2020.png", 
       plot_5, width = 10, height = 10,dpi=300)

plot_5 <- movies_df %>% filter(Year == 2018) %>% 
  
  ggplot(., aes(x = Metascore, y = imdbRating, label = Title, color = Title)) +
  geom_point(size = 5) +
  scale_color_discrete(guide = guide_legend(title = "Movie Name")) +
  geom_text(hjust=0.5,vjust=2,size=5.5) + 
  labs(x="Critic Rating", y="Viewer Rating",
       title = "Viewer Rating vs Critic Rating for Year 2019") + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust=0.5, color = 'white'),
        panel.background = element_rect(fill = "#1b1b1b"),
        plot.background = element_rect(fill = "#1b1b1b"),
        axis.title = element_text(size = 15, color='white'),
        axis.text = element_text(color = "white")) + xlim(45,100)

ggsave("./graphs/metadata_analysis/Ctitic_Rating_vs_Viewer_rating_2019.png", 
       plot_5, width = 10, height = 10,dpi=300)

############################################################################
# Viewer Rating vs Critic Rating for all the nominees
##############################################################################

plot_5 <- movies_df %>% 
  
  ggplot(., aes(x = Metascore, y = imdbRating, label = Title, color = Title)) +
  geom_point(size = 5) +
  scale_color_discrete(guide = guide_legend(title = "Movie Name")) +
  geom_text(hjust=0.5,vjust=2, size = 5.5) + 
  labs(x="Critic Rating", y="Viewer Rating",
       title = "Viewer Rating vs Critic Rating for all nominees") + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust=0.5, color = 'white'),
        panel.background = element_rect(fill = "#1b1b1b"),
        plot.background = element_rect(fill = "#1b1b1b"),
        axis.title = element_text(size = 15, color='white'),
        axis.text = element_text(color = "white")) + xlim(55,100)

ggsave("./graphs//metadata_analysis/Ctitic_Rating_vs_Viewer_rating_all_nominees.png", 
       plot_5, width = 10, height = 10,dpi=300)

###################################################################
# Average box office earning by year
#################################################################3

movies_df <- movies_df %>% 
  mutate(BoxOffice = str_remove_all(BoxOffice, "[$,]"))

movies_df$BoxOffice <-as.numeric(movies_df$BoxOffice)
movies_df$BoxOffice <- movies_df$BoxOffice/1000000


plot_6 <- movies_df %>% group_by(Year) %>% 
  summarise(mean(BoxOffice, na.rm=TRUE)) %>% 
  ggplot(aes(x=Year, y = `mean(BoxOffice, na.rm = TRUE)`, fill=Year)) + 
  geom_line(stat = "identity", aes(col="#F71707"), size=1.5) + 
  geom_point(color="yellow", size=5) +
  labs(x="Year", y="Average Box Office Earning(x1000000)",
       title="Average Box Office Earning by Year") +
  theme(plot.title = element_text(hjust=0.5, color = "white"), 
        legend.position = "none",
        panel.background = element_rect(fill = "#1b1b1b"),
        plot.background = element_rect(fill = "#1b1b1b"),
        axis.title = element_text(size = 15, color='white'),
        axis.text = element_text(color = "white"))
ggsave("./graphs/metadata_analysis/Average_box_office_earing_by_year.png", 
       plot_6, width = 10, height = 10,dpi=300)
###################################################################
# Average imdb votes per year
###############################################################

movies_df <- movies_df %>% 
  mutate(imdbVotes = str_remove_all(imdbVotes, "[,]"))
movies_df$imdbVotes <- as.numeric(movies_df$imdbVotes)
movies_df$imdbVotes <- movies_df$imdbVotes/100000

plot_7 <- movies_df %>% group_by(Year) %>% 
  summarise(mean(imdbVotes, na.rm=TRUE)) %>% 
  ggplot(aes(x=Year, y = `mean(imdbVotes, na.rm = TRUE)`, fill=Year)) + 
  geom_line(stat = "identity", aes(col="red"), size=1.5) + 
  geom_point(color="yellow", size=5) +
  labs(x="Year", y="Average IMDb Votes(x100000)",
       title="Average IMDb Votes per year") +
  theme(plot.title = element_text(hjust=0.5, color = "white"), 
        legend.position = "none",
        panel.background = element_rect(fill = "#1b1b1b"),
        plot.background = element_rect(fill = "#1b1b1b"),
        axis.title = element_text(size = 15, color='white'),
        axis.text = element_text(color = "white"))
ggsave("./graphs/metadata_analysis/Average_box_imdb_votes_per_year.png", 
       plot_7, width = 10, height = 10,dpi=300)
#############################################################
# Language analysis
############################################################

df_split <- separate_rows(movies_df, Language, sep = ", ")
df_count <- count(df_split, Language)
print(df_count)

wordcloud2(
  df_count %>% filter(Language != "English"),
  rotateRatio = 0,
  shape = "cicle"
)

export_png("./graphs/metadata_analysis/language_wordcloud.png", delay=5)















