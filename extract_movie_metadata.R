library(httr)
library(jsonlite)

extract_movie_data = function(movies, year){
  movies_df = data.frame()
  for (movie in movies) {
    movie_title = URLencode(movie)
    url = paste0("http://www.omdbapi.com/?t=", movie_title, "&y=", year, "&apikey=3a598714")
    response = GET(url)
    content = content(response, as = "text")
    movie_data = fromJSON(content)
    movie_data = movie_data[-which(names(movie_data) %in% "Ratings")]
    movies_df = rbind(movies_df, movie_data)
  }
  return(movies_df)
}

movies_2023 = c('Women Talking','Everything Everywhere All at Once',
                'The Banshees of Inisherin','Triangle of Sadness','The Fabelmans',
                'All Quiet on the Western Front','Elvis')
movies_df_2023 = extract_movie_data(movies_2023, 2022)

movies_2022 = c('Nightmare Alley','Dont Look Up','Dune','Belfast','The Power of the Dog',
                'West Side Story','King Richard','Licorice Pizza','CODA','Drive My Car',
                "Don't Look Up")
movies_df_2022 = extract_movie_data(movies_2022, 2021)

movies_2021 = c('Sound of Metal','Mank','Minari','Promising Young Woman','The Father',
               'Judas and the Black Messiah','The Trial of the Chicago 7','Nomadland')
movies_df_2021 = extract_movie_data(movies_2021, 2020)

movies_2020 = c('1917','Ford V Ferrari','Joker','Parasite','The Irishman','Little Women',
                'Jojo Rabbit','Marriage Story','Once Upon a Time in Hollywood')
movies_df_2020 = extract_movie_data(movies_2020, 2019)

movies_2019 = c('Bohemian Rhapsody','The Favourite','Black Panther','BlacKkKlansman',
                'Green Book','Vice','A Star is Born','Roma')
movies_df_2019 = extract_movie_data(movies_2019, 2018)

oscar_movies = rbind(movies_df_2023,movies_df_2022,movies_df_2021,movies_df_2020,
                     movies_df_2019)

write_csv(oscar_movies, "./Data/oscar_movies.csv")