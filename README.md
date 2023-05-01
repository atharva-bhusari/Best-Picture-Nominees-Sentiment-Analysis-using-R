# 597_Data_Wrangling: Best Picture Nominations Sentiment Analysis

- Analyze the text of Oscar-nominated movies to identify the most commonly used words and determine if any are associated with great movies.

- Data collection from online sources, data pre-processing to remove irrelevant information, data analysis using NLP techniques to identify common words and phrases.

- Identification of commonly used words and phrases, determination of elements associated with great movies, comparison of results with critical and commercial success of movies to identify any correlations.

- Analysis of Oscar-nominated movies for commonly used words can provide valuable insights into the characteristics of great movies, useful for filmmakers, movie studios, and critics.

## Data Extraction

-   Extracted meta_data using [OMDB API](https://www.omdbapi.com/)
-   Extracted pdf scripts from [Deadline.com](https://deadline.com/)
-   Parsed these scripts using R package [pdftools](https://cran.r-project.org/web/packages/pdftools/index.html)
