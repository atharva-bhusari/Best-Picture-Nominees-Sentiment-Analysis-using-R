library(pdftools)

make_scripts = function(pdf_urls, movie_mapping) {
  
  scripts = tibble(line = character(0), movie = character(0))
  
  for (i in 1:length(pdf_urls)) {
    temp_file = tempfile(fileext = ".pdf")
    download.file(pdf_urls[i], temp_file, mode = "wb")
    
    pdf = pdf_text(temp_file)
    pdf_df = tibble(line = character(0), movie = character(0))
    
    for (j in seq_along(pdf)) {
      lines = strsplit(pdf[j], "\n")[[1]]
      page_df = tibble(line = lines)
      page_df = page_df %>% mutate(movie = as.character(i))
      pdf_df = bind_rows(pdf_df, page_df)
    }
    
    scripts = bind_rows(scripts, pdf_df)
  }
  
  scripts = scripts %>% mutate(movie = ifelse(movie %in% names(movie_mapping), movie_mapping[movie], movie))
  
  return(scripts)
}

### 2023 ###

pdf_urls = c("https://paramountpicturesfyc.com/assets/cms/TOP-GUN-MAVERICK_Final.pdf",
              "https://d1qf1lxchdoll2.cloudfront.net/2022/uar/screenplay/wt/WOMEN_TALKING_SCRIPT.pdf",
              "https://nofilmschool.com/sites/default/files/eeaao_script.pdf",
              "https://www.searchlightpictures.com/fyc/media/screenings/film/film_scripts-c8f5adb8-3c2a-483c-92bf-82c8f87ca77a.pdf",
              "https://deadline.com/wp-content/uploads/2023/01/Triangle-Of-Sadness-Read-The-Screenplay.pdf",
              "https://awards.universalpictures.com/the-fabelmans/screenplay/The_Fabelmans.pdf",
              "https://deadline.com/wp-content/uploads/2023/01/All-Quiet-On-The-Western-Front-Read-The-Screenplay.pdf",
              "https://deadline.com/wp-content/uploads/2023/01/Elvis-Read-The-Screenplay.pdf",
              "https://focusfeaturesguilds2022.com/tar/tar.pdf")

movie_mapping = c('1' = 'Top Gun: Maverick',
                  '2' = 'Women Talking',
                  '3' = 'Everything Everywhere All at Once',
                  '4' = 'The Banshees of Inisherin',
                  '5' = 'Triangle of Sadness',
                  '6' = 'The Fabelmans',
                  '7' = 'All Quiet on the Western Front',
                  '8' = 'Elvis',
                  '9' = 'Tar')

scripts_2023 = make_scripts(pdf_urls, movie_mapping)

### 2022 ###

pdf_urls = c("https://deadline.com/wp-content/uploads/2022/01/Nightmare-Alley-Read-The-Screenplay-1.pdf",
             "https://deadline.com/wp-content/uploads/2022/01/Dont-Look-Up-Read-The-Screenplay.pdf",
             "https://d2bu9v0mnky9ur.cloudfront.net/academy2021/scripts/duneMxFtT98NYwBsMltl20211109/dune_final_shooting_script_6_19_20.pdf",
             "https://deadline.com/wp-content/uploads/2022/01/Belfast-Read-The-Screenplay_Redacted.pdf",
             "https://deadline.com/wp-content/uploads/2022/01/The-Power-Of-The-Dog-Read-The-Screenplay.pdf",
             "https://deadline.com/wp-content/uploads/2022/01/West-Side-Story-Read-The-Screenplay.pdf",
             "https://deadline.com/wp-content/uploads/2022/01/King-Richard-Read-The-Screenplay-2.pdf")

movie_mapping = c('1' = 'Nightmare Alley',
                  '2' = 'Dont Look Up',
                  '3' = 'Dune',
                  '4' = 'Belfast',
                  '5' = 'The Power of the Dog',
                  '6' = 'West Side Story',
                  '7' = 'King Richard')

scripts_2022 = make_scripts(pdf_urls, movie_mapping)

### 2021 ###

pdf_urls = c("https://d2detfmr8cx0ni.cloudfront.net/8/ef/7e028/3a21e/13c33/9d2fd/fca13/5496.pdf",
             "https://deadline.com/wp-content/uploads/2021/02/Mank-Screenplay.pdf",
             "https://deadline.com/wp-content/uploads/2021/03/Minari-Screenplay.pdf",
             "https://focusfeaturesguilds2020.com/promising-young-woman/screenplay/Promising_Young_Woman.pdf",
             "https://www.sonyclassics.com/assets/screenplays/thefather/thefather-screenplay.pdf",
             "https://d2bu9v0mnky9ur.cloudfront.net/academy2020/scripts/sxdTyWbSrntjatbm/jatm_2021_01_06.pdf",
             "https://deadline.com/wp-content/uploads/2020/12/The-Trial-of-the-Chicago-7-Script.pdf",
             "https://deadline.com/wp-content/uploads/2021/02/Nomadland-Screenplay.pdf")

movie_mapping = c('1' = 'Sound of Metal',
                  '2' = 'Mank',
                  '3' = 'Minari',
                  '4' = 'Promising Young Woman',
                  '5' = 'The Father',
                  '6' = 'Judas and the Black Messiah',
                  '7' = 'The Trial of the Chicago 7',
                  '8' = 'Nomadland')

scripts_2021 = make_scripts(pdf_urls, movie_mapping)

### 2020 ###

pdf_urls = c("https://variety.com/wp-content/uploads/2020/01/1917.pdf",
             "https://deadline.com/wp-content/uploads/2019/12/ford-v-ferrari-script-final.pdf",
             "https://d2bu9v0mnky9ur.cloudfront.net/academy2019/screenplay/joker/joker_new_final.pdf",
             "https://deadline.com/wp-content/uploads/2020/01/parasite-script.pdf",
             "https://deadline.com/wp-content/uploads/2019/12/the-irishman-ampas-script.pdf",
             "https://www.sonypictures-awards.com/static/files/LITTLE%20WOMEN%20-%20GG%20FINAL%20(Revisions%20by%20Alessia)%20tmk.pdf",
             "https://deadline.com/wp-content/uploads/2020/01/jojo-rabbit-final-script.pdf",
             "https://deadline.com/wp-content/uploads/2019/12/marriage-story-ampas-script.pdf")

movie_mapping = c('1' = '1917',
                  '2' = 'Ford V Ferrari',
                  '3' = 'Joker',
                  '4' = 'Parasite',
                  '5' = 'The Irishman',
                  '6' = 'Little Women',
                  '7' = 'Jojo Rabbit',
                  '8' = 'Marriage Story')

scripts_2020 = make_scripts(pdf_urls, movie_mapping)

### 2019 ###

pdf_urls = c("https://assets.scriptslug.com/live/pdf/scripts/bohemian-rhapsody-2018.pdf",
             "https://s3-us-west-2.amazonaws.com/script-pdf/favourite-the-script-pdf.pdf",
             "https://static1.squarespace.com/static/5a1c2452268b96d901cd3471/t/5c2687b74d7a9c2ebbdb95e9/1546028997301/Black+Panther.pdf",
             "https://assets.scriptslug.com/live/pdf/scripts/blackkkklansman-2018.pdf",
             "https://cursosdeguion.com/wp-content/uploads/The-Green-Book-Guion.pdf",
             "https://cursosdeguion.com/wp-content/uploads/Vice.pdf",
             "https://d2bu9v0mnky9ur.cloudfront.net/academy2018/asib/screenplay/asib_wbfomat.pdf",
             "https://deadline.com/wp-content/uploads/2018/12/Roma-Screenplay-ENGLISH.pdf")

movie_mapping = c('1' = 'Bohemian Rhapsody',
                  '2' = 'The Favourite',
                  '3' = 'Black Panther',
                  '4' = 'BlacKkKlansman',
                  '5' = 'Green Book',
                  '6' = 'Vice',
                  '7' = 'A Star is Born',
                  '8' = 'Roma')

scripts_2019 = make_scripts(pdf_urls, movie_mapping)
