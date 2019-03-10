# load the libraries
library(dplyr) # Data wrangling, glimpse(75) and tbl_df().

library(ggplot2) # Visualise data.
library(lubridate) # Dates and time.
library(readr) # Efficient reading of CSV data.
library(stringr) # String operations.
library(tibble) # Convert row names into a column.
library(tidyr) # Prepare a tidy dataset, gather().
library(magrittr) # Pipes %>%, %T>% and equals(), extract().
library(tidyverse) # all tidyverse packages
library(mosaic) # favstats and other summary functions
library(fs) # file management functions
library(forcats) # manipulate data for visualization
library(cowplot) # visualization
library(stringi) # more strings
library(tidytext) # tidying text data for analysis
library(maps) # maps for us.cities
# load data
Abc7 <- read_csv("News/abc7ny.csv")
Kcra <- read_csv("News/kcra.csv")
# look
Abc7 %>% glimpse(78)
Kcra %>% glimpse(78)
# check to see if the columns are the same
base::identical(names(Abc7), names(Kcra))

# check to see if you are using the right method to merge the two dataframes
# test 
dplyr::bind_rows(Abc7, Kcra, .id = "data_id") %>% 
  dplyr::count(data_id)
# assign
NewsData <- dplyr::bind_rows(Abc7, Kcra, .id = "data_id")
# verify
NewsData %>% 
  dplyr::count(data_id)
NewsData %>% 
  glimpse(75)
# check the headlines
headlines_var <- NewsData %>% 
  dplyr::select(headline) %>% 
  Matrix::head(5) 
# is headline character vector?
base::is.character(headlines_var)
base::typeof(headlines_var)

# need to unlist the list
headlines_var <- headlines_var %>% base::unlist() 
base::is.character(headlines_var)

headlines_var

# remove characters
base::sub(pattern = "-", replacement = " ", x = headlines_var)
base::gsub(pattern = "-", replacement = " ", x = headlines_var)
# create a long character vector
base::paste(headlines_var, collapse = "; ")
base::paste0(headlines_var, sep = "", collapse = "; ")
# print without quotes
base::noquote(headlines_var)
base::cat(headlines_var, sep = ", ")
# convert to lower case
NewsData %>% 
  dplyr::mutate(headline_low = stringr::str_to_lower(headline)) %>% 
  dplyr::select(headline, headline_low) %>% 
  head(5)
# convert to title case
NewsData %>% 
  dplyr::mutate(headline_title = stringr::str_to_title(headline)) %>% 
  dplyr::select(headline, headline_title) %>% 
  head(5)
# count the first 3 words in the headline
# test
NewsData %>% 
  dplyr::mutate(teaser_3_words = stringr::word(NewsData$teaser, 1, 3)) %>% 
  count(teaser_3_words, sort = TRUE) %>% 
  head(10)
NewsData <- NewsData %>% 
  dplyr::mutate(teaser_3_words = stringr::word(NewsData$teaser, 1, 3))
NewsData %>% 
  dplyr::count(teaser_3_words, sort = TRUE) %>% 
  utils::head(10)
NewsData %>% 
  dplyr::distinct(teaser_3_words) %>% 
  base::nrow()

# add count columns and arrange in dataframe
NewsData <- NewsData %>% 
  dplyr::add_count(teaser_3_words) %>% # count this variable and add it
  dplyr::arrange(desc(n)) %>% # arrange the new n data with largest on top
  dplyr::rename(tease_3rd_count = n) %>%  # get rid of n variable
  dplyr::group_by(data_id) %>% # collapse the data frame by news feed
  dplyr::add_tally() %>% # add the total count 
  dplyr::ungroup() %>% # expand the data to all variables again
  dplyr::rename(newsfeed_n = n) %>% # rename n to newsfeed_n
  dplyr::mutate(tease_3rd_prop = tease_3rd_count/newsfeed_n, # create prop
                data_id_fct = factor(data_id, # create factor for ID
                                     levels = c(1, 2),
                                     labels = c("Abc7", 
                                                "Kcra")),
                teaser_3_words = factor(teaser_3_words)) 
NewsData %>% 
  dplyr::glimpse(75)

NewsData %>% 
  dplyr::arrange(desc(tease_3rd_prop)) %>% # sort desc by the proportion
  dplyr::filter(tease_3rd_count >= 50) %>% # only keep frequecies above 50
  dplyr::filter(!is.na(teaser_3_words)) %>% # remove missing
# Make the plot
  ggplot2::ggplot(aes(x = tease_3rd_prop, # plot the prop
                      y = fct_reorder(teaser_3_words, # reorder words
                                      tease_3rd_count), # by counts
                      fill = data_id_fct,
                      group = teaser_3_words)) + # fill by feed
  ggplot2::geom_segment(aes(yend = teaser_3_words), 
                        xend = 0, 
                        color = "grey50") +
  ggplot2::geom_point(size = 3, 
                      aes(color = data_id_fct), 
                      show.legend = FALSE) + 
  ggplot2::facet_wrap( ~ data_id_fct, # arrange in 2 columns
                       nrow = 2, 
                       scales = "free", # free scales
                       labeller = as_labeller( # add custom lables
                         c(`Abc7` = "ABC7NY News Feed", 
                           `Kcra` = "KCRA Sacramento News Feed"))) +
  ggplot2::scale_x_continuous(labels = scales::percent) + # use %
  ggplot2::theme(strip.text.x = element_text(face = "bold")) + # bold type
  ggplot2::labs( # labels and captions
    x = "Percent First Three Words Appeared", 
    y = NULL,
    caption = "These are the first three words from headline teasers 
            appearing in ABC7NY and KCRA (from Sacramento)",
    title = "TEAS3RS - Trump, Weather, Police",
    subtitle = "The First Three Words From News Headlines Teasers") 

# us maps using joins
UsCity <- maps::us.cities
UsCity %>% glimpse(75)

UsCity %>% dplyr::distinct(name) %>% base::nrow()

# get numbers
stringr::str_extract_all(string = headlines_var, pattern = "[\\d]")
# replace the state name
UsCity <- UsCity %>% 
  mutate(city_id = stringr::str_replace_all(string = UsCity$name, 
                                            pattern = UsCity$country.etc, 
                                            replacement = ""),
         city_id = stringr::str_trim(city_id),
         city_id = stringr::str_to_lower(city_id))
UsCity %>% glimpse(75)

# strip headlines for state names
city_id_vec <- unlist(UsCity$city_id)
city_id_vec <- paste(city_id_vec, sep = "", collapse = " | ")
city_id_vec

# double check to make lower case
NewsData <- NewsData %>% 
  mutate(headline = stringr::str_to_lower(headline))
NewsData %$% head(headline, 1)
# see which headlines in NewsData contains cities from city_id
MapNewsData <- NewsData %>% 
  filter(stringr::str_detect(string = headline, 
                             pattern = city_id_vec)) %>% 
  dplyr::select(headline, 
                dplyr::everything())
MapNewsData %>% dplyr::glimpse(75)
# Need to join back to UsCity, I need to create a city_idvariable in MapNewsData
MapNewsData <- MapNewsData %>% 
  mutate(city_id = str_extract(string = MapNewsData$headline, 
                               pattern = paste(city_id_vec, collapse = "|")),
         city_id = stringr::str_trim(city_id)) 
MapNewsData %>% 
  count(city_id, sort = TRUE) %>% 
  head(5)

# join the dataframes
dplyr::inner_join(MapNewsData, UsCity, by = "city_id") %>% 
  utils::head(20) %>% 
  dplyr::count(city_id, name) %>% 
  tidyr::spread(name, n)
MapNewsData <- dplyr::inner_join(MapNewsData, UsCity, by = "city_id")
# clean the dates
# test
tidyr::separate(MapNewsData, 
                datetime, 
                into = c("month", "day", "year", "at", "hour", "min")) %>% 
  dplyr::select(headline,
                month:min,
                dplyr::everything()) %>% 
  dplyr::glimpse(75)

tidyr::separate(MapNewsData, 
                datetime, 
                into = c("month", 
                         "day", 
                         "year", 
                         "at", 
                         "hour", 
                         "min")) %>% # I know this works
  dplyr::mutate( # new variables
    month = match(month, month.name), # month.name is loaded with R
    day = as.numeric(day), # make numeric day
    year = as.numeric(year), # # make numeric year
    am_or_pm = stringr::str_sub(min, 
                                start = 3, 
                                end = 4),  # break up min from AM/PM
    hour = if_else(am_or_pm %in% "PM", # condition for 24H time
                   as.numeric(hour) + 12, # 24 hour
                   as.numeric(hour)),
    minute = stringr::str_sub(min, 
                              start = 1, # get the minute elemets
                              end = 2),
    min = as.numeric(minute), # format the minutes
    date = lubridate::make_date(
      year = year,
      month = month,
      day = day), 
    datetime = lubridate::make_datetime( # create the datetime
      year = year, 
      month = month, 
      day = day, 
      hour = hour, 
      min = min)) %>% 
  dplyr::glimpse(75)
# create data so we can look at the words over time
MapNewsData <- MapNewsData %>% 
  tidyr::separate(datetime, 
                  into = c("month", 
                           "day", 
                           "year", 
                           "at", 
                           "hour", 
                           "min")) %>% # I know this works
  dplyr::mutate( # new variables
    month = match(month, month.name), # month.name is loaded with R
    day = as.numeric(day), # make numeric day
    year = as.numeric(year), # # make numeric year
    am_or_pm = str_sub(min, 
                       start = 3, 
                       end = 4),  # break up min from AM/PM
    hour = if_else(am_or_pm %in% "PM", # condition for 24H time
                   as.numeric(hour) + 12, # 24 hour
                   as.numeric(hour)),
    minute = str_sub(min, 
                     start = 1, # get the minute elemets
                     end = 2),
    min = as.numeric(minute), # format the minutes
    date = make_date(
      year = year,
      month = month,
      day = day),
    datetime = lubridate::make_datetime( # create the datetime
      year = year, 
      month = month, 
      day = day, 
      hour = hour, 
      min = min))
# create data subset
MapNewsData_Prop_Data <- MapNewsData %>% 
  arrange(desc(tease_3rd_prop)) %>% 
  dplyr::select(data_id_fct, 
                tease_3rd_prop,
                teaser_3_words,
                datetime) 
MapNewsData_Prop_Data %>% glimpse(75)
# plot
# create base line plot
MapNewsLinePlot <- MapNewsData_Prop_Data %>%
  ggplot2::ggplot(aes(x = datetime, 
                      y = tease_3rd_prop,
                      color = data_id_fct,
                      label = teaser_3_words)) + 
  ggplot2::geom_line(aes(group = data_id_fct)) 
MapNewsLinePlot

# get colors in this plot
MapNewsLinePlotData <- ggplot2::ggplot_build(MapNewsLinePlot)$data[[1]]
MapNewsLinePlotData %>% distinct(colour)

MapNewsData_Prop_Plot <- MapNewsLinePlot +
  ggplot2::geom_text(data = filter(MapNewsData,
                                   tease_3rd_prop >= 0.020 &
                                     datetime <= "2017-11-15"),
                     aes(label = teaser_3_words),
                     vjust = 2,
                     hjust = 1,
                     show.legend = FALSE,
                     color = "#F8766D") +
  ggplot2::geom_text(data = filter(MapNewsData,
                                   tease_3rd_prop >= 0.020 &
                                     datetime >= "2017-11-15" & 
                                     datetime <= "2017-12-7"),
                     aes(label = teaser_3_words),
                     vjust = 0.7,
                     hjust = 0.9,
                     show.legend = FALSE,
                     color = "#F8766D") +
  ggplot2::geom_text(data = filter(MapNewsData,
                                   tease_3rd_prop >= 0.020 &
                                     datetime >= "2017-12-7"),
                     aes(label = teaser_3_words),
                     vjust = 2,
                     hjust = 0.09,
                     show.legend = FALSE,
                     color = "#F8766D") +
  ggplot2::geom_text(data = filter(MapNewsData,
                                   tease_3rd_prop > 0.015 &
                                     datetime <= "2017-09-01"),
                     aes(label = teaser_3_words),
                     vjust = 0.9,
                     hjust = 0.1,
                     show.legend = FALSE,
                     color = "#00BFC4") +
  ggplot2::geom_text(data = filter(MapNewsData,
                                   tease_3rd_prop > 0.015 &
                                     datetime >= "2017-11-01" & 
                                     data_id_fct == "Kcra"),
                     aes(label = teaser_3_words),
                     vjust = 0.9,
                     hjust = 0.1,
                     show.legend = FALSE,
                     color = "#00BFC4") +
  ggplot2::theme(legend.position = "top") +
  ggplot2::labs(x = "Date",
                y = NULL,
                color = "News Feed",
                caption = "Proportion First Three Words Appeared") + 
  ggplot2::ggtitle("ABC7 & KCRA Teaser First Three Word Occurrences July 18 to January 16")
MapNewsData_Prop_Plot
# save file
ggsave("MapNewsData_Prop_Plot.png", width = 7, height = 5, units = "in")
