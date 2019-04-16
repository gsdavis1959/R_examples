#load libraries
library(tidyverse)
library(fs)
library(readxl)

setwd('~/Data/Datasets/Data_Excel')
#import first sheet
read_excel("madrid_temp.xlsx")

#import third sheet
read_excel("madrid_temp.xlsx",3)

# read the names of the sheets
path <- "madrid_temp.xlsx"

path%>%
  excel_sheets()


# read all sheets
mad <- path%>%
  excel_sheets()%>%
  set_names()%>%
  map(read_excel,
      path=path)

str(mad)

# join all sheets
mad <- path%>%
  excel_sheets()%>%
  set_names()%>%
  map_df(read_excel,
         path=path)

mad

# add an id to each sheet
mad <- path%>%
  excel_sheets()%>%
  set_names()%>%
  map_df(read_excel,
         path=path,
         .id="yr2")

str(mad)

# import multiple excel files
dir_ls()
## berlin_temp.xlsx featured.png     index.en.html    index.en.Rmd     
## madrid_temp.xlsx
#we can filter the files that we want
dir_ls(regexp="xlsx") 

#without joining
dir_ls(regexp="xlsx")%>%
  map(read_excel)

#joining with a new id column
dir_ls(regexp="xlsx")%>%
  map_df(read_excel,.id="city")

# function to read sheets
read_multiple_excel <- function(path) {
  path%>%
    excel_sheets() %>% 
    set_names() %>% 
    map_df(read_excel, path = path)
}

#separately
data <- dir_ls(regexp="xlsx") %>% 
  map(read_multiple_excel)

str(data)


#joining all data.frames
data_df <- dir_ls(regexp="xlsx") %>% 
  map_df(read_multiple_excel,
         .id="city")

str(data_df)
