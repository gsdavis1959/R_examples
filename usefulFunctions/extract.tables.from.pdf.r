library(rJava)      # Needed for tabulizer
library(tabulizer)  # Handy tool for PDF Scraping
library(tidyverse)  # Core data manipulation and visualization libraries


f2 <- "https://github.com/Coopmeister/data_science_r_projects/blob/master/endangered_species.pdf"


# PDF Scrape Tables
endangered_species_scrape <- extract_tables(
  file   = '~/Data/Datasets/Biology/endangered_species.pdf', 
  method = "decide", 
  output = "data.frame")

# Pluck the first table in the list
endangered_species_scrape_raw_tbl <- endangered_species_scrape %>% 
  pluck(1) %>% 
  as_tibble()

# Show first 6 rows
endangered_species_scrape_raw_tbl %>% head() %>% knitr::kable()


# Get column names from Row 1
col_names <- endangered_species_scrape_raw_tbl %>%
  slice(1) %>%
  pivot_longer(cols = everything()) %>%
  mutate(value = ifelse(is.na(value), "Missing", value)) %>%
  pull(value)

# Overwrite names and remove Row 1
endangered_species_renamed_tbl <- endangered_species_raw_tbl %>%
  set_names(col_names) %>%
  slice(-1)

# Show first 6 rows
endangered_species_renamed_tbl %>% head() %>% knitr::kable()

# not working
endangered_species_final_tbl <- endangered_species_renamed_tbl %>%
  
  # 1. Remove columns with NAs
  select_if(~ !all(is.na(.))) %>%
  
  # 2. Fix columns that were combined
  separate(col  = `Amphibians Fishes Insects`, 
           into = c("Amphibians", "Fishes", "Insects"), 
           sep  = " ") %>%
  
  # 3. Convert to (Tidy) Long Format for visualization
  pivot_longer(cols = -Year, names_to = "species", values_to = "number") %>%
  
  # 4. Fix numeric data stored as character
  mutate(number = str_remove_all(number, ",")) %>%
  mutate(number = as.numeric(number)) %>%
  
  # 5. Convert Character Year & species to Factor
  mutate(Year = as_factor(Year)) %>%
  mutate(species = as.factor(species)) %>%
  
  # 6. Percents by year
  group_by(Year) %>%
  mutate(percent = number / sum(number)) %>%
  mutate(label = scales::percent(percent)) %>%
  ungroup()

# Show first 6 rows
endangered_species_final_tbl %>% head() %>% knitr::kable()

