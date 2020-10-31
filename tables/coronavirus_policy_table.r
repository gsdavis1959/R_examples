knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(DT)
library(RColorBrewer)
library(readxl)
library(lubridate)
library(sessioninfo)

colorBars <- function(df, column, color) {
  df1 <- df %>% formatStyle(column,
                            background = styleColorBar(range(mpg[column]), color),
                            backgroundSize = '100% 50%',
                            backgroundRepeat = 'no-repeat',
                            backgroundPosition = 'center')
  return(df1)
}

covid19_policy <- read_csv("https://raw.githubusercontent.com/parmsam/rstudio-table-contest/main/state_policy_updates_20201022_2346.csv") %>% 
  mutate(website = str_extract(source,"https://.*")) %>%
  mutate(website = str_c('<a  target=_blank href=', website, '>', 'Click Here','</a>' )) %>%
  mutate(date = as_date(date)) %>%
  filter(date > "2020-01-20") 

#filter out any pre-first USA case policy order data b/c entry error on date
state_abbrev <- read_csv("https://raw.githubusercontent.com/parmsam/rstudio-table-contest/main/State%20Abbreviations.csv")

covid19_policy <- covid19_policy %>% left_join(state_abbrev, by="state_id") %>%
  mutate(map = sprintf('<img src="https://raw.githubusercontent.com/CivilServiceUSA/us-states/master/images/maps/%s-small.png" ></img>', str_replace(str_to_lower(state)," ","-"))) %>%
  filter(!state %in% c("District of Columbia","Northern Mariana Islands","Virgin Islands","Guam","Puerto Rico"))

state_covid19_policy <- covid19_policy %>% filter(policy_level == "state") %>% 
  arrange(state_id, date) %>% 
  select(map, state, policy_level, policy_type, start_stop, date, source, website, total_phases, -comments, -county, -fips_code, -state_id, -total_phases)

county_covid19_policy <- covid19_policy %>% filter(policy_level == "county") %>% arrange(state_id, county, date) %>%
  select(map, state, county, policy_level, policy_type, start_stop, date, source, website, total_phases, -comments, -fips_code, -state_id, -total_phases , -source)

state_policy_table <- state_covid19_policy %>% 
  datatable(escape = FALSE, selection = "single", extensions=c("Buttons","ColReorder", "Responsive"), 
            filter = list(position = 'top', clear = FALSE, plain = TRUE), 
            class = 'cell-border stripe',
            options=list(searchHighlight= FALSE, pagelength =5, lengthMenu = list(c(10, 20, 50, -1),c("10","20","50","All")), hover=TRUE, autoWidth = FALSE,
                         dom='tlfrtBip', colReorder = TRUE, keys=TRUE,
                         columnDefs = list(list(searchable = FALSE)),
                         fixedColumns = TRUE,
                         buttons= list('copy', list(textAlign='center',
                                                    extend = 'collection',
                                                    buttons = list(list(extend='csv',filename='mpg_summary'), list(extend='excel',filename='mpg_summary')),
                                                    text = 'Download'
                         )),
                         list(colReorder = TRUE)
            )) %>% 
  formatStyle('policy_type',fontWeight = "normal", color="black", textAlign = 'center',
              backgroundColor = styleEqual(unique(state_covid19_policy$policy_type), rainbow(n_distinct(state_covid19_policy$policy_type)))) %>%
  formatStyle('start_stop',textAlign = 'center', fontWeight = "bold",
              color = styleEqual(unique(state_covid19_policy$start_stop), c("green","red"))) %>%
  formatStyle('policy_level',textAlign = 'center', fontWeight = "bold",
              color = styleEqual(unique(state_covid19_policy$policy_level), c("maroon"))) %>%
  formatStyle('state',textAlign = 'center', fontWeight = "bold",
              color = c("maroon"))

county_policy_table <- county_covid19_policy %>% 
  datatable(escape = FALSE, selection = "single", extensions=c("Buttons","ColReorder", "Responsive"), 
            filter = list(position = 'top', clear = FALSE, plain = TRUE), 
            class = 'cell-border stripe',
            options=list(searchHighlight= FALSE, pagelength =5, lengthMenu = list(c(10, 20, 50, -1),c("10","20","50","All")), hover=TRUE, autoWidth = TRUE,
                         dom='tlfrtBip', colReorder = TRUE, keys=TRUE,
                         columnDefs = list(list(searchable = FALSE)),
                         fixedColumns = TRUE,
                         buttons= list('copy', list(textAlign='center',
                                                    extend = 'collection',
                                                    buttons = list(list(extend='csv',filename='mpg_summary'), list(extend='excel',filename='mpg_summary')),
                                                    text = 'Download'
                         )),
                         list(colReorder = TRUE)
            )) %>% 
  formatStyle('policy_type',fontWeight = "normal", color="black", textAlign = 'center',
              backgroundColor = styleEqual(unique(county_covid19_policy$policy_type), rainbow(n_distinct(county_covid19_policy$policy_type)))) %>%
  formatStyle('start_stop',textAlign = 'center', fontWeight = "bold",
              color = styleEqual(unique(county_covid19_policy$start_stop), c("green","red"))) %>%
  formatStyle('policy_level',textAlign = 'center', fontWeight = "bold",
              color = styleEqual(unique(county_covid19_policy$policy_level), c("steelblue"))) %>%
  formatStyle('county',textAlign = 'center', fontWeight = "bold",
              color = c("steelblue")) %>%
  formatStyle('state',textAlign = 'center', fontWeight = "bold",
              color = c("steelblue"))
state_policy_table
