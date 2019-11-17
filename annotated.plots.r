library(tidyverse)
library(lubridate) # format dates 
library(janitor) # cleaning things

dogs <- read_csv("~/Data/Datasets/History/Dogs-Database.csv")
flights <- read_csv("~/Data/Datasets/History/Flights-Database.csv")

dogs_tidy <- dogs %>% 
  # clean names to snake_case
  clean_names() 

dogs_tidy <- dogs_tidy %>%
  # flights are recorded on same row - put on separate rows to make it 'tidy'
  separate_rows(flights, sep = ",") 

dogs_tidy <- dogs_tidy %>%
  # format data
  mutate(date_flight = ymd(flights),
         # from fate variable extract the date if dog died
         date_death = case_when(str_sub(fate, 1, 4) == "Died" ~ str_sub(fate, 6, 15)),
         date_death = ymd(date_death),
         # if dog died on flight then set flight_fate to Died
         flight_fate = case_when(date_flight == date_death ~ "Died",
                                 TRUE ~ "Survived")) 

dogs_tidy <- dogs_tidy %>%
  select(-notes, everything(), -fate)


glimpse(dogs_tidy)
glimpse(dogs_tidy)

flights_tidy <- flights %>% 
  clean_names() %>% 
  select(date_flight = date, rocket, altitude_km, result, notes_flight = notes) %>% 
  mutate(altitude = case_when(str_detect(altitude_km, "^[0-9]") ~ parse_number(altitude_km)))

glimpse(flights_tidy)

all_dogs_flights <- dogs_tidy %>% 
  inner_join(flights_tidy, by = "date_flight") %>% 
  mutate(flight_year = year(date_flight)) %>% 
  arrange(date_flight, name_latin) %>% 
  group_by(flight_year) %>% 
  mutate(year_pos = row_number())

glimpse(all_dogs_flights)

library(ggforce)
library(ggtext)

p <- ggplot(all_dogs_flights, aes(x = year_pos, y = flight_year)) +
  geom_line(aes(group = date_flight), colour = "white", size = 1.5) +
  geom_point(aes(fill = flight_fate), shape = 21, colour = "white", size = 4.5, stroke = 1.5) +
  scale_y_reverse(breaks = seq(1951, 1966, 1)) +
  scale_fill_manual(values = c("Survived" = "#E69F00", "Died" = "#CC79A7")) +
  labs(y = "", x = "", 
       fill = "Each dot represents a dog and its fate on a mission\nDogs on the same flight are connected by a line",
       title = "Soviet Space Dogs",
       subtitle = "Dogs sent on sub-orbital and orbital space flights by the\nSoviet Space Program in the 1950s and 1960s",
       caption = "Source: @DuncanGeere | Graphic: @committedtotape") +
  theme(plot.background = element_rect(fill = "#383854", colour = "#383854"),
        panel.background = element_rect(fill = "#383854", colour = "#383854"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(colour = "white", family = "Space Mono"),
        axis.text = element_text(colour = "white"),
        axis.text.x = element_blank(),
        plot.title = element_text(face = "bold", size = 20),
        plot.margin = margin(10,20,10,10),
        legend.background = element_rect(fill = "#383854", colour = "white"),
        legend.key = element_rect(fill = "#383854", colour = "#383854"),
        legend.direction = "horizontal",
        legend.position = c(0.5, 0.2))  
p

p1 <- p +
  geom_mark_circle(aes(filter = name_latin == 'Laika', label = 'Laika - 3 November 1957', 
                       description = "The 1st living creature in orbit, never expected to survive"),
                   label.family = "Space Mono",
                   label.fontsize = 10,
                   label.colour = c("#CC79A7", "white"),
                   label.fill = NA,
                   label.buffer = unit(1, 'mm'),
                   con.colour = "white",
                   colour = NA,
                   con.type = "straight",
                   con.cap = 0) +
  geom_mark_ellipse(aes(filter = date_flight == as.Date("1951-07-29"), 
                        label = 'Dezik and Lisa - 29 July 1951', 
                        description = "The 1st deaths, due to parachute failure"),
                    label.family = "Space Mono",
                    label.fontsize = 10,
                    label.colour = c("#CC79A7", "white"),
                    label.fill = NA,
                    label.buffer = unit(1, 'mm'),
                    con.colour = "white",
                    colour = NA,
                    con.type = "straight",
                    con.cap = 0) +
  geom_mark_ellipse(aes(filter = date_flight == as.Date("1960-08-19"), 
                        label = 'Belka and Strelka - 19 August 1960', 
                        description = "Spent a day in space and safely returned to earth"),
                    label.family = "Space Mono",
                    label.fontsize = 10,
                    label.colour = c("#E69F00", "white"),
                    label.fill = NA,
                    label.buffer = unit(1, 'mm'),
                    con.colour = "white",
                    colour = NA,
                    con.type = "straight",
                    con.cap = 0)

p1

p1 +
  geom_point(data = filter(all_dogs_flights, name_latin == 'Kusachka / Otvazhnaya'), 
             shape = 1, colour = "#64DCF4", size = 4.5, stroke = 2) +
  geom_richtext(aes(x = 4.5, y = 1959, 
                    label = "<span style='color:#64DCF4'>**Otvazhnaya ('Brave One')**</span> made the most flights of any space dog"
  ),
  fill = NA, label.color = NA,
  label.padding = grid::unit(rep(0, 4), "pt"),
  hjust = 0, family = "Space Mono", 
  color = "white", size = 3.3)
