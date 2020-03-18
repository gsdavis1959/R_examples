# install.packages("devtools")
devtools::install_github("RamiKrispin/coronavirus", force = TRUE)

#==============
# LOAD PACKAGES
#==============

library(tidyverse)
library(sf)
library(rvest)
library(stringr)
library(scales)
library(viridis)
library(coronavirus)
library(extrafont)
library(lubridate)

data("coronavirus") 

head(coronavirus)
tail(coronavirus)




summary_df <- coronavirus %>% group_by(Country.Region, type, Lat, Long) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases)

summary_df %>% head(20)

# summarize by country

by_country <- coronavirus %>% group_by(Country.Region, type) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases)

glimpse(by_country)

# filter only confirmed cases

by_country <- by_country %>% filter(type == 'confirmed')

# rename columns
colnames(by_country) <- c('region', 'type', 'total_cases')
by_country <- as.data.frame(by_country)


#========
# GET MAP
#========

map.world <- map_data('world')
map.world <- subset(map.world, region!="Antarctica")
glimpse(map.world)

#==========================
# CHECK FOR JOIN MISMATCHES
#==========================

anti_join(by_country, map.world)

#=====================
# RECODE COUNTRY NAMES
#=====================

map.world %>%
  group_by(region) %>%
  summarise() %>%
  print(n = Inf)

by_country <- by_country %>%  mutate(country = recode(region, `US` = 'USA'
                                              , `Korea, South` = 'South Korea'
                                              , `United Kingdom` = 'UK'
                                              , `Czechia` = 'Czech Republic'
                                              , `Taiwan*` = 'Taiwan'
                                              , `North Macedonia` = 'Macedonia'
                                              , `Antigua and Barbuda` = 'Antigua'
                                              , `Congo (Kinshasa)` = 'Republic of Congo'
                                              , `Cote d'Ivoire` = 'Ivory Coast'
                                              , `Holy See` = 'Vatican'
                                              , `Trinidad and Tobago` = 'Turks and Caicos Islands'
                                              , `Eswatini` = 'Swaziland'
                                              , `Congo (Brazzaville)` = 'Republic of Congo'
                                              , `Saint Vincent and the Grenadines` = 'Sint Maarten'
                                              , `The Bahamas` = 'Bahamas'
                                              , `occupied Palestinian territory` = 'Palestine'
)
)


#-----------------------
# JOIN DATASETS TOGETHER
#-----------------------

map.covid <- left_join( map.world, by_country, by = c('region' = 'country')) 
map.covid <- map.covid %>% filter(type == 'confirmed')
map.covid <- left_join(map.world, map.covid)
map.covid$total_cases[is.na(map.covid$total_cases)] <- 0
summary(map.covid)




#=====
# PLOT
#=====

# BASIC (this is a first draft)

ggplot(map.covid, aes( x = long, y = lat, group = group )) +
  geom_polygon(aes(fill = total_cases))


#=======================
# FINAL, FORMATTED DRAFT
#=======================



# load fonts
loadfonts(quiet = T)

fonts()



# blues light to dark
#eff3ff','#bdd7e7','#6baed6','#3182bd','#08519c'
#461863','#404E88','#2A8A8C','#7FD157','#F9E53F' - original palette
#1B365D','#003594','#7BA4DB','#8E9FBC','#1B365D - blue palette

ggplot(map.covid, aes( x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = total_cases)) +
  scale_fill_gradientn(colours = c('#eff3ff','#bdd7e7','#6baed6','#3182bd','#08519c')
                       ,values = scales::rescale(c(0,100,3000,10000,60000))
                       ,labels = comma
                       ,breaks = c(0,100,3000,10000,60000)
  ) +
  guides(fill = guide_legend(reverse = T)) +
  labs(fill = 'Confirmed Cases'
       ,title = 'COVID-19 Cases'
       ,subtitle = 'March 17, 2020'
       ,x = NULL
       ,y = NULL) +
  theme(text = element_text(family = 'Gill Sans', color = '#EEEEEE')
        ,plot.title = element_text(size = 28)
        ,plot.subtitle = element_text(size = 14)
        ,axis.ticks = element_blank()
        ,axis.text = element_blank()
        ,panel.grid = element_blank()
        ,panel.background = element_rect(fill = '#333333')
        ,plot.background = element_rect(fill = '#333333')
        ,legend.position = c(.18,.36)
        ,legend.background = element_blank()
        ,legend.key = element_blank()
  ) +
  annotate(geom = 'text'
           ,label = 'Source: RamiKrispin/coronavirus\nhttps://github.com/RamiKrispin/coronavirus'
           ,x = 18, y = -55
           ,size = 3
           ,family = 'Gill Sans'
           ,color = '#CCCCCC'
           ,hjust = 'left'
  )






world_map<- subset(world_map, region!="Antarctica")
ggplot(map.covid2, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill=total_cases))

map.covid %>% filter(region == 'Egypt')
map.world %>% filter(region == 'Egypt')

# Draw plot
ggplot(map.covid, aes(x=reorder(region, -total_cases), y = (total_cases/1000))) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Coronavirus Cases", 
       subtitle="Total Cases by Country", 
       caption="source: ") + 
  theme(axis.text.x = element_text(angle=90, vjust=0.1, hjust=1))




# Forecast

by_day <- coronavirus %>% mutate(day = day(date)) %>% glimpse()

library(prophet)
library(tidyquant)



by_day <- by_day %>% filter(type == 'confirmed',
                            Country.Region == 'China')

by_day <- by_day %>% mutate(cum = cumsum(cases))

cases_by_day <- by_day %>% select(date, cases) %>% glimpse()
cum_by_day <- by_day %>% select(date, cum) %>% glimpse()
                            
colnames(country_by_day) <- c('ds', 'y')
colnames(cum_by_day) <- c('ds', 'y')

tail(cum_by_day)

m <- prophet(cum_by_day)
future <- make_future_dataframe(m, periods = 365)
tail(future)
m



forecast <- predict(m, future)
head(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
glimpse(forecast)



forecast %>%
  ggplot(aes(x = ds, y = yhat)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_point(alpha = 0.5, color = palette_light()[[1]]) +
  geom_smooth() +
  theme_tq()


plot(m, forecast)
prophet_plot_components(m, forecast)
dyplot.prophet(m, forecast)


library(fpp)
library(forecast)
library(expsmooth)

by_day <- by_day %>% select(cum)

data = ts(by_day)
autoplot(forecast(data))

# Fully automated forecasting
plot(forecast(data))
plot(forecast(a10))
plot(forecast(taylor))

fit1 <- ets(data, model="ANN", damped=FALSE)
autoplot(forecast(fit1))
