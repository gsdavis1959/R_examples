library(tidyverse)
library(lubridate)
library(rvest)
library(stringdist)
library(plotly)

# Function to read the raw CSV files. The files are aggregated to the country
# level and then converted to long format

clean_jhd_to_long <- function(df) {
  df_str <- deparse(substitute(df))
  var_str <- substr(df_str, 1, str_length(df_str) - 4)
  
  df %>% group_by(`Country/Region`) %>%
    filter(`Country/Region` != "Cruise Ship") %>%
    select(-`Province/State`, -Lat, -Long) %>%
    mutate_at(vars(-group_cols()), sum) %>% 
    distinct() %>%
    ungroup() %>%
    rename(country = `Country/Region`) %>%
    pivot_longer(
      -country, 
      names_to = "date_str", 
      values_to = var_str
    ) %>%
    mutate(date = mdy(date_str)) %>%
    select(country, date, !! sym(var_str)) 
}

confirmed_raw <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
deaths_raw <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")


jh_covid19_data <- clean_jhd_to_long(confirmed_raw) %>%
  full_join(clean_jhd_to_long(deaths_raw))


# Next, I pull official country level indicators from the UN Statstics Division
# to get country level identifiers.

jhd_countries <- tibble(country = unique(jh_covid19_data$country)) %>% arrange(country)

ctry_ids <- read_html("https://unstats.un.org/unsd/methodology/m49/") %>%
  html_table()
un_m49 <- ctry_ids[[1]]
colnames(un_m49) <- c("country", "un_m49", "iso3c")


# Merging by country name is messy. I start with a fuzzy matching approach
# using the {stringdist} package

ctry_names_dist <- matrix(NA, nrow = nrow(jhd_countries), ncol = nrow(un_m49))
for(i in 1:length(jhd_countries$country)) {
  for(j in 1:length(un_m49$country)) { 
    ctry_names_dist[i,j]<-stringdist(tolower(jhd_countries$country[i]), 
                                     tolower(un_m49$country[j]))      
  }  
}

min_ctry_name_dist <- apply(ctry_names_dist, 1, min)

matched_ctry_names <- NULL

for(i in 1:nrow(jhd_countries)) {
  un_m49_row <- match(min_ctry_name_dist[i], ctry_names_dist[i,])
  if (length(which(ctry_names_dist[i,] %in% min_ctry_name_dist[i])) > 1) un_m49_row <- NA
  matched_ctry_names <- rbind(matched_ctry_names,
                              tibble( 
                                jhd_countries_row = i, 
                                un_m49_row = un_m49_row,
                                jhd_ctry_name = jhd_countries$country[i], 
                                un_m49_name = ifelse(is.na(un_m49_row), NA, 
                                                     un_m49$country[un_m49_row])
                              ))
}

# This matches most cases well but some cases need to be adjusted by hand.
# In addition there are two jurisdictions (Kosovo, Taiwan)
# that cannot be matched as they are no 'country' as far as the U.N.
# Statistics Devision is concerned.

# WATCH OUT: The data from JHU is subject to change without notice.
# New countries are being added and names/spelling might change. 
# Also, in the long run, the data provided by the UNSD might change.
# Inspect 'matched_ctry_names' before using the data.

matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Bolivia"] <- 27
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Brunei"] <- 35
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Congo (Brazzaville)"] <- 54
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Congo (Kinshasa)"] <- 64
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "East Timor"] <- 222
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Iran"] <- 109
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Korea, South"] <- 180
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Kosovo"] <- NA
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Moldova"] <- 181
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Russia"] <- 184
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Taiwan*"] <- NA
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Tanzania"] <- 236
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "United Kingdom"] <- 235
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "US"] <- 238
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Venezuela"] <- 243

# Last Step: Match country identifier data and save file (commented out here)
jhd_countries %>% 
  left_join(matched_ctry_names %>% 
              select(jhd_ctry_name, un_m49_row), 
            by = c(country = "jhd_ctry_name")) %>%
  left_join(un_m49 %>% mutate(un_m49_row = row_number()), by = "un_m49_row") %>%
  rename(country = country.x) %>%
  select(country, iso3c)  -> jhd_countries

jh_covid19_data <- jh_covid19_data %>% left_join(jhd_countries) %>%
  select(country, iso3c, date, confirmed, deaths)

write_csv(jh_covid19_data, sprintf("jh_covid19_data_%s.csv", Sys.Date()))

# world bank

library(wbstats)

pull_worldbank_data <- function(vars) {
  new_cache <- wbcache()
  all_vars <- as.character(unique(new_cache$indicators$indicatorID))
  data_wide <- wb(indicator = vars, mrv = 10, return_wide = TRUE)
  new_cache$indicators[new_cache$indicators[,"indicatorID"] %in% vars, ] %>%
    rename(var_name = indicatorID) %>%
    mutate(var_def = paste(indicator, "\nNote:",
                           indicatorDesc, "\nSource:", sourceOrg)) %>%
    select(var_name, var_def) -> wb_data_def
  new_cache$countries %>%
    select(iso3c, iso2c, country, region, income) -> ctries
  left_join(data_wide, ctries, by = "iso3c") %>%
    rename(year = date,
           iso2c = iso2c.y,
           country = country.y) %>%
    select(iso3c, iso2c, country, region, income, everything()) %>%
    select(-iso2c.x, -country.x) %>%
    filter(!is.na(NY.GDP.PCAP.KD),
           region != "Aggregates") -> wb_data
  wb_data$year <- as.numeric(wb_data$year)
  wb_data_def<- left_join(data.frame(var_name = names(wb_data),
                                     stringsAsFactors = FALSE),
                          wb_data_def, by = "var_name")
  wb_data_def$var_def[1:6] <- c(
    "Three letter ISO country code as used by World Bank",
    "Two letter ISO country code as used by World Bank",
    "Country name as used by World Bank",
    "World Bank regional country classification",
    "World Bank income group classification",
    "Calendar year of observation"
  )
  wb_data_def$type = c("cs_id", rep("factor",  4), "ts_id",
                       rep("numeric", ncol(wb_data) - 6))
  return(list(wb_data, wb_data_def))
}

vars <- c("SP.POP.TOTL", "AG.LND.TOTL.K2", "EN.POP.DNST", "EN.URB.LCTY", "SP.DYN.LE00.IN", "NY.GDP.PCAP.KD")
wb_list <- pull_worldbank_data(vars)
wb_data <- wb_list[[1]]
wb_data_def <- wb_list[[2]]

wb_data %>%
  group_by(iso3c) %>%
  arrange(iso3c, year) %>%
  summarise(
    population = last(na.omit(SP.POP.TOTL)),
    land_area_skm = last(na.omit(AG.LND.TOTL.K2)),
    pop_density = last(na.omit(EN.POP.DNST)),
    pop_largest_city = last(na.omit(EN.URB.LCTY)),
    gdp_capita = last(na.omit(NY.GDP.PCAP.KD)),
    life_expectancy = last(na.omit(SP.DYN.LE00.IN))
  ) %>% left_join(wb_data %>% select(iso3c, region, income) %>% distinct()) -> wb_cs

write_csv(wb_data, "jh_add_wbank_data.csv")

head(jh_covid19_data)
tail(jh_covid19_data)
head(wb_cs)

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(gghighlight)
  library(ggrepel)
})

dta2 <- read_csv(
  "jh_covid19_data_2020-04-18.csv",
  col_types = cols()
) %>%
  mutate(date = ymd(date))

wb_cs2 <- read_csv(
  "jh_add_wbank_data.csv", 
  col_types = cols()
)

# I define event time zero where, for a given country, the confirmed
# cases match or exceed the Chinese case number at the beginning of the
# data so that all countries can be compared across event time.
# Also a require each country to have at least 7 days post event day 0

dta2 %>% 
  group_by(country) %>%
  filter(confirmed >= min(dta2$confirmed[dta2$country == "China"])) %>%
  summarise(edate_confirmed = min(date)) -> edates_confirmed



dta2 %>% 
  left_join(edates_confirmed, by = "country") %>%
  mutate(
    edate_confirmed = as.numeric(date - edate_confirmed)
  ) %>%
  filter(edate_confirmed >= 0) %>%
  group_by(country) %>%
  filter (n() >= 7) %>% 
  ungroup() %>%
  left_join(wb_cs, by = "iso3c") %>% 
  mutate(
    confirmed_1e5pop = 1e5*confirmed/population
  ) -> df

lab_notes <- paste0(
  "Data as provided by Johns Hopkins University Center for Systems Science ", 
  "and Engineering (JHU CSSE)\nand obtained on April 12, 2020. ",
  "The sample is limited to countries with at least seven days of positive\n", 
  "event days data. Code and walk-through: https://joachim-gassen.github.io."
)

lab_x_axis_confirmed <- sprintf(paste(
  "Days since confirmed cases matched or exceeded\n", 
  "initial value reported for China (%d cases)\n"
), min(dta2$confirmed[dta2$country == "China"]))

gg_my_blob <- list(
  scale_y_continuous(trans='log10', labels = scales::comma),  
  theme_minimal(), 
  theme(
    plot.title.position = "plot", 
    plot.caption.position =  "plot",
    plot.caption = element_text(hjust = 0),
    axis.title.x = element_text(hjust = 1),
    axis.title.y = element_text(hjust = 1),
  ),
  labs(caption = lab_notes,
       x = lab_x_axis_confirmed,
       y = "Confirmed cases (logarithmic scale)"),
  gghighlight(TRUE,  label_key = country, use_direct_label = TRUE,
              label_params = list(segment.color = NA, nudge_x = 1))
)

# cases
ggplot(df %>% filter (edate_confirmed <= 30), 
       aes(x = edate_confirmed, color = country, y = confirmed)) +
  geom_line() +
  labs(
    title = "Focus on the first month: Confirmed Cases - as of 4/18/2020\n"
  ) +
  gg_my_blob

# deaths
ggplot(df %>% filter (edate_confirmed <= 30), 
       aes(x = edate_confirmed, color = country, y = deaths)) +
  geom_line() +
  labs(
    title = "Focus on the first month: Deaths - as of 4/18/2020\n"
  ) +
  gg_my_blob

glimpse(df)

df2 <- left_join(wb_cs2, dta2, by = 'country')


grp <- df2 %>% group_by(country) %>%
  filter(confirmed == max(confirmed)) %>%
  arrange(country)

grp <- as.data.frame(grp) %>% glimpse()

grp <- distinct(grp, country, .keep_all = TRUE) %>% glimpse()

library(xlsx)
efi <- read.xlsx('economic_freedom_index2019_data.xlsx', 1, stringsAsFactors = FALSE) %>% glimpse()
names(efi)
efi <- as.data.frame(efi) %>% glimpse()
efi$X2019.Score <- as.numeric(efi$X2019.Score)
efi$Fiscal.Health <- as.numeric(efi$Fiscal.Health)
str(efi)
efi <- efi %>% rename(country = Country.Name) %>% glimpse()

dtrs <- read.csv('Doctors_Per_Capital_By_Country.csv', stringsAsFactors = FALSE) %>% glimpse()
dtrs <- dtrs %>% filter(TIME == '2017')
dtrs$Value <- as.numeric(dtrs$Value)
dtrs <- dtrs %>% rename(iso3c.x = ï..LOCATION) %>% glimpse()


mstr <- left_join(grp, efi, by = 'country', keep_all = FALSE) %>% glimpse()
mstr <- left_join(mstr, dtrs, by = 'iso3c.x')




# https://data.world/covid-19-data-resource-hub/covid-19-case-counts/workspace/file?filename=COVID-19+Cases.csv
dw <- read.csv('COVID-19 Cases.csv', stringsAsFactors = FALSE)
head(dw)
tail(dw)

dw_filter <- dw %>% filter(Country_Region == 'US')

dw_filter <- dw_filter %>% mutate(cum_cases = cumsum(Cases))


# forcast based on johns hopikins recent data
library(fpp)
library(forecast)

cnty_df <- dta2 %>% filter(iso3c == 'USA')
ts_df <- cnty_df %>% select(deaths)

data = ts(ts_df)
head(data)
tail(data)
autoplot(forecast(data))


# Fully automated forecasting
plot(forecast(data))
plot(forecast(a10))
plot(forecast(taylor))

# other datasets
# https://www.kaggle.com/nightranger77/covid19-demographic-predictors
cv19_by_cntry <- read.csv('covid19_by_country.csv', stringsAsFactors = FALSE) %>% glimpse()
head(cv19_by_cntry)
tail(cv19_by_cntry)
cv19_by_cntry <- cv19_by_cntry %>% rename(country = Country) %>% glimpse()
mstr <- left_join(mstr, cv19_by_cntry, by = 'country')

df_for_model <- select_if(mstr, is.numeric)
df_for_model <- na.omit(df_for_model) %>% glimpse()

ggplot(df_for_model, aes(x=confirmed, y=deaths)) + geom_point()
ggplot(df_for_model, aes(x=Median.Age, y=deaths)) + geom_point()

predictors <- df_for_model %>% select(deaths,
                                      X2019.Score,
                                      Density,
                                      Urban.Pop,
                                      Hospital.Bed,
                                      lung, Median.Age)
summary(predictors)
library(corrplot)
r <- cor(predictors)
corrplot(r)

model <- glm(deaths ~ ., data = predictors)
summary(model)
names(p)
p <- predict(model, data = predictors)
print(p)

compare <- cbind(p, predictors)
table(compare)
ggplot(compare, aes(x=deaths, y=p)) + geom_point()


library(readr)
urlfile="https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"

mydata<-read_csv(url(urlfile))


head(mydata)

s <- mydata %>% group_by(state) %>%
  filter(cases == max(cases)) %>%
  arrange(state)

s <- as.data.frame(s) %>% glimpse()

s <- distinct(s, country, .keep_all = TRUE) %>% glimpse()


library(usmap)
library(ggplot2)


states_df <- usmap::us_map()

plot_usmap(regions = "states") + 
  labs(title = "US States",
       subtitle = "This is a blank map of the states of the United States.") + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue"))

s <- left_join(states_df, s, by = 'fips')
head(s)

sub_s <- s %>% select(fips, abbr, full, cases, deaths)
sub_s <- na.omit(sub_s)

eq_transformed <- usmap_transform(s)

plot_usmap(data = sub_s, values = "deaths", color = "red") + 
  scale_fill_continuous(name = "COVID-19 Cases", label = scales::comma) + 
  theme(legend.position = "right")

head(statepop)

# keras model

library(keras)
library(tensorflow)
install_tensorflow()

# Create train test split
sample_rows <- sample(nrow(predictors), nrow(predictors) * 0.8)
# Create the training dataset
train <- predictors[sample_rows, ]
head(train)
train <- na.omit(train)
# Create the test dataset
test <- predictors[-sample_rows, ]

train_target <- train %>% select(deaths) %>% glimpse()
train_predictors <- train %>% select(-deaths) %>% glimpse()
train_target <- as.matrix(train_target)
train_predictors <- as.matrix(train_predictors)

test_target <- test %>% select(deaths) %>% glimpse()
test_predictors <- test %>% select(-deaths) %>% glimpse()
test_target <- as.matrix(test_target)
test_predictors <- as.matrix(test_predictors)


model2 <- keras_model_sequential()
model2 %>%
  layer_dense(units = ncol(train_target), activation = 'softmax',
              input_shape = ncol(train_predictors))
summary(model2)

# Get model configuration
get_config(model2)

get_layer(model2, index = 1)

model2$layers

model2$inputs

model2$output



build_model2 <- function() {
  
  model2 <- keras_model_sequential() %>%
    layer_dense(units = 16, activation = "relu",
                input_shape = ncol(train_predictors)) %>%
    layer_dense(units = 1, activation = "relu")
  
  # Compile the model
  model2 %>% compile(
    loss = 'mse',
    optimizer = 'adam',
    metrics = 'accuracy'
  )
  
  model2
}

model2 <- build_model2()
model2 %>% summary()


# Display training progress by printing a single dot for each completed epoch.
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)    

epochs <- 600



# The patience parameter is the amount of epochs to check for improvement.
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)

# Fit the model and store training stats
history <- model2 %>% fit(
  train_predictors,
  train_target,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(print_dot_callback)
)

plot(history)


c(loss, mae) %<-% (model2 %>% evaluate(test_predictors, test_target, verbose = 0))

paste0("Mean absolute error on test set: ", sprintf("%.2f", mae))


test_predictions <- model2 %>% predict(train_predictors)
test_predictions


r <- lm(deaths ~ ., data = train)
summary(r)
p <- predict(r, newdata = test)
p



# regression tree
library(party) # For ctree.plot
library(partykit) # For conversion
library(rpart)


fit.r <- rpart(deaths ~ ., data=predictors)

plot(fit.r) # Draw rtree plot
text(fit.r, use.n=TRUE, cex=.75) # Add text

# Next we will convert the display to ctree style.
# Please note that this only changes the way it is stored, not the actual tree


plot(as.party(fit.r))

# random forest
library(randomForest)

# Create a Random Forest model with default parameters
model1 <- randomForest(deaths ~ ., data = train, importance = TRUE)
model1

# Predicting on train set
predTrain <- predict(model1, train, type = "class")
head(predTrain)
# Checking classification accuracy
table(predTrain, train$deaths)  

# To check important variables
importance(model1)       
varImpPlot(model1) 


# https://data.world/markmarkoh/coronavirus-data/workspace/file?filename=full_data.csv
new <- read.csv('full_data.csv', stringsAsFactors = FALSE)
head(new)
tail(new)

location_list <- c('United States', 'Italy', 'Spain', 'China', 'World')

new_filtered <- new %>% filter(location == location_list)

library(apexcharter)

apex(data = new_filtered, type = "line", mapping = aes(x = date, y = new_cases, color = factor(location))) %>% 
  ax_stroke(width = 2)

library(plotly)

# multiple traces (less performant, but more interactive)
plot_ly(new_filtered, x = ~date, y = ~new_cases) %>%
  add_lines(color = ~ordered(location))

# cv19_by_country
cv19_filtered <- cv19_by_cntry %>% filter(country == location_list)

plot_ly(cv19_by_cntry, x = ~Total.Infected, y = ~Tests)

# plotly world map

df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')
glimpse(df)
names(mstr)
names(new)
new <- new %>% rename(country = `location`)
jhd_countries <- jhd_countries %>%  mutate(country = recode(country,`US` = 'United States'
)
)



for_map <- left_join(jhd_countries, new) %>% glimpse()

for_map <- for_map %>% group_by(country) %>% 
  mutate(all_cases = max(total_cases))

# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

fig <- plot_geo(for_map)
fig <- fig %>% add_trace(
  z = ~total_cases, color = ~total_cases, colors = 'Blues',
  frame = ~date,
  text = ~country, locations = ~iso3c, marker = list(line = l)
)
fig <- fig %>% colorbar(title = 'COVID-19 Total Cases')
fig <- fig %>% layout(
  title = 'April 18 2020 Global COVID-19',
  geo = g
)

fig
