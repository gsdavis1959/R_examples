library(tidyverse)
library(nycflights13)
library(gapminder)
library(Lahman)
library(stringr)
library(forcats)
# tidyverse_update()
# simple plot
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
# add colors for the class of car
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))
# use size for the class of car
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))
#> Warning: Using size for a discrete variable is not advised.
# Left
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

# Right
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

# use blue
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")
# facet by class
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)
# facet two variable
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)
# different uses of geom
# left
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

# right
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))
# showing different geoms and line lengths
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

ggplot(data = mpg) +
  geom_smooth(
    mapping = aes(x = displ, y = hwy, color = drv),
    show.legend = FALSE
  )
# points and lines on the same graph
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))
# putting it together with colors
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()
# add filtering
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)
# bar chart
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))
# box plot
ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = mean
  )
# add fill
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))
# stacked bar
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))
# different fills
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "identity")
ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "identity")
# bar total
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")
# change the position of the stack
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")
# use jitter
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")
# change the coordinates
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()
# use a map
us <- map_data("usa")

ggplot(us, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

ggplot(us, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()
# radar charts
bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()


# error bars
source('~/Data/RStatistics/WindowsRSource/Useful Functions/summarySE.r')
tg <- ToothGrowth
head(tg)
# summarySE provides the standard deviation, standard error of the mean, and a (default 95%) confidence interval
tgc <- summarySE(tg, measurevar="len", groupvars=c("supp","dose"))
tgc

# Standard error of the mean
ggplot(tgc, aes(x=dose, y=len, colour=supp)) + 
  geom_errorbar(aes(ymin=len-se, ymax=len+se), width=.1) +
  geom_line() +
  geom_point()


# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.1) # move them .05 to the left and right

ggplot(tgc, aes(x=dose, y=len, colour=supp)) + 
  geom_errorbar(aes(ymin=len-se, ymax=len+se), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd)


# Use 95% confidence interval instead of SEM
ggplot(tgc, aes(x=dose, y=len, colour=supp)) + 
  geom_errorbar(aes(ymin=len-ci, ymax=len+ci), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd)

# Black error bars - notice the mapping of 'group=supp' -- without it, the error
# bars won't be dodged!
ggplot(tgc, aes(x=dose, y=len, colour=supp, group=supp)) + 
  geom_errorbar(aes(ymin=len-ci, ymax=len+ci), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3)
ggplot(tgc, aes(x=dose, y=len, colour=supp, group=supp)) + 
  geom_errorbar(aes(ymin=len-se, ymax=len+se), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
  xlab("Dose (mg)") +
  ylab("Tooth length") +
  scale_colour_hue(name="Supplement type",    # Legend label, use darker colors
                   breaks=c("OJ", "VC"),
                   labels=c("Orange juice", "Ascorbic acid"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("The Effect of Vitamin C on\nTooth Growth in Guinea Pigs") +
  expand_limits(y=0) +                        # Expand y range
  scale_y_continuous(breaks=0:20*4) +         # Set tick every 4
  theme_bw() +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0))               # Position legend in bottom right

# bar charts
# Use dose as a factor rather than numeric
tgc2 <- tgc
tgc2$dose <- factor(tgc2$dose)

# Error bars represent standard error of the mean
ggplot(tgc2, aes(x=dose, y=len, fill=supp)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=len-se, ymax=len+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))


# Use 95% confidence intervals instead of SEM
ggplot(tgc2, aes(x=dose, y=len, fill=supp)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=len-ci, ymax=len+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))

# with titles and legend
ggplot(tgc2, aes(x=dose, y=len, fill=supp)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=len-se, ymax=len+se),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Dose (mg)") +
  ylab("Tooth length") +
  scale_fill_hue(name="Supplement type", # Legend label, use darker colors
                 breaks=c("OJ", "VC"),
                 labels=c("Orange juice", "Ascorbic acid")) +
  ggtitle("The Effect of Vitamin C on\nTooth Growth in Guinea Pigs") +
  scale_y_continuous(breaks=0:20*4) +
  theme_bw()

# Data wrangling

#filter
flights
filter(flights, month == 1, day == 1)
jan1 <- filter(flights, month == 1, day == 1)
(dec25 <- filter(flights, month == 12, day == 25))
dec25
# or by month
filter(flights, month == 11 | month == 12)
nov_dec <- filter(flights, month %in% c(11, 12))
# find flights that weren't delayed (on arrival or departure) by more than two hours
filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120)
# rearrange columns
arrange(flights, year, month, day)
# in decsending order
arrange(flights, desc(dep_delay))
# Select columns by name
select(flights, year, month, day)
# Select all columns between year and day (inclusive)
select(flights, year:day)
# Select all columns except those from year to day (inclusive)
select(flights, -(year:day))
# rename columns
rename(flights, tail_num = tailnum)
# rearrange columns with time_hour, air_time first and then everything else
select(flights, time_hour, air_time, everything())
# filter then create a new column
flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time
)
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60
)
# refer to columns created
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours
)
# keep only the variables created
transmute(flights,
          gain = dep_delay - arr_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours
)
# division and remainders
transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100
)
# summary, gets the mean
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))
# group then summarize
by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))

# combine operations with pipes
by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
)
delay
delay <- filter(delay, count > 20, dest != "HNL")
# graph
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

# another way of doing this
delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")
delays
# remove nas
flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay, na.rm = TRUE))

# remove not canceled flights
not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))
# counts
delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay)
  )

ggplot(data = delays, mapping = aes(x = delay)) + 
  geom_freqpoly(binwidth = 10)

# plot the average delay
delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)
# filter out the smallest numbers
delays %>% 
  filter(n > 25) %>% 
  ggplot(mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)

# batting scatterplot example
batting <- as_tibble(Lahman::Batting)

batters <- batting %>% 
  group_by(playerID) %>% 
  summarise(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE)
  )

batters %>% 
  filter(ab > 100) %>% 
  ggplot(mapping = aes(x = ab, y = ba)) +
  geom_point() + 
  geom_smooth(se = FALSE)
batters %>% 
  arrange(desc(ba))
# subsetting
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0]) # the average positive delay
  )
# Why is distance to some destinations more variable than to others?
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(distance_sd = sd(distance)) %>% 
  arrange(desc(distance_sd))
# When do the first and last flights leave each day?
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )
# the first and last departure for each day:
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first_dep = first(dep_time), 
    last_dep = last(dep_time)
  )

# rank filtered
not_cancelled %>% 
  group_by(year, month, day) %>% 
  mutate(r = min_rank(desc(dep_time))) %>% 
  filter(r %in% range(r))
# Which destinations have the most carriers?
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(carriers = n_distinct(carrier)) %>% 
  arrange(desc(carriers))
not_cancelled %>% 
  count(dest)
# weight by distance traveled
not_cancelled %>% 
  count(tailnum, wt = distance)
# How many flights left before 5am? (these usually indicate delayed
# flights from the previous day)
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(n_early = sum(dep_time < 500))
# What proportion of flights are delayed by more than an hour?
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(hour_perc = mean(arr_delay > 60))
# group multiple variables
daily <- group_by(flights, year, month, day)
(per_day   <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year  <- summarise(per_month, flights = sum(flights)))
# ungroup
daily %>% 
  ungroup() %>%             # no longer grouped by date
  summarise(flights = n())  # all flights
# Find the worst members of each group:
flights_sml %>% 
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)
# Find all groups bigger than a threshold
popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)
popular_dests
# Standardise to compute per group metrics
popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)

# get a count
diamonds %>% 
  count(cut)
# histogram with bin width set
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)
# histogram with filter
smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1)
# overlay histogram
ggplot(data = smaller, mapping = aes(x = carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1)
ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)
# histogram of old faithful
ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_histogram(binwidth = 0.25)
# unusual values
ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)
# zoom in by changing the scale
ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))
unusual <- diamonds %>% 
  filter(y < 3 | y > 20) %>% 
  select(price, x, y, z) %>%
  arrange(y)
unusual
# drop rows of strange values - not recommended
diamonds2 <- diamonds %>% 
  filter(between(y, 3, 20))
# replace unusual values with NA
diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))
diamonds2
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point()
# compare canceled flights
nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time)) + 
  geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)
# density plot
ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)
# boxplot
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()
# ordered
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy))
# flip axes
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip()
# two categorical variables
ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))
diamonds %>% 
  count(color, cut)
# heatmap
diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))
ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))
ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price), alpha = 1 / 100)
ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x = carat, y = price))

# install.packages("hexbin")
ggplot(data = smaller) +
  geom_hex(mapping = aes(x = carat, y = price))
ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))
ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))
ggplot(data = diamonds) +
  geom_point(mapping = aes(x = x, y = y)) +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))
ggplot(data = faithful) + 
  geom_point(mapping = aes(x = eruptions, y = waiting))
# model the relationships
library(modelr)

mod <- lm(log(price) ~ log(carat), data = diamonds)

diamonds2 <- diamonds %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))
# residual plot
ggplot(data = diamonds2) + 
  geom_point(mapping = aes(x = carat, y = resid))
ggplot(data = diamonds2) + 
  geom_boxplot(mapping = aes(x = cut, y = resid))
diamonds %>% 
  count(cut, clarity) %>% 
  ggplot(aes(clarity, cut, fill = n)) + 
  geom_tile()

# get working directory
getwd()

# create tibble
as_tibble(iris)
nycflights13::flights %>% 
  print(n = 10, width = Inf)
nycflights13::flights %>% 
  View()
# parsing numbers
parse_number("$100")
#> [1] 100
parse_number("20%")
#> [1] 20
parse_number("It cost $123.45")
#> [1] 123
parse_datetime("2010-10-01T2010")
#> [1] "2010-10-01 20:10:00 UTC"
# If time is omitted, it will be set to midnight
parse_datetime("20101010")
#> [1] "2010-10-10 UTC"
library(hms)
parse_time("01:10 am")
#> 01:10:00
parse_time("20:10:01")
#> 20:10:01
parse_date("01/02/15", "%m/%d/%y")
#> [1] "2015-01-02"
parse_date("01/02/15", "%d/%m/%y")
#> [1] "2015-02-01"
parse_date("01/02/15", "%y/%m/%d")
#> [1] "2001-02-15"
# write to a file
write_csv(mpg, "mpg.csv")
write_rds(mpg, "mpg.rds")
read_rds("mpg.rds")
# write to binary file
library(feather)
write_feather(mpg, "mpg.feather")
read_feather("mpg.feather")

# tidy data
# caclulate a case rate
table1 %>% 
  mutate(rate = cases / population * 10000)
# Compute cases per year
table1 %>% 
  count(year, wt = cases)
# Visualise changes over time
library(ggplot2)
ggplot(table1, aes(year, cases)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))
# spread and gather
# the column names 1999 and 2000 represent values of the year variable, and each row represents two observations, not one.
table4a
table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "cases")
table4b %>% 
  gather(`1999`, `2000`, key = "year", value = "population")
# join tables
tidy4a <- table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "cases")
tidy4b <- table4b %>% 
  gather(`1999`, `2000`, key = "year", value = "population")
left_join(tidy4a, tidy4b)
# spread
table2
table2 %>%
  spread(key = type, value = count)
# separate
table3
table3 %>% 
  separate(rate, into = c("cases", "population"))
# use a character to separate
table3 %>% 
  separate(rate, into = c("cases", "population"), sep = "/")
# convert to numbers
table3 %>% 
  separate(rate, into = c("cases", "population"), convert = TRUE)
# separate within numbers
table3 %>% 
  separate(year, into = c("century", "year"), sep = 2)
# unite
table5
table5 %>% 
  unite(new, century, year)
# remove the _
table5 %>% 
  unite(new, century, year, sep = "")
# missing data
stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)
stocks %>% 
  spread(year, return)
# remove NAs
stocks %>% 
  spread(year, return) %>% 
  gather(year, return, `2015`:`2016`, na.rm = TRUE)
stocks %>% 
  complete(year, qtr)

treatment <- tribble(
  ~ person,           ~ treatment, ~response,
  "Derrick Whitmore", 1,           7,
  NA,                 2,           10,
  NA,                 3,           9,
  "Katherine Burke",  1,           4
)
# fill the most recent missing values
treatment %>% 
  fill(person)
# messy table
who
# remove missing values
who1 <- who %>% 
  gather(new_sp_m014:newrel_f65, key = "key", value = "cases", na.rm = TRUE)
who1
who1 %>% 
  count(key)
# make names consistent
who2 <- who1 %>% 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel"))
who2
# split the codes
who3 <- who2 %>% 
  separate(key, c("new", "type", "sexage"), sep = "_")
who3
# drop the new column
who3 %>% 
  count(new)
who4 <- who3 %>% 
  select(-new, -iso2, -iso3)
who4
# separate sex and age
who5 <- who4 %>% 
  separate(sexage, c("sex", "age"), sep = 1)
who5
# do everything
who %>%
  gather(key, value, new_sp_m014:newrel_f65, na.rm = TRUE) %>% 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel")) %>%
  separate(key, c("new", "var", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)

# relational data
airlines
airports
planes
weather
flights

planes %>% 
  count(tailnum) %>% 
  filter(n > 1)
weather %>% 
  count(year, month, day, hour, origin) %>% 
  filter(n > 1)
flights %>% 
  count(year, month, day, flight) %>% 
  filter(n > 1)
flights %>% 
  count(year, month, day, tailnum) %>% 
  filter(n > 1)
# mutate joins
flights2 <- flights %>% 
  select(year:day, hour, origin, dest, tailnum, carrier)
flights2
# left join
flights2 %>%
  select(-origin, -dest) %>% 
  left_join(airlines, by = "carrier")
# another way to do this
flights2 %>%
  select(-origin, -dest) %>% 
  mutate(name = airlines$name[match(carrier, airlines$carrier)])
# simple demonstrations
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)
# inner join
x %>% 
  inner_join(y, by = "key")
# outer join - left join
x %>% 
  left_join(y, by = "key")
# duplicate keys
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  1, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2"
)
left_join(x, y, by = "key")
# all combinations
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  3, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  2, "y3",
  3, "y4"
)
left_join(x, y, by = "key")
# join by other columns, the flights and weather tables match on their common variables
flights2 %>% 
  left_join(weather)
# join by a specific column
flights2 %>% 
  left_join(planes, by = "tailnum")
# match equal values
flights2 %>% 
  left_join(airports, c("dest" = "faa"))
flights2 %>% 
  left_join(airports, c("origin" = "faa"))
# map
airports %>%
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap()
# filtering joins - semi joins
top_dest <- flights %>%
  count(dest, sort = TRUE) %>%
  head(10)
top_dest
# find each flight that went to one of those destinations
flights %>% 
  filter(dest %in% top_dest$dest)
# only keeps the rows in x that have a match in y
flights %>% 
  semi_join(top_dest)
# remove non matching records
flights %>%
  anti_join(planes, by = "tailnum") %>%
  count(tailnum, sort = TRUE)
# set operations - matching
df1 <- tribble(
  ~x, ~y,
  1,  1,
  2,  1
)
df2 <- tribble(
  ~x, ~y,
  1,  1,
  1,  2
)
intersect(df1, df2)
union(df1, df2)
setdiff(df1, df2)
setdiff(df2, df1)

# strings
str_length(c("a", "R for data science", NA))
# combine strings
str_c("x", "y")
str_c("x", "y", "z")
# separate
str_c("x", "y", sep = ", ")
# collapse a vector into strings
str_c(c("x", "y", "z"), collapse = ", ")
# subset strings
x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)

# negative numbers count backwards from end
str_sub(x, -3, -1)
# change case
str_to_upper(c("i", "ı"))
str_to_lower(c("I", "ı"))
str_to_title(c("greg", "davis"))
# regular expressions
x <- c("apple", "banana", "pear")
str_view(x, "an")
str_view(x, ".a.")
str_view(c("abc", "a.c", "bef"), "a\\.c")
# anchors
str_view(x, "^a") # first
str_view(x, "a$") # last
# matches counts
x <- c("apple", "banana", "pear")
str_count(x, "a")
# On average, how many vowels per word?
mean(str_count(words, "[aeiou]"))
# doesn't work
df %>% 
  mutate(
    vowels = str_count(word, "[aeiou]"),
    consonants = str_count(word, "[^aeiou]")
  )
# extract nouns with the and a preceding a word in sentences
noun <- "(a|the) ([^ ]+)"

has_noun <- sentences %>%
  str_subset(noun) %>%
  head(10)
has_noun %>% 
  str_extract(noun)

tibble(sentence = sentences) %>% 
  tidyr::extract(
    sentence, c("article", "noun"), "(a|the) ([^ ]+)", 
    remove = FALSE
  )
# replace
x <- c("apple", "pear", "banana")
str_replace(x, "[aeiou]", "-")
str_replace_all(x, "[aeiou]", "-")
x <- c("1 house", "2 cars", "3 people")
str_replace_all(x, c("1" = "one", "2" = "two", "3" = "three"))
# split
sentences %>%
  head(5) %>% 
  str_split(" ")
# extract a number of pieces
fields <- c("Name: Hadley", "Country: NZ", "Age: 35")
fields %>% str_split(": ", n = 2, simplify = TRUE)
x <- "This is a sentence.  This is another sentence."
str_view_all(x, boundary("word"))
# factors
x1 <- c("Dec", "Apr", "Jan", "Mar")
x2 <- c("Dec", "Apr", "Jam", "Mar")
month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)
y1 <- factor(x1, levels = month_levels)
y1
sort(y1)

gss_cat
gss_cat %>%
  count(race)
ggplot(gss_cat, aes(race)) +
  geom_bar()
ggplot(gss_cat, aes(race)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE) # all factors (without data)
# modify the factor order
relig_summary <- gss_cat %>%
  group_by(relig) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

ggplot(relig_summary, aes(tvhours, relig)) + geom_point()
# ordered
ggplot(relig_summary, aes(tvhours, fct_reorder(relig, tvhours))) +
  geom_point()
ggplot(relig_summary, aes(age, fct_reorder(relig, age))) +
  geom_point()
# by income
rincome_summary <- gss_cat %>%
  group_by(rincome) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

ggplot(rincome_summary, aes(age, fct_reorder(rincome, age))) + geom_point()
ggplot(rincome_summary, aes(age, fct_relevel(rincome, "Not applicable"))) +
  geom_point()
by_age <- gss_cat %>%
  filter(!is.na(age)) %>%
  count(age, marital) %>%
  group_by(age) %>%
  mutate(prop = n / sum(n))

ggplot(by_age, aes(age, prop, colour = marital)) +
  geom_line(na.rm = TRUE)

ggplot(by_age, aes(age, prop, colour = fct_reorder2(marital, age, prop))) +
  geom_line() +
  labs(colour = "marital")

gss_cat %>%
  mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(marital)) +
  geom_bar()
# modify factor levels
gss_cat %>% count(partyid)
gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong"    = "Strong republican",
                              "Republican, weak"      = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak"        = "Not str democrat",
                              "Democrat, strong"      = "Strong democrat"
  )) %>%
  count(partyid)
# combine factors
gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong"    = "Strong republican",
                              "Republican, weak"      = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak"        = "Not str democrat",
                              "Democrat, strong"      = "Strong democrat",
                              "Other"                 = "No answer",
                              "Other"                 = "Don't know",
                              "Other"                 = "Other party"
  )) %>%
  count(partyid)

gss_cat %>%
  mutate(partyid = fct_collapse(partyid,
                                other = c("No answer", "Don't know", "Other party"),
                                rep = c("Strong republican", "Not str republican"),
                                ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                dem = c("Not str democrat", "Strong democrat")
  )) %>%
  count(partyid)

gss_cat %>%
  mutate(relig = fct_lump(relig)) %>%
  count(relig)
gss_cat %>%
  mutate(relig = fct_lump(relig, n = 10)) %>%
  count(relig, sort = TRUE) %>%
  print(n = Inf)
# dates and times
library(lubridate)
today()
now()
ymd("2017-01-31")
mdy("January 31st, 2017")
dmy("31-Jan-2017")
ymd(20170131)
ymd_hms("2017-01-31 20:11:59")
mdy_hm("01/31/2017 08:01")
ymd(20170131, tz = "UTC") # with time zone
flights %>% 
  select(year, month, day, hour, minute)
flights %>% 
  select(year, month, day, hour, minute) %>% 
  mutate(departure = make_datetime(year, month, day, hour, minute))

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt

flights_dt %>% 
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth = 86400) # 86400 seconds = 1 day
# one day
flights_dt %>% 
  filter(dep_time < ymd(20130102)) %>% 
  ggplot(aes(dep_time)) + 
  geom_freqpoly(binwidth = 600) # 600 s = 10 minutes

as_datetime(today())
as_date(now())
as_datetime(60 * 60 * 10)
as_date(365 * 10 + 2)

# date components
datetime <- ymd_hms("2016-07-08 12:34:56")
year(datetime)
month(datetime)
mday(datetime)
yday(datetime)
wday(datetime)
month(datetime, label = TRUE)
wday(datetime, label = TRUE, abbr = FALSE)

flights_dt %>% 
  mutate(wday = wday(dep_time, label = TRUE)) %>% 
  ggplot(aes(x = wday)) +
  geom_bar()
flights_dt %>% 
  mutate(minute = minute(dep_time)) %>% 
  group_by(minute) %>% 
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n()) %>% 
  ggplot(aes(minute, avg_delay)) +
  geom_line()
sched_dep <- flights_dt %>% 
  mutate(minute = minute(sched_dep_time)) %>% 
  group_by(minute) %>% 
  summarise(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n())

ggplot(sched_dep, aes(minute, avg_delay)) +
  geom_line()
ggplot(sched_dep, aes(minute, n)) +
  geom_line()
# rounding
flights_dt %>% 
  count(week = floor_date(dep_time, "week")) %>% 
  ggplot(aes(week, n)) +
  geom_line()
# set time components
(datetime <- ymd_hms("2016-07-08 12:34:56"))
year(datetime) <- 2020
datetime
month(datetime) <- 01
datetime
hour(datetime) <- hour(datetime) + 1
datetime
update(datetime, year = 2020, month = 2, mday = 2, hour = 2)
ymd("2015-02-01") %>% 
  update(mday = 30)
ymd("2015-02-01") %>% 
  update(hour = 400)
flights_dt %>% 
  mutate(dep_hour = update(dep_time, yday = 1)) %>% 
  ggplot(aes(dep_hour)) +
  geom_freqpoly(binwidth = 300)
# durations
# How old is Greg?
g_age <- today() - ymd(19590726)
g_age
as.duration(g_age)
dseconds(15)
dminutes(10)
dhours(c(12, 24))
ddays(0:5)
dweeks(3)
dyears(1)
2 * dyears(1)
dyears(1) + dweeks(12) + dhours(15)
tomorrow <- today() + ddays(1)
last_year <- today() - dyears(1)
# periods
seconds(15)
minutes(10)
hours(c(12, 24))
days(7)
months(1:6)
weeks(3)
years(1)
10 * (months(6) + days(1))
days(50) + hours(25) + minutes(2)
# A leap year
ymd("2016-01-01") + dyears(1)
ymd("2016-01-01") + years(1)

# Daylight Savings Time
now() + ddays(1)
now() + days(1)
# periods with flight times
flights_dt %>% 
  filter(arr_time < dep_time) 
# account for overnight
flights_dt <- flights_dt %>% 
  mutate(
    overnight = arr_time < dep_time,
    arr_time = arr_time + days(overnight * 1),
    sched_arr_time = sched_arr_time + days(overnight * 1)
  )
flights_dt %>% 
  filter(overnight, arr_time < dep_time) 
# interval
years(1) / days(1)
next_year <- today() + years(1)
(today() %--% next_year) / ddays(1)
(today() %--% next_year) %/% days(1)
# time zones
Sys.timezone()
length(OlsonNames())
head(OlsonNames())
(x1 <- ymd_hms("2015-06-01 12:00:00", tz = "America/New_York"))
(x2 <- ymd_hms("2015-06-01 18:00:00", tz = "Europe/Copenhagen"))
(x3 <- ymd_hms("2015-06-02 04:00:00", tz = "Pacific/Auckland"))

# pipes
library(magrittr)

diamonds <- ggplot2::diamonds
diamonds2 <- diamonds %>% 
  dplyr::mutate(price_per_carat = price / carat)

pryr::object_size(diamonds)
pryr::object_size(diamonds2)
pryr::object_size(diamonds, diamonds2)
# functions
# Compute confidence interval around mean using normal approximation
mean_ci <- function(x, conf = 0.95) {
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - conf
  mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
}
x <- runif(100)
mean_ci(x)

# vectors
typeof(letters)
typeof(1:10)
x <- list("a", "b", 1:10)
length(x)
set_names(1:3, c("a", "b", "c"))
# lists
x <- list(1, 2, 3)
x
x_named <- list(a = 1, b = 2, c = 3)
str(x_named)
# subset a list
a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))
str(a[1:2])
str(a[4])
str(a[[1]])
str(a[[4]])
a$a
a[["a"]]
# attributes
x <- 1:10
attr(x, "greeting")
attr(x, "greeting") <- "Hi!"
attr(x, "farewell") <- "Bye!"
attributes(x)
# iterations
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
output <- vector("double", ncol(df))  # 1. output
for (i in seq_along(df)) {            # 2. sequence
  output[[i]] <- median(df[[i]])      # 3. body
}
output

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)
df
for (i in seq_along(df)) {
  df[[i]] <- rescale01(df[[i]])
}

output <- vector("double", length(df))
for (i in seq_along(df)) {
  output[[i]] <- mean(df[[i]])
}
output

col_mean <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- mean(df[[i]])
  }
  output
}
col_median <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- median(df[[i]])
  }
  output
}
col_sd <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- sd(df[[i]])
  }
  output
}
col_summary <- function(df, fun) {
  out <- vector("double", length(df))
  for (i in seq_along(df)) {
    out[i] <- fun(df[[i]])
  }
  out
}
col_summary(df, median)
col_summary(df, mean)
# map function
map_dbl(df, mean, trim = 0.5)

models <- mtcars %>% 
  split(.$cyl) %>% 
  map(function(df) lm(mpg ~ wt, data = df))
models <- mtcars %>% 
  split(.$cyl) %>% 
  map(~lm(mpg ~ wt, data = .))
models
# walk
library(ggplot2)
plots <- mtcars %>% 
  split(.$cyl) %>% 
  map(~ggplot(., aes(mpg, wt)) + geom_point())
paths <- stringr::str_c(names(plots), ".pdf")

pwalk(list(paths, plots), ggsave, path = tempdir())
plots

# models
library(modelr)
options(na.action = na.warn)
ggplot(sim1, aes(x, y)) + 
  geom_point()
models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

ggplot(sim1, aes(x, y)) + 
  geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/4) +
  geom_point()
sim1

model1 <- function(a, data) {
  a[1] + data$x * a[2]
}
model1(c(7, 1.5), sim1)
measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}
measure_distance(c(7, 1.5), sim1)

sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}

models <- models %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))
models
ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(models, rank(dist) <= 10)
  )
ggplot(models, aes(a1, a2)) +
  geom_point(data = filter(models, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist))
grid <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

grid %>% 
  ggplot(aes(a1, a2)) +
  geom_point(data = filter(grid, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist)) 
ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(grid, rank(dist) <= 10)
  )
best <- optim(c(0, 0), measure_distance, data = sim1)
best$par


ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(intercept = best$par[1], slope = best$par[2])
sim1_mod <- lm(y ~ x, data = sim1)
coef(sim1_mod)

# predictions
grid <- sim1 %>% 
  data_grid(x) 
grid
grid <- grid %>% 
  add_predictions(sim1_mod) 
grid
ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +
  geom_line(aes(y = pred), data = grid, colour = "red", size = 1)
sim1 <- sim1 %>% 
  add_residuals(sim1_mod)
sim1
ggplot(sim1, aes(resid)) + 
  geom_freqpoly(binwidth = 0.5)
ggplot(sim1, aes(x, resid)) + 
  geom_ref_line(h = 0) +
  geom_point() 
df <- tribble(
  ~y, ~x1, ~x2,
  4, 2, 5,
  5, 1, 6
)
model_matrix(df, y ~ x1)
model_matrix(df, y ~ x1 - 1)
model_matrix(df, y ~ x1 + x2)
# categorical variables
df <- tribble(
  ~ sex, ~ response,
  "male", 1,
  "female", 2,
  "male", 1
)
model_matrix(df, response ~ sex)
ggplot(sim2) + 
  geom_point(aes(x, y))
mod2 <- lm(y ~ x, data = sim2)

grid <- sim2 %>% 
  data_grid(x) %>% 
  add_predictions(mod2)
grid
ggplot(sim2, aes(x)) + 
  geom_point(aes(y = y)) +
  geom_point(data = grid, aes(y = pred), colour = "red", size = 4)
# categorical and continuous
ggplot(sim3, aes(x1, y)) + 
  geom_point(aes(colour = x2))
mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)
grid <- sim3 %>% 
  data_grid(x1, x2) %>% 
  gather_predictions(mod1, mod2)
grid
ggplot(sim3, aes(x1, y, colour = x2)) + 
  geom_point() + 
  geom_line(data = grid, aes(y = pred)) + 
  facet_wrap(~ model)
sim3 <- sim3 %>% 
  gather_residuals(mod1, mod2)

ggplot(sim3, aes(x1, resid, colour = x2)) + 
  geom_point() + 
  facet_grid(model ~ x2)
# interactions - two continuous
mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)

grid <- sim4 %>% 
  data_grid(
    x1 = seq_range(x1, 5), 
    x2 = seq_range(x2, 5) 
  ) %>% 
  gather_predictions(mod1, mod2)
grid
seq_range(c(0.0123, 0.923423), n = 5)
seq_range(c(0.0123, 0.923423), n = 5, pretty = TRUE)
x1 <- rcauchy(100)
seq_range(x1, n = 5)
seq_range(x1, n = 5, trim = 0.10)
seq_range(x1, n = 5, trim = 0.25)
seq_range(x1, n = 5, trim = 0.50)
x2 <- c(0, 1)
seq_range(x2, n = 5)
seq_range(x2, n = 5, expand = 0.10)
seq_range(x2, n = 5, expand = 0.25)
seq_range(x2, n = 5, expand = 0.50)
ggplot(grid, aes(x1, x2)) + 
  geom_tile(aes(fill = pred)) + 
  facet_wrap(~ model)
ggplot(grid, aes(x1, pred, colour = x2, group = x2)) + 
  geom_line() +
  facet_wrap(~ model)
ggplot(grid, aes(x2, pred, colour = x1, group = x1)) + 
  geom_line() +
  facet_wrap(~ model)
# transformation
df <- tribble(
  ~y, ~x,
  1,  1,
  2,  2, 
  3,  3
)
model_matrix(df, y ~ poly(x, 2))
library(splines)
model_matrix(df, y ~ ns(x, 2))
sim5 <- tibble(
  x = seq(0, 3.5 * pi, length = 50),
  y = 4 * sin(x) + rnorm(length(x))
)

ggplot(sim5, aes(x, y)) +
  geom_point()
# compare models
mod1 <- lm(y ~ ns(x, 1), data = sim5)
mod2 <- lm(y ~ ns(x, 2), data = sim5)
mod3 <- lm(y ~ ns(x, 3), data = sim5)
mod4 <- lm(y ~ ns(x, 4), data = sim5)
mod5 <- lm(y ~ ns(x, 5), data = sim5)

grid <- sim5 %>% 
  data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>% 
  gather_predictions(mod1, mod2, mod3, mod4, mod5, .pred = "y")

ggplot(sim5, aes(x, y)) + 
  geom_point() +
  geom_line(data = grid, colour = "red") +
  facet_wrap(~ model)
