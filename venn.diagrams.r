library(UpSetR)
library(venneuler)
library(rJava)
library(tidyverse)
library(grid)
# Data
expressionInput <- c(`#rstats` = 5, memes = 5, `#rstats&memes` = 3)
# Venn
# note on set up for java v11 jdk (v12 does not work with this)

myExpVenn <- venneuler(expressionInput)
par(cex=1.2)
plot(myExpVenn, main = "Why I Love Twitter")
grid.text(
  "@littlemissdata",
  x = 0.52,
  y = 0.2,
  gp = gpar(
    fontsize = 10,
    fontface = 3
  )
)
# UpsetR
upset(fromExpression(expressionInput), order.by = "freq")
grid.text(
  "Why I Love Twitter  @littlemissdata",
  x = 0.80,
  y = 0.05,
  gp = gpar(
    fontsize = 10,
    fontface = 3
  )
) # created the label via grid.text with hack found here: https://github.com/hms-dbmi/UpSetR/issues/76

# get data of Toronto senior citizen survey
rawSets <- read.csv(
  file = "https://raw.githubusercontent.com/lgellis/MiscTutorial/master/sets/seniorTransportation.csv",
  header = TRUE, sep = ",", stringsAsFactors = FALSE
)

# Some formatting

# Replace the NA's
rawSets[is.na(rawSets)] <- 0


# Rename the columns for easier display
sets <- rawSets %>%
  rename(TTC = ttcTransportation, Walk = walkTransportation, Drive = driveTransportation, Cycle = cycleTransportation, Taxi = taxiTransportation, `Community Ride` = communityRideTransportation, `Wheel Trans` = wheelTransTransportation, Friends = friendsTransportation)

dim(sets)
head(sets)

vennSets <- sets %>%
  gather(transportation, binary,6:13) %>% # take all binary mappings and convert to be a the set indicator
  filter(binary == 1) %>% # only include set matches
  select(ï..ID, transportation) %>% # only include ID and set category
  mutate(transportation = factor(transportation)) # set the transportation column as a factor

dim(vennSets)

v <- venneuler(data.frame(vennSets))

#Note that if you need to move around the labels so that they are not overlapping, you can use the new line breaks like the example below.
#v$labels <- c("TTC", "Walk", "Drive", "Cycle\n\n\n", "\nTaxi", "Community Ride", "Wheel Trans", "Friends")

par(cex = 0.7) 
plot(v, main = "Modes of Senior Transportation (Toronto 2017 Survey)", cex.main = 1.5)
grid.text(
  "@littlemissdata",
  x = 0.52,
  y = 0.15,
  gp = gpar(
    fontsize = 10,
    fontface = 3
  )
)

# format for upset chart
vennSets %>% 
  add_column(ID=1) %>% #a helper column for the spread
  spread (transportation, ID) #spread the data to binary columns

#NA replacement
vennSets[is.na(vennSets)] <- 0

upset(sets,
      nsets = 10, number.angles = 30, point.size = 3.5, line.size = 2,
      mainbar.y.label = "Modes of Senior Transportation (Toronto 2017 Survey)", sets.x.label = "Total Participants"
)
grid.text(
  "@littlemissdata",
  x = 0.90,
  y = 0.05,
  gp = gpar(
    fontsize = 10,
    fontface = 3
  )
)


upset(sets,
      query.legend = "bottom", nsets = 10, number.angles = 30, point.size = 3.5, line.size = 2,
      mainbar.y.label = "Modes of Senior Transportation (Toronto 2017 Survey)", sets.x.label = "Total Participants", 
      queries = list(
        list(
          query = intersects,
          params = list("Cycle", "Walk"), 
          color = "#Df5286", 
          active = T,
          query.name = "Physically Active Transportation"
        )
      )
)
grid.text(
  "@littlemissdata",
  x = 0.90,
  y = 0.05,
  gp = gpar(
    fontsize = 10,
    fontface = 3
  )
)

upset(sets,
      query.legend = "bottom", nsets = 10, number.angles = 30, point.size = 3.5, line.size = 2,
      mainbar.y.label = "Modes of Senior Transportation (Toronto 2017 Survey)", sets.x.label = "Total Participants", 
      queries = list(
        list(
          query = elements,
          params = list("physicalActivityPerMonth", 0,4),
          color = "#Df5286", 
          active = T,
          query.name = "Physically Active 1x/Week or Less"
        )
      )
)
grid.text(
  "@littlemissdata",
  x = 0.90,
  y = 0.05,
  gp = gpar(
    fontsize = 10,
    fontface = 3
  )
)

upset(sets,
      query.legend = "bottom", nsets = 10, number.angles = 30, point.size = 3.5, line.size = 2,  
      queries = list(
        list(
          query = elements,
          params = list("physicalActivityPerMonth", 0,4),
          color = "#Df5286", 
          active = T,
          query.name = "Physically Active 1x/Week or Less"
        )
      ), 
      boxplot.summary = c("minAgeRange")
)
grid.text(
  "@littlemissdata",
  x = 0.90,
  y = 0.05,
  gp = gpar(
    fontsize = 10,
    fontface = 3
  )
)

upset(sets,
      query.legend = "bottom", nsets = 10, number.angles = 30, point.size = 3.5, line.size = 2,
      mainbar.y.label = "Modes of Senior Transportation (Toronto 2017 Survey)", sets.x.label = "Total Participants", 
      queries = list(
        list(
          query = elements,
          params = list("physicalActivityPerMonth", 0,4),
          color = "#Df5286", 
          active = T,
          query.name = "Physically Active 1x/Week or Less"
        )
      ), 
      attribute.plots = list(gridrows = 50, 
                             plots = list(list(plot = histogram, x = "volunteerPerMonth", queries = T), 
                                          list(plot = histogram, x = "minAgeRange", queries = T), 
                                          list(plot = scatter_plot, x = "minAgeRange", y="volunteerPerMonth", queries = F)
                             ), 
                             ncols = 3
      ) 
)
grid.text(
  "@littlemissdata",
  x = 0.9,
  y = 0.02,
  gp = gpar(
    fontsize = 10,
    fontface = 3
  )
)

aggregate <- sets %>% 
  gather(transportation, binary,6:13) %>% 
  filter(binary == 1) %>% # only include set matches
  group_by(transportation) %>%  #get summary stats per transportation category
  summarize(physicalActivityPerMonth = mean(physicalActivityPerMonth))

aggregate
upset(sets, set.metadata = list(data = aggregate, plots = list(list(type = "hist", 
                                                                    column = "physicalActivityPerMonth", assign = 50))))

