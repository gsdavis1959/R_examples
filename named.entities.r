if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh("trinker/entity")
library(entity)

data(wiki)
wiki

people <- na.omit(person_entity(wiki))
people
location_entity(wiki)
organization_entity(wiki)
date_entity(wiki)
money_entity(wiki)
percent_entity(wiki)

organizations <- organization_entity(presidential_debates_2012$dialogue)
plot(organizations)
plot(organizations, min = 2)
plot(organizations, alphabetical = TRUE)

people <- person_entity(presidential_debates_2012$dialogue)
plot(people, min = 2, alphabetical = TRUE)
