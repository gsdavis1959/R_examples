library(tidyverse)
library(caret)

setwd("~/Data/RStatistics/Projects/nfl/nflscrapR-data-master/play_by_play_data/regular_season")
df <- read.csv('sum.passer.csv', stringsAsFactors = FALSE)
names(df)
dummy <- df
dmy <- dummyVars(" ~ posteam", data = dummy)
dummy <- data.frame(predict(dmy, newdata = dummy))
print(names(dummy))

c <- cbind(dummy, df)

dim(dummy)
r <- lm(sum_yards ~ 
           posteamARI +
           posteamATL +
           posteamBAL +
           posteamBUF +
           posteamCAR +
           posteamCHI +
           posteamCIN +
           posteamCLE +
           posteamDAL +
           posteamDEN +
           posteamDET +
           posteamGB +
           posteamHOU +
           posteamIND +
           posteamJAC + 
           posteamJAX +
           posteamKC +
           posteamLA +
           posteamLAC +
           posteamMIA +
           posteamMIN +
           posteamNE +
           posteamNO +
           posteamNYG +
           posteamNYJ +
           posteamOAK +
           posteamPHI +
           posteamPIT +
           posteamSD +
           posteamSEA +
           posteamSF +
           posteamSTL +
           posteamTB +
           posteamTEN +
           posteamWAS, data = c)
summary(r)

r <- lm(sum_yards ~ mean_yards + year, data = c)

library(lubridate)
all <- read.csv('reg_pbp_2009-2018.csv')
df_pbp <- all %>%
  select(posteam, passer_player_name, yards_gained, game_date) %>%
  mutate(year = year(game_date)) %>%
  na.omit()


df_pbp <- as.data.frame(df_pbp)
sum.passer <- df_pbp %>%
  group_by(posteam, year, passer_player_name) %>%
  summarise(sum_yards = sum(yards_gained), mean_yards = mean(yards_gained), sd_yards = sd(yards_gained), attempts = n()) %>%
  na.omit()
sum.passer

write.csv(sum.passer, 'sum.passer.csv')

r_attempts <- lm(sum_yards ~ attempts * sd_yards, data = sum.passer)
summary(r_attempts)

aov <- aov(sum_yards ~ attempts * sd_yards, data = sum.passer)
summary(aov)
plot(aov)

library(ggthemes)

ggplot(sum.passer, aes(x=attempts, y=sum_yards)) + 
  geom_point(aes(size=sd_yards))+
  geom_smooth(method=lm) +
  theme_bw() +
  labs(x = "Passes",
       y = "Yards Gained",
       color = "Team",
       size = "Std Deviation (Yards)",
       title = "Yards Gained from the Number of Passes Thrown",
       subtitle = "By QB per year from 2009-2018",
       caption = "Data accessed via nflscrapR")
