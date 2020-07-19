library(tidyverse)
#midwest is a dataset included in tidyverse

dat<-midwest %>% group_by(state) %>% summarize(poptotal=sum(poptotal), popwhite=sum(popwhite))


dat$state<-factor(dat$state, levels=dat$state[order(dat$poptotal)])
dat %>% ggplot(aes(state, poptotal))+
  geom_col()+
  geom_point(aes(y=popwhite), color="red")+
  theme_classic()
