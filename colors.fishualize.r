library(fishualize)
fishualize()
fishualize(n = 8, option = "Hypsypops_rubicundus", end = 0.9)
pal <- fish(256, option = "Trimma_lantana")
image(volcano, col = pal)
pal <- fish(10, option = "Ostracion_cubicus")
image(volcano, col = pal)

library(ggplot2)
library(rfishbase)

# load data for plotting 
# 1. Create list of species names currently featured in fishualize
spp <- fishualize::fish_palettes()
# 2. Get data on the included species from FishBase using the rfishbase package
dt <- rfishbase::species(gsub("_"," ", spp))

dt$Importance = factor(dt$Importance, levels = c("highly commercial", "commercial", "minor commercial", "subsistence fisheries", "of no interest"))

# plot bars with discrete colors using color scheme provided by Scarus quoyi
ggplot(dt[!is.na(dt$Importance),]) +
  geom_bar(aes(x = Importance, fill = Importance)) +
  scale_fill_fish_d(option = "Scarus_quoyi") +
  theme_bw() +
  theme(axis.text.x= element_blank() )

# plot points with continuous colors provided by Hypsypops rubicundus
ggplot(dt) +
  geom_point(aes(x = Length, y = Vulnerability, color = Vulnerability), size = 3) +
  scale_color_fish(option = "Hypsypops_rubicundus", direction = -1) +
  theme_bw()

# get ecological information from FishBase
data <- rfishbase::ecology(gsub("_"," ", spp), c("SpecCode","FeedingType", "DietTroph")) %>% 
  dplyr::left_join( rfishbase::species(gsub("_"," ", spp)))

# plot boxplots of length across feeding groups using discrete colors provided by Cirrilabrus solorensis
ggplot(data[!is.na(data$FeedingType),]) +
  geom_boxplot(aes(x = FeedingType, y = log(Length), fill = FeedingType )) +
  scale_fill_fish_d(option = "Cirrhilabrus_solorensis", labels = c("invertivore", "herbivore", "carnivore", "planktivore", "omnivore")) +
  theme_bw() +
  theme(axis.text.x= element_blank() )

# examine relationships between size and trophic level with vulnerability as a continuous color scheme provided by Lepomis megalotis
ggplot(data) +
  geom_point(aes(x = Length, y = DietTroph, color = Vulnerability), size = 6, alpha = 0.9) +
  scale_color_fish(option = "Lepomis_megalotis", direction = -1) +
  theme_bw()

library(ggplot2)

#get dataset of the world's countries
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

#plot worldmap with each country's estimated population as a continuous colors scale based on the reverse colors of Whitley's Boxfish Ostracion whitleyi
ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_fish(option = "Ostracion_whitleyi", direction = -1) +
  theme_bw() +
  theme(legend.position = "top")

#plot worldmap with each country's estimated gdp based on the colors of the Sailfin Tang Zebrasoma velifer
ggplot(data = world) +
  geom_sf(aes(fill = gdp_md_est)) +
  scale_fill_fish(option = "Zebrasoma_velifer", trans = "sqrt") +
  theme_bw()+
  theme(legend.position = "top")

#same example as above but starting at a lighter point of the color scale
ggplot(data = world) +
  geom_sf(aes(fill = gdp_md_est)) +
  scale_fill_fish(option = "Zebrasoma_velifer", trans = "sqrt", begin = 0.3, end = 1) +
  theme_bw()+
  theme(legend.position = "top")

#plot worldmap again, this time with countries colored by their respective regional affiliation using the colors of the Clown coris *Coris gaimard* and 'discrete = TRUE'
ggplot(data = world) +
  geom_sf(aes(fill = region_wb)) +
  scale_fill_fish(option = "Coris_gaimard", discrete = TRUE) +
  theme_bw()+
  theme(legend.position = "top")

##same map with colors reversed
ggplot(data = world) +
  geom_sf(aes(fill = region_wb)) +
  scale_fill_fish(option = "Coris_gaimard", discrete = TRUE, direction = -1) +
  theme_bw()+
  theme(legend.position = "top")
#another map with countries colored by economic status using the colors of the Mandarinfish *Synchiropus splendidus*
ggplot(data = world) +
  geom_sf(aes(fill = income_grp)) +
  scale_fill_fish(option = "Synchiropus_splendidus", discrete = T, alpha = 0.8) +
  theme_bw()+
  theme(legend.position = "top")
#same map as above but with a narrower color palette in discrete values
ggplot(data = world) +
  geom_sf(aes(fill = income_grp)) +
  scale_fill_fish(option = "Synchiropus_splendidus", discrete = T, alpha = 0.8, begin = 0.3, end = 1) +
  theme_bw()+
  theme(legend.position = "top")
