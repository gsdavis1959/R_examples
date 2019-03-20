library(tidyverse)

emotion_raw <- read_csv("https://osf.io/e7uab/download") %>%
  rename(Offense = Ofense,
         Embarrassment = Embarassment)
emotion_raw

# change shape

emotion_long <- emotion_raw %>%
  gather(emotion_type, value, Pride:Anger) %>%
  arrange(id, Day) %>%
  filter(value == 1) %>%
  select(-value)

emotion_long

# make edges

emotion_edges <- emotion_long %>%
  mutate(second_emotion = lead(emotion_type)) %>%
  rename(first_emotion = emotion_type) %>%
  select(id, Day, Hours, first_emotion, second_emotion) %>%
  group_by(id) %>%
  slice(-length(id))

emotion_edges

# make nodes
emotion_nodes <- emotion_long %>%
  count(emotion_type) %>%
  rowid_to_column("id") %>%
  rename(label = emotion_type) %>%
  mutate(valence = ifelse(label %in% c("Awe", "Amusement", "Joy", "Alertness",
                                       "Hope", "Love", "Gratitude", "Pride",
                                       "Satisfaction"), "positive", "negative"))

emotion_nodes

# weight connections between nodes

emotion_network <- emotion_edges %>%
  group_by(first_emotion, second_emotion) %>%
  summarize(weight = n()) %>%
  ungroup() %>%
  select(first_emotion, second_emotion, weight)

emotion_network

# trim high values
edges <- emotion_network %>%
  left_join(emotion_nodes, by = c("first_emotion" = "label")) %>%
  rename(from = id)

edges <- edges %>%
  left_join(emotion_nodes, by = c("second_emotion" = "label")) %>%
  rename(to = id) %>%
  select(from, to, weight) %>%
  mutate(weight = ifelse(weight > 4500, 4500, weight))

edges

# make graph
library(tidygraph)
library(ggraph)

network <- tbl_graph(emotion_nodes, edges, directed = TRUE)

set.seed(190318)

ggraph(network, layout = "graphopt") +
  geom_edge_link(aes(width = weight, color = scale(weight), alpha = weight), check_overlap = TRUE) +
  scale_edge_color_gradient2(low = "darkgrey", mid = "#00BFFF", midpoint = 1.5, high = "dodgerblue2") +
  scale_edge_width(range = c(.2, 1.75)) +
  geom_node_label(aes(label = label, fill = valence), size = 4) +
  scale_fill_manual(values = c("#FF6A6A", "#43CD80")) +
  theme_graph() +
  theme(legend.position = "none", plot.background = element_rect(fill = "black"))

# transform network graph

library(networkD3)

nodes_d3 <- emotion_nodes %>%
  mutate(id = id - 1,
         n = (scale(n) + 3)^3)

edges_d3 <- edges %>%
  mutate(from = from - 1, to = to - 1,
         weight = ifelse(weight < 600, 0, log(weight)))

forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Nodesize = "n",
             Target = "to", NodeID = "label", Group = "valence", Value = "weight", fontFamily = "sans-serif",
             colourScale = JS('d3.scaleOrdinal().domain(["negative", "positive"]).range(["#FF6A6A", "#43CD80"])'),
             opacity = 1, fontSize = 24, linkDistance = 300, linkColour = c("#8DB6CD"),
             arrows = TRUE, zoom = TRUE, bounded = TRUE, legend = TRUE)
