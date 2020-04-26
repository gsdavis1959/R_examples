library(readr)
library(tidyverse)
library(tidygraph)

url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-26/small_trains.csv"
small_trains <- read_csv(url)
head(small_trains)

# transform stations into from, to, and average journey time

routes <- small_trains %>%
  group_by(departure_station, arrival_station) %>%
  summarise(journey_time = mean(journey_time_avg)) %>%
  ungroup() %>%
  mutate(from = departure_station, 
         to = arrival_station) %>%
  select(from, to, journey_time)

routes

# transform the route pairs into a network

graph_routes <- as_tbl_graph(routes)

graph_routes

# creates two dataframes, edges and nodes

library(stringr)

# add attributes to the nodes


graph_routes <- graph_routes %>%
  activate(nodes) %>%
  mutate(
    title = str_to_title(name),
    label = str_replace_all(title, " ", "\n")
  )

graph_routes

# get a list of stations

stations <- graph_routes %>%
  activate(nodes) %>%
  pull(title)

stations

# graph the network - create a theme

thm <- theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
  ) 

theme_set(thm)

library(ggraph) 

graph_routes %>%
  ggraph(layout = "kk") +
  geom_node_point() +
  geom_edge_diagonal() 

graph_routes %>%
  ggraph(layout = "kk") +
  geom_node_text(aes(label = label, color = name), size = 3) +
  geom_edge_diagonal(color = "gray", alpha = 0.4) 

# determine the shortest path

from <- which(stations == "Arras")
to <-  which(stations == "Nancy")

library(rlang)
child_env("rlang")
shortest <- graph_routes %>%
  morph(to_shortest_path, from, to, weights = journey_time)

shortest

shortest <- shortest %>%
  mutate(selected_node = TRUE) %>%
  activate(edges) %>%
  mutate(selected_edge = TRUE) %>%
  unmorph() 

shortest <- shortest %>%
  activate(nodes) %>%
  mutate(selected_node = ifelse(is.na(selected_node), 1, 2)) %>%
  activate(edges) %>%
  mutate(selected_edge = ifelse(is.na(selected_edge), 1, 2)) %>%
  arrange(selected_edge)

shortest

shortest %>%
  ggraph(layout = "kk") +
  geom_edge_diagonal(aes(alpha = selected_edge), color = "gray") +
  geom_node_text(aes(label = label, color =name, alpha = selected_node ), size = 3) 

shortest %>%
  activate(edges) %>%
  filter(selected_edge == 2) %>%
  as_tibble() %>%
  summarise(
    total_stops = n() - 1,
    total_time = round(sum(journey_time) / 60)
  )

from <- which(stations == "Montpellier")
to <-  which(stations == "Laval")

shortest <- graph_routes %>%
  morph(to_shortest_path, from, to, weights = journey_time) %>%
  mutate(selected_node = TRUE) %>%
  activate(edges) %>%
  mutate(selected_edge = TRUE) %>%
  unmorph() %>%
  activate(nodes) %>%
  mutate(selected_node = ifelse(is.na(selected_node), 1, 2)) %>%
  activate(edges) %>%
  mutate(selected_edge = ifelse(is.na(selected_edge), 1, 2)) %>%
  arrange(selected_edge)

shortest %>%
  ggraph(layout = "kk") +
  geom_edge_diagonal(aes(alpha = selected_edge), color = "gray") +
  geom_node_text(aes(label = label, color =name, alpha = selected_node ), size = 3)

shortest %>%
  activate(edges) %>%
  filter(selected_edge == 2) %>%
  as_tibble() %>%
  summarise(
    total_stops = n() - 1,
    total_time = round(sum(journey_time) / 60)
  )

# part 2
create_ring(10)

iris_clust <- hclust(dist(iris[1:4]))
iris_tree <- as_tbl_graph(iris_clust)
iris_tree

iris_tree %>% activate(edges)

iris_tree <- iris_tree %>% 
  activate(nodes) %>% 
  mutate(Species = ifelse(leaf, as.character(iris$Species)[label], NA)) %>% 
  activate(edges) %>% 
  mutate(to_setose = .N()$Species[to] == 'setosa')
iris_tree

iris_sum <- iris %>% 
  group_by(Species) %>% 
  summarise_all(mean) %>% 
  ungroup()
iris_tree <- iris_tree %>% 
  activate(nodes) %>% 
  left_join(iris_sum)
iris_tree

# example of binding graphs
gr1 <- create_notable('bull') %>% 
  mutate(name = letters[1:5])
gr2 <- create_ring(5) %>% 
  mutate(name = letters[4:8])

# Plot
gr1 %>% bind_graphs(gr2) %>% 
  ggraph(layout = 'kk') + 
  geom_edge_link() + 
  geom_node_point(size = 8, colour = 'steelblue') +
  geom_node_text(aes(label = name), colour = 'white', vjust = 0.4) + 
  ggtitle('Binding graphs') + 
  theme_graph()

# joining graphs

gr1 %>% graph_join(gr2) %>% 
  ggraph(layout = 'kk') + 
  geom_edge_link() + 
  geom_node_point(size = 8, colour = 'steelblue') +
  geom_node_text(aes(label = name), colour = 'white', vjust = 0.4) + 
  ggtitle('Joining graphs') + 
  theme_graph()

# reroute graphs
gr1 <- create_star(6, directed = TRUE)
layout <- create_layout(gr1, layout = 'fr')
gr1 <- gr1 %>% 
  mutate(x = layout$x, y = layout$y, graph = 'original')
gr2 <- gr1 %>% 
  mutate(graph = 'reverse') %>% 
  activate(edges) %>% 
  reroute(from = to, to = from)
gr3 <- gr1 %>% 
  mutate(graph = 'using subset') %>% 
  activate(edges) %>% 
  reroute(from = to + 1, subset = to < 4)
ggraph(bind_graphs(gr1, gr2, gr3), layout = 'nicely') + 
  geom_edge_link(arrow = arrow()) + 
  facet_nodes(~graph) + 
  theme_graph(foreground = 'steelblue')

# find node and edge types

create_tree(20, 3) %>% 
  mutate(leaf = node_is_leaf(), root = node_is_root()) %>% 
  ggraph(layout = 'tree') +
  geom_edge_diagonal() +
  geom_node_point(aes(filter = leaf), colour = 'forestgreen', size = 10) +
  geom_node_point(aes(filter = root), colour = 'firebrick', size = 10) +
  theme_graph()

# centrality
play_smallworld(1, 100, 3, 0.05) %>% 
  mutate(centrality = centrality_authority()) %>% 
  ggraph(layout = 'kk') + 
  geom_edge_link() + 
  geom_node_point(aes(size = centrality, colour = centrality)) + 
  scale_color_continuous(guide = 'legend') + 
  theme_graph()

# clustering to find communities (see if I can use this to find clusters of degreed users)
play_islands(5, 10, 0.8, 3) %>% 
  mutate(community = as.factor(group_infomap())) %>% 
  ggraph(layout = 'kk') + 
  geom_edge_link(aes(alpha = ..index..), show.legend = FALSE) + 
  geom_node_point(aes(colour = community), size = 7) + 
  theme_graph()

# distance to center
play_geometry(50, 0.25) %>% 
  mutate(dist_to_center = node_distance_to(node_is_center())) %>% 
  ggraph(layout = 'kk') + 
  geom_edge_link() + 
  geom_node_point(aes(size = dist_to_center), colour = 'steelblue') + 
  scale_size_continuous(range = c(6, 1)) + 
  theme_graph()

# searches
play_geometry(50, 0.25) %>% 
  mutate(order = bfs_rank(which.max(centrality_alpha())))

# Weight the node degree by the average degree of its neighboors
play_smallworld(1, 100, 3, 0.05) %>% 
  mutate(weighted_degree = centrality_degree() / local_ave_degree())

# Normalise the node pair adhesion with the minimal adhesion of the graph
play_islands(5, 10, 0.7, 3) %>% 
  mutate(norm_adhesion = node_adhesion_to(c(50, 1:49)) / graph_adhesion())

# mapping over searches
iris_tree <- iris_tree %>% 
  activate(nodes) %>% 
  mutate(Species = map_bfs_back_chr(node_is_root(), .f = function(node, path, ...) {
    nodes <- .N()
    if (nodes$leaf[node]) return(nodes$Species[node])
    if (anyNA(unlist(path$result))) return(NA_character_)
    path$result[[1]]
  }))
ggraph(iris_tree, layout = 'dendrogram') + 
  geom_edge_diagonal2(aes(colour = node.Species)) + 
  theme_graph()

# mapping neighborhoods
play_smallworld(1, 100, 3, 0.05) %>% 
  mutate(neighborhood_edges = map_local_dbl(.f = function(neighborhood, ...) {
    igraph::gsize(neighborhood)
  }))

# morph to take portions of the graph for use in other calcuations
islands <- play_islands(5, 10, 0.8, 3) %>% 
  mutate(group = group_infomap())

# Get the distance to the central node in each group
islands <- islands %>% 
  morph(to_split, group) %>% 
  mutate(dist_to_center = node_distance_to(node_is_center())) %>% 
  unmorph()
islands

islands <- islands %>% 
  morph(to_contracted, group, simplify = FALSE) %>% 
  activate(edges) %>% 
  filter(!edge_is_loop()) %>% 
  activate(nodes) %>% 
  mutate(exiting_group = centrality_degree(mode = 'out')) %>% 
  unmorph()
islands

islands <- islands %>% 
  morph(to_linegraph) %>% 
  activate(nodes) %>% 
  mutate(edge_centrality = centrality_pagerank()) %>% 
  unmorph()
islands

# crystalize removes any link in the original graph
islands %>% 
  morph(to_split, group) %>% 
  crystallise()
