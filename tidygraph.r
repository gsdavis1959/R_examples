library(tidygraph)
library(ggraph)
# annotate highschool graph example
# as a tidygraph
data(highschool)
graph <- as_tbl_graph(highschool)

graph %>%
  ggraph(layout="kk") +
  geom_edge_fan(arrow=arrow()) +
  geom_node_point() +
  theme_graph(foreground=NA)
# highlight year
graph %>%
  ggraph(layout="kk") +
  geom_edge_fan(aes(color=factor(year)), arrow=arrow()) +
  geom_node_point() +
  theme_graph(foreground=NA)
# look at only 1958
graph %>%
  activate(edges) %>%
  filter(year == 1958) %>%
  ggraph(layout="kk") +
  geom_edge_fan(aes(color=factor(year)), arrow=arrow()) +
  geom_node_point() +
  theme_graph(foreground=NA)

undirected_graph <- graph %>%
  convert(to_undirected) %>%
  convert(to_simple)

undirected_graph  %>%
  ggraph(layout="kk") +
  geom_edge_fan() +
  geom_node_point() +
  theme_graph(foreground=NA)

undirected_graph <- undirected_graph %>%
  activate(nodes) %>%
  mutate(degree=centrality_degree())
undirected_graph

# with degree
undirected_graph %>%
  ggraph(layout="kk") +
  geom_edge_fan() +
  geom_node_point(aes(size=degree)) +
  theme_graph(foreground=NA)
# with centrality
undirected_graph %>%
  activate(nodes) %>%
  mutate(centrality=centrality_eigen()) %>%
  ggraph(layout="kk") +
  geom_edge_fan() +
  geom_node_point(aes(size=centrality)) +
  theme_graph(foreground=NA)
