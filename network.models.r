library(CINNA)
data("zachary")
zachary

graph_extract_components(zachary)

library(igraph)
zachary_edgelist <- as_edgelist(zachary)

misc_extract_components(zachary_edgelist)
giant_component_extract(zachary)

proper_centralities(zachary)
calculate_centralities(zachary, include = "Degree Centrality")
pr_cent <- proper_centralities(zachary)

calc_cent <- calculate_centralities(zachary, include  = pr_cent[1:10])
pca_centralities( calc_cent )
pca_centralities( calc_cent , scale.unit = FALSE )
tsne_centralities( calc_cent, dims = 2, perplexity = 1, scale=TRUE)

visualize_graph( zachary , centrality.type="Degree Centrality")
visualize_heatmap( calc_cent , scale = TRUE  )
visualize_correlations(calc_cent,"pearson")
visualize_dendrogram(zachary, k=4)

subgraph_cent <- calc_cent[[1]]
Topological_coef <- calc_cent[[2]]

visualize_association(  subgraph_cent , Topological_coef)
visualize_pair_correlation( subgraph_cent , Topological_coef)
