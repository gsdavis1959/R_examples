library(igraph)
library(edgebundleR)
library(dplyr)
library(purrr)
suppressPackageStartupMessages(library(widgetframe))
setwd('~/Data/Datasets')
parameters <- readLines("geom_aes.csv")
split_up <- function(x){
  params <- unlist(strsplit(x, ","))
  return(data.frame("from" = params[1], "to" = params[2:length(params)]))
}

relationship <- map_df(parameters, split_up)
head(relationship)

frameWidget(edgebundle(graph_from_data_frame(relationship), tension = 0.45, cutoff = 0.1, width = NULL, fontsize = 14,
                       padding = 110, nodesize = c(5, 20), directed = FALSE), height = "600")
