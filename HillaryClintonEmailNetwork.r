library(jsonlite)      # read in the JSON data from the API
library(dplyr)         # data munging
library(igraph)        # work with graphs in R
library(ggnetwork)     # devtools::install_github("briatte/ggnetwork")
library(intergraph)    # ggnetwork needs this to wield igraph things
library(ggrepel)       # fancy, non-ovelapping labels
library(svgPanZoom)    # zoom, zoom
library(SVGAnnotation) # to help svgPanZoom; it's a bioconductor package
library(DT)            # pretty tables

if (!file.exists("clinton_emails.rda")) {
  clinton_emails <- fromJSON("http://graphics.wsj.com/hillary-clinton-email-documents/api/search.php?subject=&text=&to=&from=&start=&end=&sort=docDate&order=desc&docid=&limit=27159&offset=0")$rows
  save(clinton_emails, file="clinton_emails.rda")
}

load("clinton_emails.rda")
clinton_emails %>% 
  mutate(from=trimws(from),
         to=trimws(to)) %>% 
  filter(from != "") %>% 
  filter(to != "") %>% 
  filter(!grepl(";", from)) %>% 
  filter(!grepl(";", to)) -> clinton_emails

gr <- graph_from_data_frame(clinton_emails[,c("from", "to")], directed=FALSE)

V(gr)$size <- centralization.degree(gr)$res

datatable(arrange(data_frame(person=V(gr)$name, centrality_degree=V(gr)$size), desc(centrality_degree)))
datatable(arrange(ungroup(count(clinton_emails, from, to)), desc(n)))

# weight connections
E(gr)$weight <- 1
g <- simplify(gr, edge.attr.comb="sum")

set.seed(1492)

dat <- ggnetwork(g, layout="fruchtermanreingold", arrow.gap=0, cell.jitter=0)

# graph
ggplot() +
  geom_edges(data=dat, 
             aes(x=x, y=y, xend=xend, yend=yend),
             color="grey50", curvature=0.1, size=0.15, alpha=1/2) +
  geom_nodes(data=dat,
             aes(x=x, y=y, xend=xend, yend=yend, size=sqrt(size)),
             alpha=1/3) +
  geom_label_repel(data=unique(dat[dat$size>50,c(1,2,5)]),
                   aes(x=x, y=y, label=vertex.names), 
                   size=2, color="#8856a7") +
  theme_blank() +
  theme(legend.position="none") -> gg
gg
svgPanZoom(svgPlot(show(gg), height=15, width=15), 
           width="960px",
           controlIconsEnabled=TRUE)
