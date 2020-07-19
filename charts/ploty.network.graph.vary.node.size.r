library(plotly)
library(igraph)
library(ITNr)

data("ELEnet16")

vs <- V(ELEnet16) #Get node list
vs
es <- as.data.frame(get.edgelist(ELEnet16)) # Get edgelist
es
node.data<-get.data.frame(ELEnet16,what="vertices") # Get node attribute dataframe
node.data

Nv <- length(vs) #number of nodes
Ne <- length(es[1]$V1) #number of edges

#Coordinates for nodes
L <- layout.fruchterman.reingold(ELEnet16)
Xn <- L[,1]
Yn <- L[,2]

#Creates the nodes (plots the points)
network <- plot_ly(x = ~Xn, y = ~Yn, #Node points
                   mode = "markers", 
                   text = paste(vs$name, '-', node.data$income, '-', node.data$GDPgrowth),
                   size = as.numeric(node.data$GDPgrowth),
                   hoverinfo = "text",
                   color =as.factor(node.data$region) )

#Create edges
edge_shapes <- list()
for(i in 1:Ne) {
  v0 <- es[i,]$V1
  v1 <- es[i,]$V2
  
  edge_shape = list(
    type = "line",
    line = list(color = "gray", width = 0.3),
    x0 = Xn[v0],
    y0 = Yn[v0],
    x1 = Xn[v1],
    y1 = Yn[v1]
  )
  
  edge_shapes[[i]] <- edge_shape
}

axis <- list(title = "", showgrid = FALSE, 
             showticklabels = FALSE, zeroline = FALSE)

p <- layout(
  network,
  title = 'Networks & Plotly',
  shapes = edge_shapes,
  xaxis = axis,
  yaxis = axis,
  showlegend=FALSE
)
p
