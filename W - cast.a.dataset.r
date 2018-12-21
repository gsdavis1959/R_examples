library(reshape)
genes = paste('MMP', sprintf("%04d",1:10), sep="")
data = expand.grid(gene=genes, condition=c('copper', 'cheetos', 'beer', 'pizza'))
data$value = rnorm(40)
data
cast(data, gene ~ condition)
