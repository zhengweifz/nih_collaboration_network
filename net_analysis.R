library(igraph)
library(sna)
library(RColorBrewer)

build_graph <- function (group, targeted_edge_attr , targeted_node_attr ) {
  node_file = paste0('../data/', group, '_nodes_gephi.csv')
  edge_file = paste0('../data/', group, '_edges.csv')
  nodes = read.csv(node_file, header = T)
  edges = read.csv(edge_file, header = T)
  g = graph.empty(nrow(nodes), directed = FALSE)
  g$group = group
  # covert R factor to character
  V(g)$name = as.character(nodes$Id)
  # a edge array: odd index -> source, even index -> target
  edge_list = as.vector(t(cbind(as.character(study_edges$Source), as.character(study_edges$Target))))
  g = add_edges(g, edge_list)
  E(g)$pmid = as.character(study_edges$Label)
  E(g)$year = as.character(study_edges$PUB_YEAR)
  E(g)$country = as.character(study_edges$COUNTRY)
    #add edge color attribute
  g$edge.color.lvls = levels(study_edges[[targeted_edge_attr]])
  edge_colpal = brewer.pal(length(g$edge.color.lvls), 'Set1')
  g$edge.colpal = edge_colpal
  E(g)$color = edge_colpal[study_edges[[targeted_edge_attr]]]
  return(g)
}

plot_graph <- function(g) {
  pdf_file = paste0(g$group, '.pdf')
  pdf(pdf_file, width = 8, height = 11)
  par(fin=c(6, 6))
  par(mar=c(1,1,1,1))
  
  plot(g, vertex.size=2, vertex.label=NA)
  #edge legend
  legend(x=-1.2, y=-0.5, g$edge.color.lvls, pch=21,
         col="#777777", pt.bg=g$edge.colpal, pt.cex=2, cex=.8, bty="n", ncol=1)
  dev.off()
}

analyze_grap <- function(g) {
  #associ
  
}

main <- function() {
  group = 'study'
  targeted_edge_attr = 'COUNTRY'
  targeted_node_attr = 'Group'
  g <- build_graph(group = group, targeted_edge_attr = targeted_edge_attr,  targeted_node_attr = targeted_node_attr )
  plot_graph(g)
}

main()
