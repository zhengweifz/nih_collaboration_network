library(igraph)
library(sna)
library(intergraph)
library(RColorBrewer)

build_graph <- function (group, targeted_edge_attr , targeted_node_attr, num_pub_floor = 0 ) {
  node_file = paste0('../data/', group, '_nodes_gephi.csv')
  edge_file = paste0('../data/', group, '_edges.csv')
  nodes = read.csv(node_file, header = T)
  edges = read.csv(edge_file, header = T)
  g = graph.empty(nrow(nodes), directed = FALSE)
  g$group = group
  # covert R factor to character
  V(g)$name = as.character(nodes$Id)
  # a edge array: odd index -> source, even index -> target
  edge_list = as.vector(t(cbind(as.character(edges$Source), as.character(edges$Target))))
  g = add_edges(g, edge_list)
  g$targeted_edge_attr = targeted_edge_attr
  g$targeted_node_attr = targeted_node_attr
  E(g)$Label = as.character(edges$Label)
  E(g)$PUB_YEAR = as.character(edges$PUB_YEAR)
  E(g)$COUNTRY = as.character(edges$COUNTRY)
  E(g)$ACTIVITY = as.character(edges$ACTIVITY)
  V(g)$Group = as.character(nodes$Group)
    #add edge color attribute
  g$edge.color.lvls = levels(edges[[targeted_edge_attr]])
  edge_colpal = brewer.pal(length(g$edge.color.lvls), 'Set1')
  g$edge.colpal = edge_colpal
  E(g)$color = edge_colpal[edges[[targeted_edge_attr]]]
  #remove nodes by degree 
  g = filter_node_by_number_publication(g, num_pub_floor)
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
  #calcuate graph level statistics
  #assortativity
  cat = igraph::get.vertex.attribute(g, g$targeted_node_attr )
  g$assortativity = assortativity_nominal(g, cat, directed = F)
  #clustering analysis
  #sapply(cliques(g), length)
  #largest_cliques(g)
  #block modeling
  #block model does not support multiplex edges
  simple_g = simplify(g)
  sna_g = asNetwork(simple_g)
  eq <- equiv.clust(sna_g, mode="graph")
  b <- blockmodel(sna_g, eq, k=4)
}

filter_node_by_number_publication <- function(g, num_pub_floor){
  #remove node by numbers of publication associated wiht each author
  for (i in 1:length(V(g))) {
    v = V(g)[i]
    uniq_pub = unique(E(g)[ from(v)]$Label)
    num_pub = length(uniq_pub)
    if (num_pub < num_pub_floor) {
      V(g)[i]$remove = TRUE
    } else {
       V(g)[i]$remove = FALSE
    }
  }
  g = igraph::delete.vertices(g, V(g)[V(g)$remove])
  return(g)
}
main <- function() {
  group = 'all'
  num_pub_floor = 2
  targeted_edge_attr = 'ACTIVITY'
  targeted_node_attr = 'Group'
  g <- build_graph(group = group, targeted_edge_attr = targeted_edge_attr,  targeted_node_attr = targeted_node_attr, num_pub_floor = num_pub_floor )
  #whole network 139651 nodes , 2634331 edges
  #filter out degree 1 138775 nodes , 2633466 edges
  print(length(V(g)))
  print(length(E(g)))
  #plot_graph(g)
}

main()
