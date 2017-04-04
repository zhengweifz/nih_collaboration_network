library(igraph)
library(sna)
library(intergraph)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(caret)

build_graph <- function (node_file, edge_file, targeted_edge_attr , targeted_node_attr, num_pub_floor = 0 ) {
  nodes = read.csv(node_file, header = T)
  edges = read.csv(edge_file, header = T)
  g = graph.empty(nrow(nodes), directed = FALSE)
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
  #g = filter_node_by_number_publication(g, num_pub_floor)
  return(g)
}

plot_graph <- function(g) {
  
  pdf_file = paste0(g$group, '.pdf')
  pdf(pdf_file, width = 8, height = 11)
  par(fin=c(6, 6))
  par(mar=c(1,1,1,1))
  
  plot(g, vertex.size=2, vertex.label=NA)
  #edge legend
  #legend(x=-1.2, y=-0.5, g$edge.color.lvls, pch=21,
        # col="#777777", pt.bg=g$edge.colpal, pt.cex=2, cex=.8, bty="n", ncol=1)
  dev.off()
}

plot_block_heatmap <- function(g) {
  # Create a character vector containing every node name
  # Re-generate dataframes for both nodes and edges, now containing
  # calculated network attributes
  node_list <- get.data.frame(g, what = "vertices")
  
  # Determine a community for each edge. If two nodes belong to the
  # same community, label the edge with that community. If not,
  # the edge community value is 'NA'
  edge_list <- get.data.frame(g, what = "edges") 
     # %>%inner_join(node_list %>% select(name, comm), by = c("from" = "name")) %>%
    #inner_join(node_list %>% select(name, comm), by = c("to" = "name")) %>%
    #mutate(group = ifelse(comm.x == comm.y, comm.x, NA) %>%  factor())
            
  all_nodes <- node_list$name
  
  # Adjust the 'to' and 'from' factor levels so they are equal
  # to this complete list of node names
  plot_data <- edge_list %>% mutate(
    to = factor(to, levels = all_nodes),
    from = factor(from, levels = all_nodes))
  pdf_file = paste0('adjancy_plot', '.pdf')
  pdf(pdf_file, width = 8, height = 11)
  par(fin=c(6, 6))
  par(mar=c(1,1,1,1))
  # Create the adjacency matrix plot
  ggplot(plot_data, aes(x = from, y = to, fill = group)) +
    geom_raster() +
    theme_bw() +
    # Because we need the x and y axis to display every node,
    # not just the nodes that have connections to each other,
    # make sure that ggplot does not drop unused factor levels
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(drop = FALSE) +
    theme(
      # Rotate the x-axis lables so they are legible
      axis.text.x = element_text(angle = 270, hjust = 0),
      # Force the plot into a square aspect ratio
      aspect.ratio = 1,
      # Hide the legend (optional)
      legend.position = "none")
  dev.off()
}
get_assortativity <- function(g) {
  cat = igraph::get.vertex.attribute(g, g$targeted_node_attr )
  #!! have to convert vertex values from chartacter to factor
  g$assortativity = assortativity_nominal(g, as.factor(cat), directed = F)
  return(g$assortativity);
}

get_group_confusion <- function(nodes, edges) {
  from_to = select(edges, Source, Target)
  from_to$Source = as.character(from_to$Source)
  from_to$Target = as.character(from_to$Target)
  nodes$Id = as.character(nodes$Id)
  from_to = left_join(from_to, nodes, by= c('Source' = 'Id'))
  from_to = left_join(from_to, nodes, by= c('Target' = 'Id'))
  fg_tg = select(from_to, Group.x, Group.y)
  colnames(fg_tg) = c('Source_Groups', 'Target_Groups')
  Source_Groups = as.factor(fg_tg$Source_Groups);
  Target_Groups = as.factor(fg_tg$Target_Groups);
  tbl = table(Source_Groups, Target_Groups)
  prop_table = prop.table(tbl)
  print(prop_table)
  #conf = confusionMatrix(tbl)
}




analyze_graph <- function(g, nodes, edges) {
  # Q1 Does NIGMS researchers collaborate with each other more ofthen than with other groups
  # assortativity
  assortativity = get_assortativity(g)
  print(paste0('assortativity: ', as.character(assortativity)))
  #confusion matrix
  get_group_confusion(nodes, edges)
  
  #calcuate graph level statistics
  
  
  #group blocks
  
  
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
main <- function(group, targeted_edge_attr, targeted_node_attr ) {
  
  if (group == 'study') {
    ## study file
    node_file = '../data/study_nodes_gephi.csv'
    edge_file = '../data/study_edges.csv'
  } else if (group == 'comp' ) {
    ## comp file
    node_file = '../data/comp_nodes_gephi.csv'
    edge_file = '../data/comp_edges.csv'
    
  } else if (group == 'study_comp') {
    ## comp file
    node_file = '../data/study_comp_nodes_gephi.csv'
    edge_file = '../data/study_comp_edges.csv'
  } else if (group == 'gt_10') {
    node_file = '../data/author_has_pub_gt_10_nodes_gephi.csv'
    edge_file = '../data/author_other_has_pub_gt_10_edge.csv'
  } else {
    node_file = '../data/author_has_pub_gt_5_nodes_gephi.csv'
    edge_file = '../data/author_other_has_pub_gt_5_edge.csv'
  }
 

  ## study and cob

  g <- build_graph(node_file, edge_file,  targeted_edge_attr = targeted_edge_attr,  targeted_node_attr = targeted_node_attr )
  g$group = group
  
  analyze_graph(g)
  plot_graph(g)
 
  #whole network 139651 nodes , 2634331 edges
  #filter out degree 1 138775 nodes , 2633466 edges
  print(length(V(g)))
  print(length(E(g)))
  #plot_graph(g)
}

main('comp', targeted_edge_attr='ACTIVITY',  targeted_node_attr ='Group')

