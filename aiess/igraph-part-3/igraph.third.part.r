library(igraph)

# create necessary data structures from the first part of the tutorial

setwd('~/Projects/INF-3sem-tpd/aiess/igraph-part-3/')

nodes <- read.csv("d1-nodes.csv", header=T, as.is=T)
links <- read.csv("d1-edges.csv", header=T, as.is=T)

links <- aggregate(links[,3], links[,-3], sum)
links <- links[order(links$from, links$to),]

colnames(links)[4] <- "weight"
rownames(links) <- NULL

net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
net <- simplify(net, remove.multiple = F, remove.loops = T) 

# Hubs and authorities

# The hubs and authorities algorithm developed by Jon Kleinberg was initially used 
# to examine web pages. Hubs were expected to contain catalogues with a large number 
# of outgoing links; while authorities would get many incoming links from hubs, 
# presumably because of their high-quality relevant information. 
HITS <- hits_scores(net)

hub_scores <- HITS$hub
authority_scores <- HITS$authority

par(mfrow=c(1,2))

l <- layout_nicely(net)

plot(net, vertex.size=hub_scores*20, main="Hubs", edge.arrow.size=0.1, layout=l)
plot(net, vertex.size=authority_scores*20, main="Authorities", edge.arrow.size=0.1, layout=l)

dev.off()

# ================ 7. Distances and paths ================

# Average path length 
# The mean of the shortest distance between each pair of nodes in the network 
# (in both directions for directed graphs). 
mean_distance(net, directed=F)
mean_distance(net, directed=T)

# We can also find the length of all shortest paths in the graph:
distances(net) # with edge weights
distances(net, weights=NA) # ignore weights

# We can extract the distances to a node or set of nodes we are interested in.
# Here we will get the distance of every media from the New York Times.
dist.from.NYT <- distances(net, v=V(net)[media=="NY Times"], to=V(net), weights=NA)
dist.from.NYT

# Set colors to plot the distances:
oranges <- colorRampPalette(c("darkred", "gold"))
col <- oranges(max(dist.from.NYT)+1)
col <- col[dist.from.NYT+1]

plot(net, 
     vertex.color=col, 
     vertex.label=dist.from.NYT, 
     edge.arrow.size=.3, 
     vertex.label.color="white")

# We can also find the shortest path between specific nodes.
# Say here between MSNBC and the New York Post:
news.path <- shortest_paths(net, 
                            from = V(net)[media=="MSNBC"], 
                            to  = V(net)[media=="New York Post"],
                            output = "both") # both path nodes and edges
news.path

# Generate edge color variable to plot the path:
ecol <- rep("gray", ecount(net))
ecol[unlist(news.path$epath)] <- "orange"

# Generate edge width variable to plot the path:
ew <- rep(2, ecount(net))
ew[unlist(news.path$epath)] <- 4

# Generate node color variable to plot the path:
vcol <- rep("darkgrey", vcount(net))
vcol[unlist(news.path$vpath)] <- "gold"

plot(net, 
     vertex.color=vcol, 
     edge.color=ecol, 
     edge.width=ew, 
     edge.arrow.mode=0)


# Identify the edges going into or out of a vertex, for instance the WSJ.
# For a single node, use 'incident()', for multiple nodes use 'incident_edges()'
inc.edges <- incident(net, V(net)[media=="Wall Street Journal"], mode="all")

# Set colors to plot the selected edges.
ecol <- rep("gray", ecount(net))
ecol[inc.edges] <- "orange"

vcol <- rep("steelblue", vcount(net))
vcol[V(net)$media=="Wall Street Journal"] <- "gold"

plot(net, 
     vertex.color=vcol, 
     edge.color=ecol,
     edge.arrow.size=.3)


# We can also easily identify the immediate neighbors of a vertex, say WSJ.
# The 'neighbors' function finds all nodes one step out from the focal actor.
# To find the neighbors for multiple nodes, use 'adjacent_vertices()'.
# To find node neighborhoods going more than one step out, use function 'ego()'
# with parameter 'order' set to the number of steps out to go from the focal node(s).

neigh.nodes <- neighbors(net, V(net)[media=="Wall Street Journal"], mode="out")
neigh.nodes

# Set colors to plot the neighbors:
vcol[neigh.nodes] <- "darkorange"
plot(net, 
     vertex.color=vcol,
     edge.arrow.size=.3
     )

# Special operators for the indexing of edge sequences: %--%, %->%, %<-%
# E(network)[X %--% Y] selects edges between vertex sets X and Y, ignoring direction
# E(network)[X %->% Y] selects edges from vertex sets X to vertex set Y
# E(network)[X %<-% Y] selects edges from vertex sets Y to vertex set X

# For example, select edges from newspapers to online sources:
E(net)[ V(net)[type.label=="Newspaper"] %->% V(net)[type.label=="Online"] ]

# Cocitation (for a couple of nodes, how many shared nominations they have)
cocitation(net)

# ================ 8. Subgroups and communities ================

# Converting 'net' to an undirected network.
# There are several ways to do that: we can create an undirected link between any pair
# of connected nodes (mode="collapse), or create an undirected link for each directed
# one (mode="each"), or create an undirected link for each symmetric link (mode="mutual").
# In cases when A -> B and B -> A are collapsed into a single undirected link, we
# need to specify what to do with the edge attributes. Here we have said that
# the 'weight' of links should be summed, and all other edge attributes ignored.

net.sym <- as.undirected(net, mode="collapse", edge.attr.comb=list(weight="sum", "ignore"))


#  ------->> Cliques --------

# Find cliques (complete subgraphs of an undirected graph)
cliques(net.sym) # list of cliques       
sapply(cliques(net.sym), length) # clique sizes
largest_cliques(net.sym) # cliques with max number of nodes

vcol <- rep("grey", vcount(net.sym))
vcol[unlist(largest_cliques(net.sym))] <- "gold"

plot(net.sym, vertex.label=V(net.sym)$name, vertex.color=vcol)

#  ------->> Communities --------

# A number of algorithms aim to detect groups that consist of densely connected nodes
# with fewer connections across groups. 

# Community detection based on edge betweenness (Newman-Girvan)
# High-betweenness edges are removed sequentially (recalculating at each step)
# and the best partitioning of the network is selected.
ceb <- cluster_edge_betweenness(net) 
dendPlot(ceb, mode="hclust")
plot(ceb, net, edge.arrow.size=.3) 

# Let's examine the community detection igraph object:
class(ceb)
length(ceb)     # number of communities
membership(ceb) # community membership for each node
crossing(ceb, net)   # boolean vector: TRUE for edges across communities
modularity(ceb) # how modular the graph partitioning is

# High modularity for a partitioning reflects dense connections within communities 
# and sparse connections across communities.

# Community detection based on propagating labels
# Assigns node labels, randomizes, and replaces each vertex's label with
# the label that appears most frequently among neighbors. Repeated until
# each vertex has the most common label of its neighbors.
clp <- cluster_label_prop(net)
plot(clp, net, edge.arrow.size=.3)

# Community detection based on greedy optimization of modularity
cfg <- cluster_fast_greedy(as.undirected(net))
plot(cfg, as.undirected(net), edge.arrow.size=.3)
 
# We can also plot the communities without relying on their built-in plot:
V(net)$community <- cfg$membership
colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)
plot(net, vertex.color=colrs[V(net)$community], edge.arrow.size=.3)

# K-core decomposition
# The k-core is the maximal subgraph in which every node has degree of at least k
# This also means that the (k+1)-core will be a subgraph of the k-core.
# The result here gives the coreness of each vertex in the network.

kc <- coreness(net, mode="all")
plot(net, 
     vertex.size=kc*6, 
     vertex.label=kc, 
     vertex.color=colrs[kc],
     edge.arrow.size=.3)

# ================ 9. Assortativity and Homophily ================

# Assortativity (homophily)
# The tendency of nodes to connect to others who are similar on some variable.
# assortativity_nominal() is for categorical variables (labels)
# assortativity() is for ordinal and above variables
# assortativity_degree() checks assortativity in node degrees

V(net)$type.label
V(net)$media.type

assortativity_nominal(net, V(net)$media.type, directed=F)

assortativity(net, V(net)$audience.size, directed=F)

assortativity_degree(net, directed=F)

# ------->> EXERCISE <<------------------------------

# Prepare two plots of the GoT network using hubiness and authority
# as the metric used to scale the size of each node. 

# ------->> EXERCISE <<------------------------------

# Based on the GoT network, create two plots:
# - visualize the distance of each character from Tyrion
# - visualize the neighborhood of Aria

# ------->> EXERCISE <<------------------------------

# Plot the GoT network with communities marked. Compare the communities
# computed by the label propagation algorithm with the communities
# computed by the greedy modularity optimization algorithm.


