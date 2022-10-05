library(igraph)

g<-graph.formula(1-2,1-3,2-3,2-4,3-5,4-5,4-6,4-7,5-6,6-7)
plot(g)
V(g)
E(g)
vcount(g)
ecount(g)
dg<-graph.formula(1-+2,1-+3,2++3)
plot(dg)
degree(g)
degree(dg,mode = "in")
degree(dg,mode = "out")
V(dg)$name[degree(dg)==min(degree(dg))]
V(g)$name[degree(g)==min(degree(g))]
V(dg)$name[degree(dg)==max(degree(dg))]
V(g)$name[degree(g)==max(degree(g))]
get.adjedgelist(dg)
get.adjacency(g)
neighbors(g,5)

getwd()
setwd("D:\\Practicals\\SNA")
nodes<-read.csv("Dataset1-Media-Example-NODES.csv",header=T,as.is=T)
head(nodes)
links<-read.csv("Dataset1-Media-Example-EDGES.csv",header=T,as.is=T)
head(links)
net<-graph.data.frame(d=links,vertices=nodes,directed=T)
m<-as.matrix(net)
m
get.adjacency(net)
plot(net)
mydata<-read.table("SNAP2Text.txt")
mydata

#density
ecount(g)/(vcount(g)*(vcount(g)-1)) 
#degree
degree(net)
#reciprocity
reciprocity(dg) 
dyad.census(dg) 
2*dyad.census(dg)$mut/ecount(dg) 
#transitivity
kite <- graph.famous("Krackhardt_Kite")
atri <- adjacent.triangles(kite) 
plot(kite, vertex.label=atri)
transitivity(kite, type="local")
adjacent.triangles(kite) / (degree(kite) * (degree(kite)-1)/2) 
#centrality
centralization.degree(net, mode="in", normalized=T) 
#closeness centrailization
closeness(net, mode="all", weights=NA) 
centralization.closeness(net, mode="all", normalized=T)
#betweeness centrality
betweenness(net, directed=T, weights=NA) 
edge.betweenness(net, directed=T, weights=NA)
centralization.betweenness(net, directed=T, normalized=T) 
#eigenvector centrality
centralization.evcent(net, directed=T, normalized=T)
get.adjedgelist(kite, mode = c("all", "out", "in", "total"))
#joining 2 graphs
gh1<-barabasi.game(50,p=2,directed = F)
plot(gh1)
gh2<-watts.strogatz.game(1,size = 100,nei = 5,p=0.05)
plot(gh2)
gh<-graph.union(gh1,gh2)
gh<-simplify(gh)
plot(gh)
# Looking for communities using Grivan-Newman algorithm
ebc<-edge.betweenness.community(gh,directed = F)
ebc
mods<-sapply(0:ecount(gh), function(i){
  gh2<-delete_edges(gh,ebc$removed.edges[seq(length=i)])
  cl<-clusters(gh2)$membership
  modularity(gh,cl)
})
mods
plot(mods,pch=20)
gh2<- delete.edges(gh,ebc$removed.edges[seq(length=which.max(mods)-1)])
V(gh)$color=clusters(gh2)$membership
gh$layout<-layout.fruchterman.reingold
plot(gh,vertex.label=NA)
# communities using fast greedy
fc<-fastgreedy.community(gh)
fc

#shortest path from node to node
matt<-as.matrix(read.table("SNAP4aText.txt",header=T))
matt
nms<-matt[,1]
nms
matt<-matt[,-1]
matt
colnames(matt) <- rownames(matt) <- nms
matt[is.na(matt)]<-0
matt
g <- graph.adjacency(matt, weighted=TRUE)
plot(g)
s.paths <- shortest.paths(g, algorithm = "dijkstra")
print(s.paths)
shortest.paths(g, v="R", to="S")
plot(g, edge.label=E(g)$weight)

#density of graph
dg <- graph.formula(1-+2, 1-+3, 2++3)
plot(dg)
graph.density(dg, loops=TRUE)
graph.density(simplify(dg), loops=FALSE)
#egocentric network
egocentric <- graph.formula(1-+2,3-+2,3-+1,4-+2,1++4,4++6,6++2)
plot(egocentric)

#A network as a graph
ng<-graph.formula(Andy++Garth,Garth-+Bill,Bill-+Elena,Elena++Frank,Carol-+Andy,Carol-+Elena,Carol+
                    +Dan,Carol++Bill,Dan++Andy,Dan++Bill)
plot(ng)
#A network as a matrix
get.adjacency(ng)
#A network as an edge list
E(ng)
get.adjedgelist(ng,mode="in")

#equivalence
library(sna)
links2 <- read.csv("Dataset2-Media-User-Example-EDGES.csv", header=T, row.names=1)
eq<- equiv.clust(links2)
plot(eq)
#Structural Equivalence
g.se<-sedist(links2)
#metric MDS of vertex positions
plot(cmdscale(as.dist(g.se)))
#Blockmodeling
b<-blockmodel(links2,eq,h=10)
plot(b)

#hamming distance
library(e1071)
x<- c(0,0,0,0)
y<- c(0,1,0,1)
z<- c(1,0,1,1)
x<- c(0,1,1,1)

hamming.distance(x,y)

# SVD Singular value decomposition
a <- matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,0, 0, 0, 0, 1,
              1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1), 9, 4)
a
svd(a)

#bipartite network
davis <- read.csv(file.choose(), header=FALSE)
g <- graph.data.frame(davis,directed=FALSE)
plot(g)
bipartite.mapping(g)
V(g)$type <- bipartite_mapping(g)$type
plot(g)
plot(g, vertex.label.cex = 0.8, vertex.label.color = "black")
V(g)$color <- ifelse(V(g)$type, "lightblue", "salmon")
V(g)$shape <- ifelse(V(g)$type, "circle", "square")
E(g)$color <- "lightgray"
plot(g, vertex.label.cex = 0.8, vertex.label.color = "black")
V(g)$label.color <- "black" 
#label.cex value to adjust our label size
V(g)$label.cex <- 1 
V(g)$frame.color <-  "gray"
V(g)$size <- 18 
plot(g, layout = layout_with_graphopt)
plot(g, layout=layout.bipartite, vertex.size=7, vertex.label.cex=0.6)
