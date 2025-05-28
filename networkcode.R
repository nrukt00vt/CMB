library(igraph)
library(ggplot2)
#movemat is an adjacency matrix, (i,j) is the connectivity of i to j
movemat = matrix(c(1,.8,.1,0,
                   .9,1,0,.5,
                   .2,.4,.9,.6,
                   .1,.0,.6,.2),ncol=4)

image(movemat)
#construct network
gall<-graph.adjacency(as.matrix(movemat),weighted=T,mode="directed")


plot.igraph(gall)
degree(gall)
closeness(gall)
betweenness(gall)
evcent(gall)$vector

commall=cluster_walktrap(gall,weights=E(gall)$weight,merges=TRUE,modularity=TRUE) #this creates an object that stores the heirarchical community structure

commall$membership # returns a string of values that tells you community membership of each node (in this case, there are 4 nodes)
# if the row/column order corresponds with the rows in a shapefile or a data frame, you could just attach this onto the shapefile directly
# e.g. shapefile$community_membership = commall$membership
# (just make sure the rows correspond to the rows/columns in your adjacency matrix first)


cut_at(commall,no=2) # you can manually define the number of communities


#virginia data example
library(sf)
VA_shp=read_sf(dsn="VA_shapefile_wpop",
               layer="VA_shapefile_wpop",stringsAsFactors = F)

adjmat = read.csv("VAmatrix_average.csv",row.names=1,header=T, check.names = F)


g<-graph.adjacency(as.matrix(adjmat),weighted=T,mode="undirected")
g <- simplify(g)
plot.igraph(g)

comm_struct=cluster_walktrap(g,weights=E(g)$weight,merges=TRUE)

community_structure = data.frame(FIPS = as.numeric(comm_struct$names), community = comm_struct$membership)

VA_shp_with_comm = merge(VA_shp, community_structure, by.x = "STCOFIPS", by.y = "FIPS")

ggplot()+ 
  geom_sf(data=VA_shp_with_comm,aes(fill=as.factor(community)))+
  scale_fill_brewer(guide="none",palette="Set3") 
ggtitle("Community structure")+theme_bw()

eigenvector_centrality = data.frame(FIPS = names(evcent(g)$vector), evec = evcent(g)$vector)
VA_shp_with_evec = merge(VA_shp, eigenvector_centrality, by.x = "STCOFIPS", by.y = "FIPS")

ggplot()+ 
  geom_sf(data=VA_shp_with_evec,aes(fill=evec))+
  scale_fill_distiller(palette="YlOrRd" , direction = 1)  +
  ggtitle("Eigenvector centrality")+
  theme_bw()

#degree distribution
adjmat_degree = adjmat
diag(adjmat_degree) = 0

hist(colSums(adjmat))
VA_shp$degree = colSums(adjmat)
ggplot()+ 
  geom_sf(data=VA_shp,aes(fill=degree))+
  scale_fill_distiller(palette="YlOrRd" , direction = 1, trans = "log10")  +theme_bw()

