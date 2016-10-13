##This function calculates all the edges inter-community of each community in an igraph object.
#It implements a number for each algorithm. You can add another algorithms. The output is a table with the number of communities
#the number of nodes in each community and its inter-edges.
Inter.edges<-function(G,algorithm){
  ##Arg:
  ##G: an igraph's graph  
  ##algorithm: a numeric object that indicates the cluster algorithm to use
  ##You should keep in mind 
  #1=cluster_louvain() 
  #2=cluster_edge_betweenness()
  #3=cluster_walktrap()
  #4=cluster_label_prop()
  #5=cluster_fast_greedy()
  #6=cluster_infomap()
  if(algorithm==1){
    Modu<-cluster_louvain(G)}
  if(algorithm==2){
    Modu<-cluster_edge_betweenness(G)}
  if(algorithm==3){
    Modu<-cluster_walktrap(G)}
  if(algorithm==4){
    Modu<-cluster_label_prop(G)}
  if(algorithm==5){
    Modu<-cluster_fast_greedy(G)}
  if(algorithm==6){
    Modu<-cluster_infomap(G)}
  if(algorithm > 6){
    print("No valid number for cluster algorithm.")}
  ##Number of communities and its size
  Com<-as.data.frame(sizes(Modu))
  ##We get the community's membership vector
  NoCom<-as.vector(Com$Community.sizes)
  #Get the subgraphs of each community
  inter<-vector()
  for(i in 1:length(NoCom)){
    M<-which(membership(Modu)==i)
    sg<-induced.subgraph(G,M)
    i.ds<-sum(degree(sg))
    c.vds<-sum(degree(G,M))
    e.esum <-c.vds-i.ds
    #depu<-e.esum/2
    inter[i]<-e.esum
  }
  mods.ed<-data.frame(Com,vert)
  print(mods.ed)
}
