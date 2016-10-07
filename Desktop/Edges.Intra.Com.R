##This function calculates all the edges of every community in an igraph object.
#It implements a number for each algorithm. You can add another algorithms. 


intra.edges<-function(G,algorithm) {
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
  if(algoritmo==1){
    Mod<-cluster_louvain(G)}
  if(algoritmo==2){
    Mod<-cluster_edge_betweenness(G)}
  if(algoritmo==3){
    Mod<-cluster_walktrap(G)}
  if(algorithm==4){
    Mod<-cluster_label_prop(G)}
  if(algorithm==5){
    Mod<-cluster_fast_greedy(G)}
  if(algorithm==6){
    Mod<-cluster_infomap(G)}
  if(algoritmo > 6){
    print("No valid number for cluster algorithm.")}
  
  ##Number of communities and its size
  Com<-as.data.frame(sizes(Mod))
  ##We get the community's membership vector
  NoCom<-as.vector(Com$Community.sizes)
  #Get the subgraphs of each community
  vert<-vector()
  for(i in 1:length(NoCom)){
    M<-which(membership(Mod)==i)
    sg<-induced.subgraph(G,M)
    c.ec<-ecount(sg) #cantidad de vinculos intra-comunidad
    vert[i]<-c.ec
  }
  intra<-data.frame(Com,vert)
  print(intra)
}

