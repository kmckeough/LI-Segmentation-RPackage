#' Cluster Spin States
#'
#' Use graph of spin states to create clusters
#'
#' @param z pixel assignments
#' @param lambda LIRA draw
#' @param graph graph of pixel assignments and connect state
#' @return the cluster assignments of the pixels as connected nodes
#' @export
#'
#'

clusterGraph<-function(z,
                       lambda,
                       graph){

  n.z<-nrow(z) # z must be square
  #convert image into df
  z.cluster<-data.frame(node=1:(n.z^2),cluster=rep(0,n.z^2),z=c(z),lambda=c(lambda))

  graph.comp<-components(graph)
  #connected nodes
  n.cluster<-graph.comp$no
  names.cluster<-as.numeric(names(graph.comp$membership))
  for(cc in 1:n.cluster){
    index.cluster<-which(z.cluster$node%in%names.cluster[graph.comp$membership==cc])
    z.cluster$cluster[index.cluster]<-cc
  }

  #unconnected nodes, remain as 0
  return(z.cluster)
}
