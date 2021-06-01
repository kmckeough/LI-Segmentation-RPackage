#' Create Graph of Spin States
#'
#' Graph of spin states
#'
#' @param z pixel assignments
#' @param p probability assigned to flipping node
#' @return spin states and adjacent pixels in graph form
#' @export
#'
#'
createGraph<-function(z,
                       p){
  #edges signify d=1 (connected), no edge means d=0
  #if item is not included in final graph object, then it is not connected to anything

  n.z<-nrow(z) # z must be square
  ref.array<-array(1:(n.z^2),dim=c(n.z,n.z))

  #first create dataframe listing the connections
  graph.df<-c()
  for(ii in 2:n.z){
    h.connect<-cbind(ref.array[,ii-1],ref.array[,ii])
    h.sim<-z[,ii-1]==z[,ii]
    h.keep<-h.sim*0
    h.keep[h.sim]<-sample(c(0,1),sum(h.sim),replace=T,prob=c(1-p,p))
    graph.df<-rbind(graph.df,h.connect[h.keep==1,])

    v.connect<-cbind(ref.array[ii-1,],ref.array[ii,])
    v.sim<-z[ii-1,]==z[ii,]
    v.keep<-v.sim*0
    v.keep[v.sim]<-sample(c(0,1),sum(v.sim),replace=T,prob=c(1-p,p))
    graph.df<-rbind(graph.df,v.connect[v.keep==1,])
  }

  graph<-graph_from_data_frame(graph.df,directed=FALSE)

}
