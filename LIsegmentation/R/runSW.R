#' Run Swendsen Wang Iterations
#'
#' run SW
#'
#' @param lambda_draw single draw of LIRA posterior
#' @param tau0 mean of bkg (sqrt)
#' @param tau1 mean of source (sqrt)
#' @param sigma0_sq var of bkg (sqrt)
#' @param sigma1_sq var of source (sqrt)
#' @param beta temperature parameter for Ising
#' @param z_0 original spin state
#' @return draws of Z from SW step
#' @export
#'
#'


runSW<-function(lambda_draw,
                        tau0,
                        tau1,
                        beta,
                        sigma0_sq,
                        sigma1_sq,
                        z_0){
  #Gets a single draw from the posterior
  n.z<-nrow(z_0)		#size of image(side in pixels) img should be square
  #Calculate p
  p<-1-exp(-2*beta)
  if(is.na(p)){print(beta)}

  #Sample bonds:
  #d|z
  graph.bonds<-createGraph(z_0,p)

  #Sample spins
  #z|d
  z.cluster<-clusterGraph(z_0,lambda_draw,graph.bonds)


  #sample connected nodes
  if(sum(z.cluster$cluster!=0)>0){
    z.cluster_connected<-z.cluster[z.cluster$cluster!=0,]
    z.cluster_connected.split<-split(z.cluster_connected,z.cluster_connected$cluster)
    z.cluster_newspin<-unlist(llply(z.cluster_connected.split,sampleCluster,tau0=tau0,tau1=tau1,sigma0_sq=sigma0_sq,sigma1_sq=sigma1_sq))
    z.cluster_connected<-z.cluster_connected[order(z.cluster_connected$cluster),]
    z.cluster_connected$z<-z.cluster_newspin

  }else{
    z.cluster_connected<-c()
  }


  #sample unconnected nodes (z_ij=0)
  if(sum(z.cluster$cluster==0)>0){
    z.cluster_disconnected<-z.cluster[z.cluster$cluster==0,]
    z.cluster_disconnected$z<-apply(z.cluster_disconnected,1,sampleNode,tau0=tau0,tau1=tau1,sigma0_sq=sigma0_sq,sigma1_sq=sigma1_sq)
  }else{
    z.cluster_disconnected<-c()
  }

  # all pixel assignments are the same randomly change a disconnected pixel (this fix is often not relevant but will need some)
  # if((all(z.cluster_disconnected$cluster==-1) & all(z.cluster_connected$cluster==-1)) |
  #    (all(z.cluster_disconnected$cluster==1) & all(z.cluster_connected$cluster==1))){
  #
  #   if(sum(z.cluster$cluster==0)>0){
  #     z.cluster_disconnected$z[sample(1:nrow(z.cluster_disconnected),1)]<-z.cluster_disconnected$z[sample(1:nrow(z.cluster_disconnected),1)]*-1
  #   }else{
  #     #TBD
  #   }
  #
  # }

  #transform back into image:
  z.newspin<-rbind(z.cluster_connected,z.cluster_disconnected)
  z.newspin<-z.newspin[order(z.newspin$node),]$z
  if(length(unique(z.newspin))==1){
    index<-sample(1:length(z.newspin),1)
    z.newspin[index]<-z.newspin[index]*-1
    }
  z.new<-array(z.newspin,dim=c(n.z,n.z))


  return(z.new)

}
