#' Sample Cluster
#'
#' Gibbs sampler
#'
#' @param z_cluster clusters of spin states
#' @param tau0 mean of bkg (sqrt)
#' @param tau1 mean of source (sqrt)
#' @param sigma0_sq var of bkg (sqrt)
#' @param sigma1_sq var of source (sqrt)
#' @return sample new spin states for each cluster
#' @export
#'
#'


sampleCluster<-function(z_cluster,
                        tau0,
                        tau1,
                        sigma0_sq,
                        sigma1_sq){
  #output samples a vector of new spins for the cluster

  hz_plus.log<-sum(h_z.param.log(rep(1,nrow(z_cluster)), z_cluster$lambda,tau0,tau1,sigma0_sq,sigma1_sq))
  hz_minus.log<-sum(h_z.param.log(rep(-1,nrow(z_cluster)), z_cluster$lambda,tau0,tau1,sigma0_sq,sigma1_sq))

  R<-exp(hz_plus.log-hz_minus.log)
  if(R==Inf){p_plus<-1}else{p_plus<-R/(1+R)}

  new.spin<-sample(c(-1,1),1,prob=c(1-p_plus,p_plus))

  return(rep(new.spin,nrow(z.cluster)))

}
