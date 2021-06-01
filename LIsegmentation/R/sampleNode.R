#' Sample Nodes
#'
#' SW step
#'
#' @param z_node nodes of spin states
#' @param tau0 mean of bkg (sqrt)
#' @param tau1 mean of source (sqrt)
#' @param sigma0_sq var of bkg (sqrt)
#' @param sigma1_sq var of source (sqrt)
#' @return sample new spin states for each cluster
#' @export
#'
#'


sampleNode<-function(z_node,
                      tau0,
                      tau1,
                      sigma0_sq,
                      sigma1_sq){
  #output samples a new spin for an individual node
  #NOTE: the lambda value needs to by in the 4th position

  R.log<-h_z.param.log(-1, z_node[4],tau0,tau1,sigma0.sq,sigma1.sq) - h_z.param.log(1, z_node[4],tau0,tau1,sigma0_sq,sigma1_sq)
  R<-exp(R.log)
  p_plus<-exp(-log(1+R))
  new.spin<-sample(c(-1,1),1,prob=c(1-p_plus,p_plus))

  return(new.spin)
}
