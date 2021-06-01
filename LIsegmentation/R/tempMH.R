#' Draw Temperature Parameter (beta)
#'
#' Draws for the temperature parameter (beta) using Metropolis Hastings
#'
#' @param beta current beta step
#' @param z current spin state
#' @param G data frame with energy and density for partition
#' @param jump size of jump
#' @param n size of image
#' @param beta_a hyper prior for beta
#' @param beta_b hyper prior for beta
#' @param iter number of iterations
#' @return log of the partition function given a specific energy state and density
#' @export
#'


tempMH<-function(beta,
                 z,
                 G,
                 jump,
                 n,
                 beta_a,
                 beta_b,
                 iter){

  z_sum<-prodAdjacent(z)
  beta_mh<-rep(beta,iter)
  for(bb in 2:iter){
    a0<-beta^2/jump^2
    b0<-beta/jump^2
    beta_new<-rgamma(1,a0,b0)
    a_new<-beta_new^2/jump^2
    b_new<-beta_new/jump^2
    R<- (z_sum-beta_b)*(beta_new-beta)+ (beta_a-1)*(log(beta_new)-log(beta))+
      estPartition(G,beta,n)-estPartition(G,beta_new,n)+
      log(dgamma(beta,a_new,b_new))-log(dgamma(beta_new,a0,b0))
    log_U<-log(runif(1))
    if(log_U< min(0,R)){
      beta_mh[bb]<-beta_new
    }
  }


  return(beta_mh)
}
