#' Estimate Partition Function
#'
#' Calculate the log partition function given a specific beta parameter
#'
#' @param n size of image
#' @param G partition data frame containing the energy (U) and corresponding counts of each state (G)
#' @param beta beta parameter estimate
#' @return log of the partition function given a specific energy state and density
#' @export
#'
#'


estPartition<-function(G,
                       beta,
                       n){

  energy<-G$U
  Ge<-G$G

  part<-0
  part.max<-max(Ge)
  for(pp in 1:length(Ge)){
    part.e<-beta*energy[pp]+Ge[pp]-part.max
    #part<-part+exp(part.e)
    if((part+exp(part.e))==Inf){part<-add.bigz(part,exp(part.e))}
    else{part<-part+exp(part.e)}
  }

  part.log<-log(part)
  return(part.log)
}
