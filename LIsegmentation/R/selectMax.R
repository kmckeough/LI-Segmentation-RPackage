#' Genetic Algorithm -  Select MAx
#'
#' Select the maximum out of a set of pixel assignments
#'
#' @param init_population the population from which to select the maximum from
#' @param verbose if verbose is TRUE the current max will be printed
#' @return the total summation component of the Ising distribution
#' @export
#'


selectMax<-function(init_population,
                    verbose=FALSE){

  gen_converge_unique<-dplyr::distinct(as.data.frame(init_population))

  maxZ<-as.numeric(gen_converge_unique[1,])
  for(ii in 2:nrow(gen_converge_unique)){

    Z2<-as.numeric(gen_converge_unique[ii,])

    logs<-postRatio(maxZ,Z2,lambda,param)
    R<-ratioCheck(logs[,1],logs[,2])

    if(R<1){
      if(verbose){print(paste('switch',ii))}
      maxZ<-Z2
    }
  }

  return(maxZ)
}
