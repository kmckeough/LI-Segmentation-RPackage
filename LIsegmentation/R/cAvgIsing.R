#' Cumulative Average Ising
#'
#' Calculates the proportion of iterations each pixel was assigned to +1
#'
#'
#' @param ising_iter the iterations from the ising gibbs sampler, one iteration per row
#' @param burn specified iterations to remove
#' @return a vector of the proportion of times the pixel is assigned to the ROI
#' @export
#'
#'
#

cAvgIsing<-function(ising_iter,burn=NA){
  n<-sqrt(ncol(ising_iter))

  # remove burn in iter
  if(!all(is.na(burn))){
    ising_iter<-ising_iter[-burn,]
  }

  ising_iter[ising_iter==-1]<-0

  # average pixel val
  avg<-apply(ising_iter,2,mean)


  return(array(avg,dim=c(n,n)))
}
