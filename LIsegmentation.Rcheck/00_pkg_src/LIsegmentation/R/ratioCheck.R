#' Calculate Final Ration
#'
#' This function makes use of the Brobdingnag for adding exponants.
#' Even after the adjustment made to alleviate underflow error, there are still
#' issues calculating the ratio. Adding the probabilities as type brob
#' reduces underflow error even more.
#'
#' @param log1 the log posterior componants for pixel assignments 1
#' @param log2 the log posterior componants for pixel assignments 2
#' @return the weights and exponant components of the ratio
#' @export
#'
ratioCheck<-function(log1,log2){

  sum1<-0
  sum2<-0
  lmax<-max(log2)

  for(kk in sample(1:length(log1))){
    sum1<-sum1 + brob(log1[kk]-lmax)
    sum2<-sum2+ brob(log2[kk]-lmax)
  }

  R<-sum1/sum2

  return(R)

}
