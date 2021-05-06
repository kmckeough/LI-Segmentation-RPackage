#' Calculate Posterior Ratio
#'
#' The final ratio between the posteriors of pixel assignments 1 and pixel
#' assignments 2. If the ratio is greater than1, than pixel assignment 1 is more
#' likely than pixel assingment 2.
#'
#' @param log1 the log posterior componants for pixel assignments 1
#' @param log2 the log powerior componants for pixel assignments 2
#' @return the weights and exponant components of the ratio
#' @export
#'
ratioCalc<-function(log1,log2){

  lmax<-max(log2)
  w<-exp(log2-lmax)/sum(exp(log2-lmax))
  expP<-exp(log1-log2)

  inf_val<-is.infinite(expP)

  return(cbind(w,expP))

}
