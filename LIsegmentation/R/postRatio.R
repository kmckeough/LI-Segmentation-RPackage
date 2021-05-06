#' Calculate Log Posterior
#'
#' Calculates componants of the ratio to compare two posterior distriubtions
#' of the pixel assignments
#'
#' @param Z1 vector of matrix representation of top pixel assignments (-1,1)
#' @param Z2 vector of matrix representation of bottom pixel assignments (-1,1)
#' @param lambda posterior draws of LIRA output, one draw per row
#' @param param posterior draws of other parameters, one draw per row
#' @return the log posterior componants for the ratio calculation
#' @export
#'

postRatio<-function(Z1,Z2,lambda,param){

  n<-sqrt(length(Z1))
  Z1_array<-array(Z1,dim=c(n,n))
  Z2_array<-array(Z2,dim=c(n,n))

  ising1<-calcIsing(Z1_array)
  ising2<-calcIsing(Z2_array)

  log1<-c()
  log2<-c()

  for(ii in 1:nrow(param)){

    log1<-c(log1,-1*sum((1-Z1)/2*(log(sqrt(2*pi*param$sigma0[ii])) + (lambda[ii,] - param$tau0[ii])^2/(2*param$sigma0[ii])) +
                          (1+Z1)/2*(log(sqrt(2*pi*param$sigma1[ii]))+(lambda[ii,] - param$tau1[ii])^2/(2*param$sigma1[ii])))+
              param$beta[ii]*ising1)

    log2<-c(log2,-1*sum((1-Z2)/2*(log(sqrt(2*pi*param$sigma0[ii])) + (lambda[ii,] - param$tau0[ii])^2/(2*param$sigma0[ii])) +
                          (1+Z2)/2*(log(sqrt(2*pi*param$sigma1[ii]))+(lambda[ii,] - param$tau1[ii])^2/(2*param$sigma1[ii])))+
              param$beta[ii]*ising2)
  }

  return(cbind(log1,log2))

}
