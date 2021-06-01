#' Swendsen Wang for Spins
#'
#' log value
#'
#' @param z pixel assignments
#' @param lambda draws from LIRA
#' @param tau0 mean bkg (sqrt normal)
#' @param tau1 mean source (sqrt normal)
#' @param sigma0_sq var bkg (sqrt normal)
#' @param sigma1_sq var source (sqrt normal)
#' @return H
#' @export
#'
#'



# Swendsen Wang for spins
hzParam<-function(z,
                  lambda,
                  tau0,
                  tau1,
                  sigma0_sq,
                  sigma1_sq){
  H<--(lambda-(1-z)/2*tau0-(1+z)/2*tau1)^2/(2*((1-z)/2*sigma0_sq+(1+z)/2*sigma1_sq))
  return(H)
}
