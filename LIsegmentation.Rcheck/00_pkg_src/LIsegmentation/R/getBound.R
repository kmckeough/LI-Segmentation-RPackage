#' Final Minimal Boundary Estimate
#'
#' Taking in the posterior draws of the pixel assignments, multiscale counts
#' (LIRA), and other parameters to calculate the final minimal boundary
#' estimate. Based on ad hoc set of pixel assignments created from the
#' neighborhood values.
#'
#' @param Ziter posterior draws of pixel assignments, one row per draw
#' @param lambda posterior draws of multiscale counts (LIRA), one row per draw
#' @param param posterior draws of parameters, one row per draw
#' @return A matrix of neighborhood values
#' @export
#'
getBound<-function(Ziter,lambda,param){


  n<-sqrt(ncol(Ziter))

  Nij<-t(apply(Ziter,1,function(zz){
    z_array<-array(zz,dim=c(n,n))
    return(c(calcN(z_array)))
  }))

  Navg<-apply(Nij,2,mean)
  Norder<-order(Navg,decreasing=TRUE)

  #refArray<-array(1:n^2,dim=c(n,n))
  ROI_base<-rep(-1,n^2)

  Znbhd<-foreach(i=1:n^2,.combine=rbind) %dopar% {
    ROI<-ROI_base
    ROI[Norder[1:i]]<-1
    ROI
  }

  Zset<-rbind(Ziter,Znbhd)

  Zmax<-adhocBound(Zset,lambda,param)

  return(Zmax)
}
