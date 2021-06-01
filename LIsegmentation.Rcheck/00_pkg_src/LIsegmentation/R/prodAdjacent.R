#' Product of Adjacent Pixels
#'
#' The sum of the product of adjacent pixels to use for the Ising function ()
#'
#' @param Z a matrix or array (pixel assignments, spin states)
#' @return  a vector that is the product between all adjacent pairs of Z (no particular order)
#' @export

prodAdjacent<-function(Z){

  n<-ncol(Z)

  z_prod<-c()
  for(ii in 2:n){
    z_prod<-c(z_prod,Z[ii-1,]*Z[ii,],Z[,ii-1]*Z[,ii])
  }
  return(sum(z_prod))
}
