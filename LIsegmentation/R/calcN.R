#' Calculate Neighborhood Values
#'
#' This function takes an image of pixel assignments and calculates the
#' neighborhood value for each pixel, average number of 1's in surrounding
#' neighborhood.If pixel value is 0, N_ij=0.
#'
#' @param Z a matrix or array of pixel assignments (-1,1)
#' @return A matrix of neighborhood values
#' @export
calcN<-function(Z){


  Nij<-Z*0
  n<-nrow(Z)
  Z[Z==-1]<-0

  # Corners
  Nij[1,1]<-mean(Z[1,1]*c(Z[1,2],Z[2,1]))
  Nij[1,n]<-mean(Z[1,n]*c(Z[1,n-1],Z[2,n]))
  Nij[n,1]<-mean(Z[n,1]*c(Z[n,2],Z[n-1,1]))
  Nij[n,n]<-mean(Z[n,n]*c(Z[n,n-1],Z[n-1,n]))

  # Edges
  Nij[2:(n-1),1]<-sapply(2:(n-1),function(x){mean(Z[x,1]*c(Z[x,2],Z[x-1,1],Z[x+1,1]))})
  Nij[2:(n-1),n]<-sapply(2:(n-1),function(x){mean(Z[x,n]*c(Z[x,n-1],Z[x-1,n],Z[x+1,n]))})
  Nij[1,2:(n-1)]<-sapply(2:(n-1),function(x){mean(Z[1,x]*c(Z[2,x],Z[1,x-1],Z[1,x+1]))})
  Nij[n,2:(n-1)]<-sapply(2:(n-1),function(x){mean(Z[n,x]*c(Z[n-1,x],Z[n,x-1],Z[n,x+1]))})

  # Middle
  for(ii in 2:(n-1)){
    Nij[2:(n-1),ii]<-sapply(2:(n-1),function(x){mean(Z[x,ii]*c(Z[x,ii+1],Z[x-1,ii],Z[x+1,ii],Z[x,ii-1]))})
  }


  return(Nij)
}
