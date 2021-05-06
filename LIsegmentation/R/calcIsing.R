#' Calculate Ising Sum Component
#'
#' Helper function in calculating Ising probabilities. Calculates the sum of all
#' adjacent products of spins. (sum z_i_zj where i and j are adjacent)
#'
#' @param Z a matrix or array of pixel assignments (-1,1)
#' @return the total summation component of the Ising distribution
#' @export
#'
calcIsing<-function(Z){
  Nij<-Z*0
  n<-nrow(Z)

  # Corners
  Nij[1,1]<-sum(Z[1,1]*c(Z[1,2],Z[2,1]))
  Nij[1,n]<-sum(Z[1,n]*c(Z[1,n-1],Z[2,n]))
  Nij[n,1]<-sum(Z[n,1]*c(Z[n,2],Z[n-1,1]))
  Nij[n,n]<-sum(Z[n,n]*c(Z[n,n-1],Z[n-1,n]))

  # Edges
  Nij[2:(n-1),1]<-sapply(2:(n-1),function(x){sum(Z[x,1]*c(Z[x,2],Z[x-1,1],Z[x+1,1]))})
  Nij[2:(n-1),n]<-sapply(2:(n-1),function(x){sum(Z[x,n]*c(Z[x,n-1],Z[x-1,n],Z[x+1,n]))})
  Nij[1,2:(n-1)]<-sapply(2:(n-1),function(x){sum(Z[1,x]*c(Z[2,x],Z[1,x-1],Z[1,x+1]))})
  Nij[n,2:(n-1)]<-sapply(2:(n-1),function(x){sum(Z[n,x]*c(Z[n-1,x],Z[n,x-1],Z[n,x+1]))})

  # Middle
  for(ii in 2:(n-1)){
    Nij[2:(n-1),ii]<-sapply(2:(n-1),function(x){sum(Z[x,ii]*c(Z[x,ii+1],Z[x-1,ii],Z[x+1,ii],Z[x,ii-1]))})
  }


  return(sum(Nij))
}
