#' Genetic Algorithm -  Initial Population
#'
#' Create the initial population to begin the genetic algorithm.
#'
#' @param Navg the average pixel-wise neighbourhood statistic
#' @param n_pop the size of initial population for the GA
#' @return the total summation component of the Ising distribution
#' @export
#'
initPop<-function(Navg, n_pop){

  # order the neighbourhood statistic
  Norder<-order(Navg,decreasing=TRUE)
  n<-sqrt(length(Norder))

  skip<-floor(length(Norder)/n_pop)
  init_population<-array(-1,dim=c(n_pop,n^2))
  init_population[1,Norder[1:skip]]<-1
  init_index<-sample(seq(skip,n^2-skip,by=skip),n_pop,replace=FALSE)
  for(ii in 2:n_pop ){
    init_population[ii,]= init_population[ii-1,]
    init_population[ii,Norder[init_index[ii]:(init_index[ii]+(skip-1))]]<-1
  }

  return(init_population)
}
