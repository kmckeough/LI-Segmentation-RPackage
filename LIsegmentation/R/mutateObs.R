#' Genetic Algorithm -  Mutation
#'
#' Tournament selection. Randomly select two parents from the inital populatio
#' with replacement. Add the fittest to the selection population. Do this until
#' you have a specified number of parents in the selection population.
#'
#' @param offspring offspring to mutate
#' @param mutation type of mutation
#' @return the total summation component of the Ising distribution
#' @export
#'


mutateObs<-function(offspring,
                    mutation = 'adjacent'){

  n <- sqrt(length(offspring))


  if(mutation == 'random'){
    mutate<-sample(c(0:1),size=n^2,replace=TRUE,prob=c(1-1/n^2,1/n^2))
    offspring[mutate==1]<--1*sign(offspring[mutate==1])
  }else if(mutation =='adjacent'){
    sumAdj<- countNotAdjacent(array(offspring,dim=c(n,n)))
    mutate<-c(sumAdj)
    mutate[sumAdj>0]<- runif(sum(sumAdj>0)) <= 1/sum(sumAdj>0)
    mutate[sumAdj==0]<-runif(sum(sumAdj==0)) <= 1/(n^2)
    offspring[mutate==1]<--1*sign(offspring[mutate==1])
  }

  return(offspring)

}
