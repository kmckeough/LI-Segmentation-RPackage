#' Genetic Algorithm -  Selection
#'
#' Tournament selection. Randomly select two parents from the inital populatio
#' with replacement. Add the fittest to the selection population. Do this until
#' you have a specified number of parents in the selection population.
#'
#' @param init_population the current population from which to select from
#' @param lambda posterior multi-scale counts output from LIRA (one iteration per row)
#' @param param parameter iterations from ising (one iteration per row)
#' @param n_select the total number of selected parents
#' @return the total summation component of the Ising distribution
#' @export
#'


selectPop<-function(init_population,
                    lambda,
                    param,
                    n_select){
  set.seed(123)
  select_population<-foreach(i=1:n_select,.combine=rbind) %dopar% {

       select_i<-sample(1:nrow(init_population),size=2)

    Z1<-init_population[select_i[1],]
    Z2<-init_population[select_i[2],]

    logs<-postRatio(Z1,Z2,lambda,param)

    R<-ratioCheck(logs[,1],logs[,2])

    if(R>1){
      Z1
    }else{
      Z2
    }
  }

  return(select_population)
}
