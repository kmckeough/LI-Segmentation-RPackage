#' Genetic Algorithm -  Selection
#'
#' Tournament selection. Randomly select two parents from the inital populatio
#' with replacement. Add the fittest to the selection population. Do this until
#' you have a specified number of parents in the selection population.
#'
#' @param select_population population to reproduce
#' @param crossover type of crossover
#' @param mutation type of mutation
#' @return the total summation component of the Ising distribution
#' @export
#'

reproducePop<-function(select_population,
                       crossover = 'uniform',
                       mutation = 'adjacent'){

  n_offspring<-nrow(select_population)

  n<-sqrt(ncol(select_population))

  offspring_population<-foreach(i=1:(n_offspring/2),.combine=rbind) %dopar% {
    parent1<-select_population[2*(i-1)+1,]
    parent2<-select_population[2*i,]


    if(crossover == 'uniform'){
    #uniform crossover
      crossover_points<-sample(1:n^2,n^2/2)

      offspring1<-parent1
      offspring1[crossover_points]<-parent2[crossover_points]
      offspring2<-parent2
      offspring2[crossover_points]<-parent1[crossover_points]

    }else if(crossover == 'single'){
    #single point crossover
      crossover_point<-sample(2:(n^2-1),1)
      offspring1<-c(parent2[1:crossover_point],
                      parent1[(crossover_point+1):n^2])
      offspring2<-c(parent1[1:crossover_point],
                      parent2[(crossover_point+1):n^2])
    }



    ## Mutation
    # offspring_population[(2*(ii-1)+1),]<-mutateObs(offspring1)
    # offspring_population[2*ii,]<-mutateObs(offspring2)

    rbind(mutateObs(offspring1,mutation=mutation),
    mutateObs(offspring2,mutation=mutation))
  }

  return(offspring_population)
}
