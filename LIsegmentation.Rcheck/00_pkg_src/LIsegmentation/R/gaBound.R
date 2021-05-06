#' Genetic Algorithm
#'
#' Run entire GA
#'
#' @param lambda lira draws
#' @param param parameter draws
#' @param draws draws of pixel assignments from ising gibbs
#' @param n_pop size of initial population
#' @param n_select number of parents selected in each set
#' @param max_iter maximum number of iterations
#' @param converge convergence level
#' @param crossover type of crossover
#' @param mutation type of mutation (adjacent)
#' @param numCores number of cores to run function in parallel
#' @param verbose verbose
#' @return list that contains (maxZ) the MAP via GA and (setZ) the final set of
#' pixel assignments the max was selected from (to check for convergence)
#' @export
#'
#'
gaBound<-function(lambda,
                  param,
                  draws,
                  n_pop=1000,
                  n_select=500,
                  max_iter=1000,
                  converge = 0.99,
                  crossover = 'uniform',
                  mutation = 'adjacent',
                  numCores = 1,
                  verbose=FALSE){

  registerDoMC(numCores)

  if(n_select%%2 !=0){return('Error: n_select must be even number')}

  n <-sqrt(ncol(lambda)) #size of image

  #Inital Population
  Nij<-apply(draws,1,function(Z){
    Zarray<-array(Z,dim=c(n,n))
    return(c(calcN(Zarray)))
  })
  Navg<-apply(t(Nij),2,mean)
  init_population <- initPop(Navg,n_pop)

  ii<-0
  matchp<-0
  while(ii<=max_iter & matchp<converge){
    if(ii%%100==0){print(ii)}
    # Select Population
    select_population <- selectPop(init_population = init_population,
                                   lambda = lambda,
                                   param = param,
                                   n_select = n_select)


    # Produce Offspring
    offspring_population <- reproducePop(select_population,
                                         crossover,
                                         mutation)

    init_population<-rbind(select_population,offspring_population)

    ii<-ii+1
    matchp<-sum(apply(init_population,2,function(x){
      return(length(unique(x))==1)
    }))/ncol(init_population)
  }

 maxZ<-selectMax(init_population,
                 verbose=verbose)

 if(ii>max_iter){print(paste('did not converge\nconverge rate:',matchp))}
 else{print(paste('converged in:',ii,'iterations'))}

 return(list(maxZ=maxZ,setZ=init_population))

}

