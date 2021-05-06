#' Run Ising Gibbs Sampler
#'
#' Run the Ising Gibbs Sampler to get posterior distribution of pixel assignments
#'
#' @param Z1 vector of matrix representation of top pixel assignments (-1,1)
#' @return the log posterior componants for the ratio calculation
#' @export
#'
#'


runGibbs<-function(lira,
                     G,
                     init_iter = 500,
                     burn_iter = 50,
                     bet.jump = 0.01,
                     beta.a = 100,
                     beta.b = 1,
                     beta.start = 0.1,
                     beta.niter = 50,
                     tau.mu = 5,
                     sigma.df = 10,
                     omega.sq = 1,
                     init.seed = NA,
                     ncores = NA
){
  # Detect cores if not given
  if(is.na(ncores)){
    ncores<-1#detectCores(all.tests = FALSE, logical = TRUE)
  }
  registerDoMC(ncores)

  # If file given instead of array, read in file
  lira_array<-format_liraOut(lira)

  # Initialize
  print('Initialize MCMC Runs:')
  timestamp()
  init<-isingGibbs(lira_array[1,],Z=NA,iter=init_iter,
                  beta.jump=beta.jump,beta.a=beta.a,beta.b=beta.b,beta.start=beta.start,beta.niter = beta.niter,
                  tau.mu=tau.mu,sigma.df=sigma.df,omega.sq=omega.sq,G=G,
                  init.seed=init.seed)

  # Run for each lira output
  print('Running on LIRA iter:')
  timestamp()
  tau1<-init$tau1[init_iter]
  tau0<-init$tau0[init_iter]
  sigma0.sq<-init$sigma0.sq[init_iter]
  sigma1.sq<-init$sigma1.sq[init_iter]
  beta<-init$beta[init_iter]
  Z<-init$Z[init_iter,]
  ising_array<-foreach(i=1:nrow(lira_array),.combine=rbind) %dopar% {
    burn<-isingGibbs(lira_array[i,],Z=Z,iter=burn_iter,
                    tau1.start=tau1, tau0.start=tau0,
                    sigma1.sq.start=sigma1.sq, sigma0.sq.start=sigma0.sq,
                    beta.jump=beta.jump,beta.a=beta.a,beta.b=beta.b,beta.start=beta,beta.niter = beta.niter,
                    tau.mu=tau.mu,sigma.df=sigma.df,omega.sq=omega.sq,G=G,
                    init.seed=init.seed)

    c(burn$tau1[burn_iter],burn$tau0[burn_iter],
      burn$sigma0.sq[burn_iter],burn$sigma1.sq[burn_iter],
      burn$beta[burn_iter],burn$Z[burn_iter,])
  }
  param<-ising_array[,1:5]
  colnames(param)<-c('tau1','tau0','sigma0.sq','sigma1.sq','beta')
  z_array<-ising_array[,-c(1:5)]
  return(list(param = param,ising_array = z_array))

}
