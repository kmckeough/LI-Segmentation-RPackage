#' Run Ising Gibbs Sampler
#'
#' Run the entire Ising Gibbs Sampler to get posterior distribution of pixel assignments
#'
#' @param lambda posterior multiscale counts from running LIRA, one per row
#' @param G energy information from Beale exact calculation
#' @param init_iter number of iterations on inital run
#' @param burn_iter burn in for each iteration
#' @param beta_jump parameter
#' @param beta_a prior parameter
#' @param beta_b prior parameter
#' @param beta_start initial value for beta parameter (keep close to 0)
#' @param beta_niter number of iterations for beta parameter draw
#' @param tau_mu prior parameter
#' @param sigma_df prior parameter
#' @param omega_sq prior parameter
#' @param init_seed set seed to replicate results, NA means no seed set
#' @param ncores the number of cores , ''all' to use all cores in machine
#' @return the log posterior componants for the ratio calculation
#' @export
#'
#'




isingGibbs<-function(lambda,
                   G,
                   init_iter = 500,
                   burn_iter = 50,
                   beta_jump = 0.01,
                   beta_a = 100,
                   beta_b = 1,
                   beta_start = 0.1,
                   beta_niter = 50,
                   tau_mu = 5,
                   sigma_df = 10,
                   omega_sq = 1,
                   init_seed = NA,
                   ncores = 1
){

  #Detect cores if 'all' is chosen
  if(ncores=='all'){
    ncores<-detectCores(all.tests = FALSE, logical = TRUE)
  }else{
    ncores<-ncores
  }
  registerDoMC(ncores)

  # Initialize
  print('Initialize MCMC Runs:')
  timestamp()
  init<-runGibbs(lambda[1,],Z=NA,iter=init_iter,
                   beta_jump=beta_jump,
                   beta_a=beta_a,
                   beta_b=beta_b,
                   beta_start=beta_start,
                   beta_niter = beta_niter,
                   tau_mu=tau_mu,
                   sigma_df=sigma_df,
                   omega_sq=omega_sq,
                   G=G,
                   init_seed=init_seed)

  # Run for each lira output
  print('Running on LIRA iter:')
  timestamp()
  tau1<-init$tau1[init_iter]
  tau0<-init$tau0[init_iter]
  sigma0_sq<-init$sigma0_sq[init_iter]
  sigma1_sq<-init$sigma1_sq[init_iter]
  beta<-init$beta[init_iter]
  Z<-init$Z[init_iter,]
  ising_array<-foreach(i=1:nrow(lambda),.combine=rbind) %dopar% {

    burn<-runGibbs(lambda[i,],Z=Z,iter=burn_iter,
                     tau1.start=tau1, tau0.start=tau0,
                     sigma1_sq.start=sigma1_sq, sigma0_sq.start=sigma0_sq,
                     beta_jump=beta_jump,beta_a=beta_a,beta_b=beta_b,beta_start=beta,beta_niter = beta_niter,
                     tau_mu=tau_mu,sigma_df=sigma_df,omega_sq=omega_sq,G=G,
                     init_seed=init_seed)

    c(burn$tau1[burn_iter],burn$tau0[burn_iter],
      burn$sigma0_sq[burn_iter],burn$sigma1_sq[burn_iter],
      burn$beta[burn_iter],burn$Z[burn_iter,])
  }

  param<-ising_array[,1:5]
  colnames(param)<-c('tau1','tau0','sigma0_sq','sigma1_sq','beta')
  z_array<-ising_array[,-c(1:5)]

  return(list(param = param,ising_array = z_array))

}
