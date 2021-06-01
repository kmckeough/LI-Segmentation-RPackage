#' Run Ising Gibbs Sampler
#'
#' Ising Gibbs Sampler
#'
#' @param lambda_draw posterior multiscale counts from running LIRA, single draw
#' @param G energy information from Beale exact calculation
#' @param Z set initial draw for spin state, random assingments if NA
#' @param iter number of iterations
#' @param beta_jump jump for MH
#' @param beta_a prior parameter
#' @param beta_b prior parameter
#' @param beta_start initial value for beta parameter (keep close to 0)
#' @param beta_niter number of iterations for beta parameter draw
#' @param tau1_start initial value for tau1
#' @param tau0_start initial value for tau0
#' @param sigma1_sq_start initial value for sigma1
#' @param sigma0_sq_start initial value for sigma0
#' @param tau_mu prior parameter
#' @param sigma_df prior parameter
#' @param omega_sq prior parameter
#' @param init_seed set seed to replicate results, NA means no seed set
#' @return the log posterior componants for the ratio calculation
#' @export
#'
#'

runGibbs<-function(lambda_draw,
                   G,
                    Z=NA,
                    iter=1000,
                    beta_jump=0.01,
                    beta_a=100,
                    beta_b=1,
                    beta_start=0.1,
                    beta_niter = 20,
                    tau1_start=NA,
                    tau0_start=NA,
                    sigma1_sq_start=NA,
                    sigma0_sq_start=NA,
                    tau_mu=5,
                    sigma_df=10,
                    omega_sq=1,
                    init_seed=NA){

  n.sq<-length(lambda_draw)
  n<-sqrt(n.sq)

  if(any(is.na(Z))){
    Z<-array(sample(c(-1,1),iter*n.sq,replace=TRUE),dim=c(iter,n.sq))
  }else{
    Z<-array(Z,dim=c(iter,n.sq))
  }


  #Initialize parameter vectors
  if(!is.na(init_seed)){set.seed(init_seed)}

  beta<-rep(beta.start,iter)
  if(is.na(sigma1_sq_start)){
    sigma1_sq<-rep(sigma_df*omega_sq/rchisq(1,sigma_df),iter)
  }else{
    sigma1_sq<-rep(sigma1_sq_start,iter)
  }
  if(is.na(sigma0_sq_start)){
    sigma0_sq<-rep(sigma_df*omega_sq/rchisq(1,sigma_df),iter)
  }else{
    sigma0_sq<-rep(sigma0_sq_start,iter)
  }
  if(is.na(tau1_start)){
    tau1<-rep(rnorm(1,tau_mu,sqrt(sigma1_sq[1])),iter)
  }else{
    tau1<-rep(tau1_start,iter)
  }
  if(is.na(tau0_start)){
    tau0<-rep(rnorm(1,tau_mu,sqrt(sigma0_sq[1])),iter)
  }else{
    tau0<-rep(tau0_start,iter)
  }

  #timestamp()
  for(ii in 2:iter){
    #if(ii%%100 == 0 ){
    #timestamp()
    #print(ii)
    #}

    # Draw beta for Ising:
    beta_draw<-tempMH(beta = beta[ii-1],
                      z = array(Z[ii-1,],dim = c(n,n)),
                      G = G,
                      jump = beta_jump,
                      iter = beta_niter,
                      beta_a = beta_a,
                      beta_b = beta_b)

    beta[ii]<-beta_draw[beta_niter]

    # Draw for mean and sd parameters
    param <- paramDraw(z = Z[ii-1,],
                       lambda = lambda_draw,
                        tau0 = tau0[ii-1],
                       tau1 = tau1[ii-1],
                        sigma0_sq = sigma0_sq[ii-1],
                       sigma1_sq = sigma1_sq[ii-1],
                        mu0 = tau_mu,
                       sigma_df = sigma_df,
                       omega_sq = omega_sq)

    tau0[ii] <- param$tau0
    tau1[ii] <- param$tau1
    sigma0_sq[ii] <- param$sigma0_sq
    sigma1_sq[ii] <- param$sigma1_sq

    # Draw from SW
    z.img<-runSW(lambda = lambda_draw,
                         z_0 = array(Z[ii-1,],dim=c(n,n)),
                         tau0 = tau0[ii],
                         tau1 = tau1[ii],
                         sigma0_sq = sigma0_sq[ii],
                         sigma1_sq = sigma1_sq[ii],
                         beta = beta[ii])
    Z[ii,]<-c(z.img)
  }

  sims.list<-list(Z=Z,
                  tau0=tau0,tau1=tau1,
                  sigma0_sq=sigma0_sq,
                  sigma1_sq=sigma1_sq,
                  beta=beta)
  return(sims.list)

}
