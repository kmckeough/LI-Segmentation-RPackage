#' Conjugate Prior Draw for sqrt Normal
#'
#'
#'
#' @param Z a matrix or array (pixel assignments, spin states)
#' @param lambda_draw single draw from LIRA
#' @param tau0 mean of bkg (sqrt)
#' @param tau1 mean of source (sqrt)
#' @param sigma0_sq var of bkg (sqrt)
#' @param sigma1_sq var of source (sqrt)
#' @param tau_mu hyperprior
#' @param sigma_df hyperprior
#' @param omega_sq hyperprior
#'
#' @return  a vector that is the product between all adjacent pairs of Z (no particular order)
#' @export
#'


paramDraw<-function(Z,
                     lambda_draw,
                     tau0,
                     tau1,
                     sigma0_sq,
                     sigma1_sq,
                     tau_mu,
                     sigma_df,
                     omega_sq){
  #Directly draw from the posterior

  lambda_draw<-c(lambda_draw)
  N<-length(lambda_draw)

  tau<-c()
  sigma<-c()
  for(zz in c(-1,1)){

    # Calculate relevant values
    n.z<-sum(z==zz)
    lambda.n.bar<-mean(lambda_draw[z==zz])
    s.n<-1/(n.z-1)*sum((lambda_draw[z==zz]-lambda.n.bar)^2)
    mu.n<-1/(n.z+1)*tau_mu + n.z/(1+n.z)*lambda.n.bar
    v.n<-sigma_df+n.z
    v.omega.n<-sigma_df*omega_sq + (n.z-1)*s.n+n.z/(1+n.z)*(lambda.n.bar-tau_mu)^2

    #Draw new parameters
    sigma.z<-v.omega.n/rchisq(1,v.n)
    tau.z<-rnorm(1,mu.n,sqrt(sigma.z/(n.z+1)))

    tau<-c(tau,tau.z)
    sigma<-c(sigma,sigma.z)

  }

  tau.max<-which.max(tau)

  new.param<-data.frame(tau1=tau[tau.max],tau0=tau[-tau.max],sigma0_sq=sigma[-tau.max],sigma1_sq=sigma[tau.max])
  return(new.param)
}
