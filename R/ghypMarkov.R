library(roxygen2)
library(devtools)

#' ghypMarkov
#'
#' Fit Hidden Markov Models under symmetric generalized hyperbolic distribution
#' @keywords Markov model
#' @param data data frame or matrix of multivariate series (eg, stock returns)
#' @param response equations for series in depmix() function
#' @param family distributions for depmix() function
#' @return list
#' @author Stephen Downing
#' @details This function fit Hidden Markov Models under symmetric generalized hyperbolic distribution by first fitting the 2-state Markov model assuming Gaussian distribution, then fitting the two state subsets of data to symmetric generalized hyberbolic distribution separately, finally recomputing posterior probabilities of hidden Markov State
#' @export
ghypMarkov <- function(data,
                       response,  #list of series in dataframe
                       family, #list of initial HMM distr used in depmix()
                       ...
) {
  library(depmixS4)
  library(ghyp)
  
  ## Markov multistep calculation assumes only 2 Markov states #########
  # 1. determine states from Gaussian
  msp <- depmix(data=data, response = response,family = family, nstates = 2, ...)
  #optimize parameters
  set.seed(1)
  fmsp <- fit(msp, ...) 
  
  # 2. fit ghypmv to only obs identified as state 1 (or 2)
  s1 <- which(posterior(fmsp)[,1]==1)
  s2 <- which(posterior(fmsp)[,1]==2)
  
  fitghs1 <- fit.ghypmv(data = data[s1,],symmetric = T,save.data = T,trace = T,silent = T, ...)
  
  lambda1 <- attributes(fitghs1)$lambda
  alpha.bar1 <- attributes(fitghs1)$alpha.bar
  mu1 <- attributes(fitghs1)$mu
  sigma1 <- attributes(fitghs1)$sigma
  gamma1 <- rep(0,dim(data)[2])
  
  fitghs2 <- fit.ghypmv(data = data[s2,],symmetric = T,save.data = T,trace = T,silent = T, ...)
  
  lambda2 <- attributes(fitghs2)$lambda
  alpha.bar2 <- attributes(fitghs2)$alpha.bar
  mu2 <- attributes(fitghs2)$mu
  sigma2 <- attributes(fitghs2)$sigma
  gamma2 <- rep(0,dim(data)[2])
  
  # 3. recalculate posterior probabilities with new ghypmv params
  p <- matrix(rep(NA,4),2)
  p[1,1:2] <- attributes(attributes(fmsp)$transition[[1]])$parameters$coefficients
  p[2,1:2]<- attributes(attributes(fmsp)$transition[[2]])$parameters$coefficients
  
  pi <- vector()
  pi[1] <- (1-p[2,2]) / (2 - p[1,1] - p[2,2])
  pi[2] <- (1-p[1,1]) / (2 - p[1,1] - p[2,2])
  
  ghyp1 <- ghyp(lambda = lambda1,
                alpha.bar = alpha.bar1,
                mu = mu1,
                sigma = sigma1,
                gamma = gamma1 )
  ghyp2 <- ghyp(lambda = lambda2,
                alpha.bar = alpha.bar2,
                mu = mu2,
                sigma = sigma2,
                gamma = gamma2)
  dgh1 <- dghyp(x = data[s1,], object = ghyp1)
  dgh2 <- dghyp(x = data[s2,], object = ghyp2)
  
  pstar1 <- c()
  pstar2 <- c()
  
  for (i in 1:dim(data)[1]) {
    if (i==1) {
      post1 <- (pi[1]*p[1,1]+pi[2]*p[2,1])*dghyp(x=data[1,],
                                                 object=ghyp1) 
      post2 <- (pi[1]*p[1,2]+pi[2]*p[2,2])*dghyp(x=data[1,],
                                                 object=ghyp2)
      pstar1[i] <- post1 / (post1 + post2)
      pstar2[i] <- post2 / (post1 + post2)
    } else {
      post1 <- (pstar1[i-1]*p[1,1]
                +pstar2[i-1]*p[2,1])*dghyp(x=data[i-1,],
                                           object=ghyp1) 
      post2 <- (pstar1[i-1]*p[1,2]
                +pstar2[i-1]*p[2,2])*dghyp(x=data[i-1,],
                                           object=ghyp2)
      pstar1[i] <- post1 / (post1 + post2)
      pstar2[i] <- post2 / (post1 + post2)
    }
  }
  psdf <- data.frame(pstar1=pstar1,pstar2=pstar2)
  psdf$state <- NA
  psdf$state[psdf$pstar2 >= .5] <- 2
  psdf$state[psdf$pstar1 >= .5] <- 1
  psdf <- psdf[,c(3,1,2)]  #reorder state var first
  
  return(list(ghyp.posterior=psdf,
              ghyp.state1=fitghs1,
              ghyp.state2=fitghs2,
              gaussian.posterior=posterior(fmsp),
              pi=pi,
              p=p) )
} # end function