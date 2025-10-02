get_maggi <- function(strategy,alpha=1,beta=1,decay=.9){ 
  # mode: (alphas-1) / (alphas+betas-2)
  # mean: alphas / (alphas+betas)

  # initialise empty arrays
  alphas <- rep(0,length(strategy))
  betas <- alphas
  s <- alphas
  f <- alphas 
  
  for (i in 1:length(strategy)){
    
    if(is.na(strategy[i])){ # if the first data point is an NA, then the 
      if(i==1){ # alphas and betas for that trial take on the default values 
        alphas[i] <- alpha  
        betas[i] <- beta
      }else{
        
        alphas[i] <- alphas[i-1] # else, if the data point is NA, take
        betas[i] <- betas[i-1] # the alpha or beta value from the previous
        s[i] <- s[i-1]
        f[i] <- f[i-1]
        
      } 
      
    }else{
      if(i==1){
        # on first trial, record whether 'success' or 'failure'
        s[i] <- strategy[i] 
        f[i] <- 1-strategy[i] 
      }else{
        # record success and failure, modified by success and failure at our last check
        # so, if not the first trial, decay the previously collected evidence -
        # this reflects the ability to forget
        s[i] <- decay*s[i-1] + strategy[i] 
        f[i] <- decay*f[i-1] + 1-strategy[i]
      }
      
      # update the distribution's parameters
      alphas[i] <- alpha+s[i] # why alpha, not alphas[i-1] here??
      betas[i] <- beta+f[i]
    } 

  }

  beta_map <- (alphas-1) / (alphas+betas-2) # maximum a posteriori probability i.e. the mode
  beta_variance <- alphas*betas / ((alphas+betas)^2 * (alphas+betas+1))
  
  return(list(alphas=alphas,betas=betas,beta_map=beta_map,beta_var=beta_variance,s=s,f=f))
}
