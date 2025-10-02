# lydia barnes, may 2024
# compare output of maggi's python functions with our get_maggi.R

library(zeallot) #unpack/destructure with %<-%
source(file.path(getwd(), "src-learn", "get_maggi.R"))

set.seed(17)

# read synthesised data from file
data <- read.csv('src-learn/synthesised_data.csv')
data <- data[[2]]

# calculate recency-weighted probability of finding strategy s
c(alphas,betas,beta_map,beta_variance) %<-% get_maggi(data)

# compare to output from maggi's python code
results_py <- read.csv('src-learn/maggi_py_results.csv')

#===================================================================================
# view alphas and betas over time
plot(1:length(data),alphas,type="l",col="darkgreen")
points(1:length(data),betas,type="l",col="navy")
points(1:length(results_py$Alpha),results_py$Alpha,type="l",col="pink")
points(1:length(results_py$Beta),results_py$Beta,type="l",col="darkred")

# view precision (1/variance)
plot(1:length(beta_variance),1/beta_variance,type="l",col="navy")
points(1:length(results_py$Precision),results_py$Precision,type="l",col="darkorange")

# view MAP (maximum a posteriori probability, i.e. the mode)
plot(1:length(beta_map),beta_map,type="l",col="navy")
points(1:length(results_py$MAP),results_py$MAP,type="l",col="darkorange")

# view the beta distribution at the final trial
increments <- seq(0,1,by=.01)
plot(increments,dbeta(increments,alphas[length(data)],betas[length(data)]),type="l",col="darkgreen") 
points(increments,dbeta(increments,results_py$Alpha[length(results_py$Alpha)],results_py$Beta[length(results_py$Beta)]),type="l",col="orange")
