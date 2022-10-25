##create serial interval -> generations probability density matrix
library(fitdistrplus)

SI_meanlog <- 2.96
SI_sdlog <-  0.82

n_missing <- 1200 # number of cases not seen
n_sim <- 1000 # number of simulations to try this on....
max_time = 25000
# prep vector to store distribution params for the range of n_missing
SI_conv_meanlog <- numeric(n_missing)
SI_conv_sdlog <- numeric(n_missing)

for(i in 1:n_missing){
  # make a convolution of i serial intervals, replicate this n_sims times)
  SI_conv_sims = replicate(n_sim, sum(rlnorm(i, meanlog = SI_meanlog, sdlog = SI_sdlog)))
  SI_conv_fit <- fitdist(SI_conv_sims, "lnorm") # fit distribution to the new intervals
  SI_conv_meanlog[i] <- SI_conv_fit$estimate["meanlog"]
  SI_conv_sdlog[i] <- SI_conv_fit$estimate["sdlog"]
  
}


densities = setNames(data.frame(matrix(ncol = n_missing, nrow = max_time)), c("1":as.character(n_missing)))
for(i in 1:max_time){
  densities[i,] = dlnorm(i, SI_conv_meanlog, sdlog = SI_conv_sdlog)
}

write.csv(densities, "output/densitiesSimParams.csv")