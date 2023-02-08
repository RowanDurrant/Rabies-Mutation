library(fitdistrplus)

meanClockRate = 1.903E-04

meanClockRate*11923*(17.3/365)

SI_meanlog <- 2.85
SI_sdlog <-  0.966

max_time = 1:1000

curve = c()
for(i in max_time){
  curve[i] = dlnorm(i, SI_meanlog, sdlog = SI_sdlog)
}

mutation_distr = c()
for(j in max_time){
  mutation_time = meanClockRate*11923*(j/365)
  weighted = rep(mutation_time, curve[j]/min(curve))
  mutation_distr = append(mutation_distr, weighted)
}

runs = rep(NA, 10)
snps_gamma = data.frame(shape = runs, rate = runs, aic = runs, LL = runs)
snps_lnorm = data.frame(meanlog = runs, sdlog = runs, aic = runs, LL = runs)

for (j in 1:length(runs)){ # for each run assign uncertainty to periods
  # Fit distributions
  snps_predict_gamma = fitdist(mutation_distr, "gamma") 
  snps_gamma$shape[j] = snps_predict_gamma$estimate["shape"]
  snps_gamma$rate[j] = snps_predict_gamma$estimate["rate"]
  snps_gamma$aic[j] = snps_predict_gamma$aic
  snps_gamma$LL[j] = snps_predict_gamma$loglik
  
  snps_predict_lnorm = fitdist(mutation_distr, "lnorm") 
  snps_lnorm$meanlog[j] = snps_predict_lnorm$estimate["meanlog"]
  snps_lnorm$sdlog[j] = snps_predict_lnorm$estimate["sdlog"]
  snps_lnorm$aic[j] = snps_predict_lnorm$aic
  snps_lnorm$LL[j] = snps_predict_lnorm$loglik
  print(j)
}


hist(snps_gamma$aic); hist(snps_lnorm$aic)
mean(snps_gamma$aic); mean(snps_lnorm$aic)
median(snps_gamma$aic); median(snps_lnorm$aic)
mean(snps_gamma$LL); mean(snps_lnorm$LL)

param <- MASS::fitdistr(mutation_distr, "lognormal", lower=c(0,0))
x <- seq(min(mutation_distr), max(mutation_distr), length.out = 1000)

