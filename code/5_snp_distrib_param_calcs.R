library(fitdistrplus)

tipDists = read.csv("output/pemba/full_bootstraps_100_2022.csv")
snps_predict = tipDists$snpsPerGen

runs = rep(NA, 1000)
snps_gamma = data.frame(shape = runs, rate = runs, aic = runs, LL = runs)
snps_lnorm = data.frame(meanlog = runs, sdlog = runs, aic = runs, LL = runs)

for (j in 1:length(runs)){ # for each run assign uncertainty to periods
  # Fit distributions
  snps_predict_gamma = fitdist(snps_predict[snps_predict>0], "gamma") 
  snps_gamma$shape[j] = snps_predict_gamma$estimate["shape"]
  snps_gamma$rate[j] = snps_predict_gamma$estimate["rate"]
  snps_gamma$aic[j] = snps_predict_gamma$aic
  snps_gamma$LL[j] = snps_predict_gamma$loglik
  
  snps_predict_lnorm = fitdist(snps_predict[snps_predict>0], "lnorm") 
  snps_lnorm$meanlog[j] = snps_predict_lnorm$estimate["meanlog"]
  snps_lnorm$sdlog[j] = snps_predict_lnorm$estimate["sdlog"]
  snps_lnorm$aic[j] = snps_predict_lnorm$aic
  snps_lnorm$LL[j] = snps_predict_lnorm$loglik
  print(j)
}

par(mfrow=c(1,3))
hist(snps_predict)
hist(snps_gamma$aic); hist(snps_lnorm$aic)
mean(snps_gamma$aic); mean(snps_lnorm$aic)
median(snps_gamma$aic); median(snps_lnorm$aic)
mean(snps_gamma$LL); mean(snps_lnorm$LL)

param <- MASS::fitdistr(snps_predict[snps_predict>0], "gamma", lower=c(0,0))
x <- seq(min(snps_predict), max(snps_predict), length.out = 1000)

par(mfrow = c(1,1))

hist(snps_predict, main = NA, xlab = "SNPs per generation (median gen prediction)", breaks = 50)
curve(dgamma(x, shape = 10.01941, rate = 58.80515), add = TRUE,
      col = "red", lty = "dotted")

hist(snps_predict, main = NA, xlab = "SNPs per generation (median gen prediction)", breaks = 50)
curve(dlnorm(x, meanlog =  -1.820404, sdlog = 0.3321436), add = TRUE,
      col = "red", lty = "dotted")
