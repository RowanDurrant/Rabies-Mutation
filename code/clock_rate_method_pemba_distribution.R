clockRates = read.csv("output/pemba/treeYearsUnpairedClockRate.csv")

clockRates$Clock.Rate.Weighted = clockRates$Clock.Rate * clockRates$Branch.Length
meanClockRate = sum(clockRates$Clock.Rate.Weighted)/sum(clockRates$Branch.Length)

meanClockRate*12000*(26.3/365)

SI_meanlog <- 2.96
SI_sdlog <-  0.82

max_time = 1:1000

curve = c()
for(i in max_time){
  curve[i] = dlnorm(i, SI_meanlog, sdlog = SI_sdlog)
}

mutation_distr = c()
for(j in max_time){
  mutation_time = meanClockRate*12000*(j/365)
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

par(mfrow = c(1,1))

plot(1~1, ylim = c(0,10), xlim = c(0,0.6), col = "white",
     xlab = "SNPs per Generation", ylab = "Frequency")
curve(dlnorm(x, meanlog =  -1.820404, sdlog = 0.3321436), add = TRUE,
      col = "red")
curve(dgamma(x, shape = 10.01941, rate = 58.80515), add = TRUE,
      col = "blue")
legend(x = "topright",          # Position
       legend = c("Novel Method", "Clock Rate Method"),
       lty = c(1,1),
       col = c("blue", "red"))
