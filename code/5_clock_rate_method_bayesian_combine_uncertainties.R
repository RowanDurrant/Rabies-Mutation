##Jonathan's function
logPost <- function(dat, psamps){
  d <- log(dat)
  sig <- sd(d)
  mu <- mean(d)
  n <- length(d)
  musamp <- rnorm(psamps, mu, sig/sqrt(n))
  sigsamp <- sig*rchisq(psamps, n)/n
  return(exp(musamp+sigsamp^2/2))
}

#---------------------------------------------------------------------------

library("plotrix")
set.seed(27354)

library(data.table)
clock_posterior = fread("input/pemba_m6_m7.log", select = "default.meanRate")$default.meanRate

si <- fread("input/serial_interval_clean_RD.csv", select = "x")$x

SI_posterior = logPost(si, 124299)



par(mfrow = c(1,2))
hist(SI_posterior)
hist(clock_posterior)

multiplied_posteriors = SI_posterior*clock_posterior*11923/365
quantile(multiplied_posteriors, c(.025, .975))

# 2.5%      97.5% 
# 0.1266889 0.2190537 

mean(multiplied_posteriors)
# 0.1712748

write.csv(multiplied_posteriors, "output/pemba/multiplied_posteriors.csv")
