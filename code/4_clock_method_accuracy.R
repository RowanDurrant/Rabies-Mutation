seeds = c(9,28,29,31,35)
SNPRates = c(0.2,0.5,1,2,5)
perBaseRates = SNPRates/12000
library(data.table)

trueSNPRate = c()
predictedSNPRate = c()
accuracy = c()
clockRate = c()
for(j in seeds){
  for(i in perBaseRates){
    trueSNPRate = append(trueSNPRate, i*12000)
    clockRates = fread(file = paste0("output/simulation/tree building/simSequences",j,"_100_",i,"_strict.MCC.log"), select = "meanRate")$meanRate
    clockRates = clockRates[100:length(clockRates)] #burn in 
    clockRate = append(clockRate, mean(clockRates))
    SIs = rlnorm(length(clockRates), meanlog=2.96, sdlog=0.82)
    multipliedPosteriors = clockRates*SIs*12000
    predictedSNPRate = append(predictedSNPRate, mean(multipliedPosteriors))
  }
}
accuracy = log(predictedSNPRate/trueSNPRate)
df = as.data.frame(cbind(trueSNPRate, predictedSNPRate, accuracy))
write.csv(df, file = "output/simulation/clockrate_method_accuracy.csv")

df = read.csv("output/simulation/clockrate_method_accuracy.csv")

sqrt(mean(df$accuracy[df$trueSNPRate == 0.2]^2))
sqrt(mean(df$accuracy[df$trueSNPRate == 0.5]^2))
sqrt(mean(df$accuracy[df$trueSNPRate == 1]^2))
sqrt(mean(df$accuracy[df$trueSNPRate == 2]^2))
sqrt(mean(df$accuracy[df$trueSNPRate == 5]^2))
