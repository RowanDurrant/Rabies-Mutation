seeds = 24
perBaseRates = c(0.1/12000, 0.2/12000, 0.5/12000, 1/12000, 2/12000,0.05/(12000), 
                 0.15/(12000), 0.25/(12000), 0.75/(12000), 1.5/(12000), 3/(12000))
caseSizes = c(8.16,  40.80,  81.60, 163.20,5.68  ,28.40  ,56.80 ,113.60,124.34 , 621.70 ,1243.40 ,
              2486.80,15.65 , 78.25 ,156.50 ,313.00,38.39 ,191.95 ,383.90 ,767.80,
              5.33  ,26.65 , 53.30, 106.60,33.49 ,167.45 ,334.90 ,669.80,193.82  ,969.10 ,1938.20,
              3876.40,19.17,  95.85, 191.70 ,383.40)
vect001 = caseSizes[seq(1,length(caseSizes),4)]
vect005 = caseSizes[seq(2,length(caseSizes),4)]
vect01 = caseSizes[seq(3,length(caseSizes),4)]
vect02 = caseSizes[seq(4,length(caseSizes),4)]
branches = c(71,83,124,156,166,179,191,269,271)

library(data.table)

trueSNPRate = c()
predictedSNPRate = c()
accuracy = c()
clockRate = c()
percent_sampled = c()
no_cases = c()

for(j in seeds){
  for(i in perBaseRates){
    for(k in branches){
      for(l in caseSizes){
        if(file.exists(paste0("output/simulation/simsampledtips_perGen_",j,"_",i,"_",k,"_",l,".log"))){
          clockRates = fread(file = paste0("output/simulation/simsampledtips_perGen_",j,"_",i,"_",k,"_",l,".log"), select = "meanRate")$meanRate
          trueSNPRate = append(trueSNPRate, i*12000)
          
          if(l %in% vect001){percent_sampled = append(percent_sampled, 0.01)}
          else if(l %in% vect005){percent_sampled = append(percent_sampled, 0.05)}
          else if(l %in% vect01){percent_sampled = append(percent_sampled, 0.1)}
          else if(l %in% vect02){percent_sampled = append(percent_sampled, 0.2)}
          
          no_cases = append(no_cases, l)
          
          clockRates = clockRates[round(length(clockRates)*0.1):length(clockRates)] #10% burn in 
          clockRate = append(clockRate, mean(clockRates))
          SIs = rlnorm(length(clockRates), meanlog=2.96, sdlog=0.82)
          multipliedPosteriors = clockRates*SIs*12000
          predictedSNPRate = append(predictedSNPRate, mean(multipliedPosteriors))
        }
        
      }
    }
  }
}
accuracy = log(predictedSNPRate/trueSNPRate)
df = as.data.frame(cbind(trueSNPRate, predictedSNPRate, accuracy, clockRate, no_cases, percent_sampled))

sqrt(mean(df$accuracy[df$trueSNPRate == 0.2]^2))
sqrt(mean(df$accuracy[df$trueSNPRate == 0.5]^2))
sqrt(mean(df$accuracy[df$trueSNPRate == 1]^2))
sqrt(mean(df$accuracy[df$trueSNPRate == 2]^2))
sqrt(mean(df$accuracy[df$trueSNPRate == 5]^2))

mean(df$accuracy)
sqrt(mean(df$accuracy^2))

sqrt(mean(df$accuracy[df$no_cases < 50]^2))
sqrt(mean(df$accuracy[df$no_cases > 2000]^2))

write.csv(df, file = "output/simulation/clockrate_method_accuracy.csv")