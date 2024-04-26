# files are in old repo

library(stringr)
perTimeRates = c(0.1/(12000*26.3), 0.2/(12000*26.3), 0.5/(12000*26.3), 1/(12000*26.3), 
                 2/(12000*26.3),0.05/(12000*26.3), 0.15/(12000*26.3), 0.25/(12000*26.3), 
                 0.75/(12000*26.3), 1.5/(12000*26.3), 3/(12000*26.3))
perBaseRates = c(0.1/12000, 0.2/12000, 0.5/12000, 1/12000, 2/12000,0.05/(12000), 
                 0.15/(12000), 0.25/(12000), 0.75/(12000), 1.5/(12000), 3/(12000))
branches = c(71,83,124,156,166,179,191,269,271)
startSeq = paste(rep("a",12000), collapse = "")

df = data.frame(Method = NA,
                 PercentSampled = NA,
                EquivalentPerGenRate = NA,
                 R_Squared = NA)

caseSizes = c(8.16,  40.80,  81.60, 163.20,5.68  ,28.40  ,56.80 ,113.60,124.34 , 621.70 ,1243.40 ,
              2486.80,15.65 , 78.25 ,156.50 ,313.00,38.39 ,191.95 ,383.90 ,767.80,
              5.33  ,26.65 , 53.30, 106.60,33.49 ,167.45 ,334.90 ,669.80,193.82  ,969.10 ,1938.20,
              3876.40,19.17,  95.85, 191.70 ,383.40)
vect001 = caseSizes[seq(1,length(caseSizes),4)]
vect005 = caseSizes[seq(2,length(caseSizes),4)]
vect01 = caseSizes[seq(3,length(caseSizes),4)]
vect02 = caseSizes[seq(4,length(caseSizes),4)]

for(noCases in caseSizes){
  for(g in branches){
    for(k in perBaseRates){
      if(file.exists(paste0("output/simulation/simsampledtips_perGen_24_",k,"_",g,"_",noCases,".csv"))){
        samples = read.csv(paste0("output/simulation/simsampledtips_perGen_24_",k,"_",g,"_",noCases,".csv"))
        time = rep(NA, nrow(samples))
        divergence = rep(NA, nrow(samples))
        for(i in 1:nrow(samples)){
          time[i] = samples$infD[i]
          divergence[i] = (nchar(startSeq) - str_count(samples$sequence[i], "a"))/nchar(startSeq)
          
        }
         ml = lm(divergence ~ 0 + time)
        r_squared = summary(ml)$r.squared
    
        df2 = data.frame(Method = "Generation",
                         PercentSampled = NA,
                         EquivalentPerGenRate = k*12000,
                         R_Squared = r_squared)
        if(noCases %in% vect001){df2$PercentSampled = 0.01}
        else if(noCases %in% vect005){df2$PercentSampled = 0.05}
        else if(noCases %in% vect01){df2$PercentSampled = 0.1}
        else if(noCases %in% vect02){df2$PercentSampled = 0.2}
        df = rbind(df, df2)
       }
    }
    for(j in perTimeRates){
        if(file.exists(paste0("output/simulation/simsampledtips_perfectClockRate_24_",j,"_",g,"_",noCases,".csv"))){
          samples = read.csv(paste0("output/simulation/simsampledtips_perfectClockRate_24_",j,"_",g,"_",noCases,".csv"))
          time = rep(NA, nrow(samples))
          divergence = rep(NA, nrow(samples))
          for(i in 1:nrow(samples)){
            time[i] = samples$infD[i]
            divergence[i] = (nchar(startSeq) - str_count(samples$sequence[i], "a"))/nchar(startSeq)
            
          }
          ml = lm(divergence ~ 0 + time)
          r_squared = summary(ml)$r.squared
      
          df2 = data.frame(Method = "Clock Rate",
                           PercentSampled = NA,
                           EquivalentPerGenRate = j*12000*26.3,
                           R_Squared = r_squared)
          if(noCases %in% vect001){df2$PercentSampled = 0.01}
          else if(noCases %in% vect005){df2$PercentSampled = 0.05}
          else if(noCases %in% vect01){df2$PercentSampled = 0.1}
          else if(noCases %in% vect02){df2$PercentSampled = 0.2}
          df = rbind(df, df2)
        }

      }
  }
}

write.csv(df, "output/simulation/clockrate-gen-model-comparison.csv")
