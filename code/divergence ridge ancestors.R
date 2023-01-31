data = read.csv("output/simulation/simsampledtips_perGen_24_0.000166666666666667_124_621.7.csv")
library(stringr)
startSeq = paste(rep("a",12000), collapse = "")
data$divergence = NA
for(i in 1:nrow(data)){
  data$divergence[i] = (nchar(startSeq) - str_count(data$sequence[i], "a"))/nchar(startSeq)
  
}
data$rate = data$divergence/data$infD
plot(data$divergence ~ data$infD, col = ifelse(data$rate > 7.8e-06, "red", "black"))

ridgepoints = data[data$rate > 7.8e-06,]
dataFull = read.csv("../Rabies-Mutation-Rates/output/simulation/simFullCases_perGen_24_0.000166666666666667_124.csv")

highrateancestors = c()
for(j in 1:nrow(ridgepoints)){
  child = ridgepoints$caseID[j]
  parent = ridgepoints$parentID[j]
  
  while(parent != 0){
    highrateancestors = append(highrateancestors, parent)
    
    child = parent
    parent = dataFull$from[dataFull$to == child]
  }
  
}
tab = table(highrateancestors)
dataFull$highRateToOccurence = NA
for(k in 1:nrow(dataFull)){
  if(length(tab[names(tab) == dataFull$to[k]]) > 0){
    dataFull$highRateToOccurence[k] = tab[names(tab) == dataFull$to[k]]  
    
  }else{dataFull$highRateToOccurence[k] = 0}
  
  
}

mainpoints = data[data$rate < 7.8e-06,]
lowrateancestors = c()
for(j in 1:nrow(mainpoints)){
  child = mainpoints$caseID[j]
  parent = mainpoints$parentID[j]
  
  while(parent != 0){
    lowrateancestors = append(lowrateancestors, parent)
    
    child = parent
    parent = dataFull$from[dataFull$to == child]
  }
  
}
tab = table(lowrateancestors)
dataFull$lowRateToOccurence = NA
for(k in 1:nrow(dataFull)){
  if(length(tab[names(tab) == dataFull$to[k]]) > 0){
    dataFull$lowRateToOccurence[k] = tab[names(tab) == dataFull$to[k]]  
    
  }else{dataFull$lowRateToOccurence[k] = 0}
  
  
}

dataFull$lowRateProp = dataFull$lowRateToOccurence/nrow(mainpoints)
dataFull$highRateProp = dataFull$highRateToOccurence/nrow(ridgepoints)

plot(dataFull$highRateProp ~ dataFull$lowRateProp,
     xlab = "Proportion of lower divergence rate cases with this ancestor",
     ylab = "Proportion of high divergence rate cases with this ancestor",
     col = ifelse(dataFull$TimeDiff > mean(dataFull$TimeDiff), "red", "blue"),
     pch = 16)
legend(x = "bottomright", legend = c("above mean serial interval", "below mean serial interval"),
                                     fill = c("red", "blue"))
