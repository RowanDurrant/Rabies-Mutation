library(igraph)
library(readr)
library(seqinr)
library(ape)
source("code/mutations_function.R")
source("code/generations_calculation_function.R") 

densities= read_csv("output/simulation/densitiesSimParams.csv")
densities = densities[,2:ncol(densities)]

noCases = 500
NoReps = 100
genomeLength = 12000
#perBaseRate = 0.2/12000
timelimPercent = c(0,20)

sample(1:50, 1) #28
seeds = 28

snpRate = c(1)

for(seed in seeds){
  for(perBaseRate in snpRate/genomeLength){
    source("code/2b_sim_output_prep.R")
  }
}

##MAKE TREES WITH SEQUENCE OUTPUT BEFORE PROCEEDING

for(seed in seeds[2:5]){
  for(perBaseRate in snpRate/genomeLength){
    source("code/prep_sim_trees.R")
  }
}

for(seed in seeds[2:5]){
  for(NoReps in c(100)){
    for(perBaseRate in snpRate/genomeLength){
      
      source("code/sim_calcs.R")
      source("code/compare_sim_gens.R")
    }
  }
}


accuracy =  setNames(data.frame(matrix(ncol = 9, nrow = 0)), 
                           c("NoReps","NoCases", "perBaseRate", "SNPRate", "predictedSNPs",
                             "predictedGens", "actualGens", "SNPsAccuracy", "GensAccuracy"))
for(seed in seeds){
  for(NoReps in c(100)){
    for(perBaseRate in snpRate/genomeLength){
      for(noCases in c(100)){
        accuracy2 =  setNames(data.frame(matrix(ncol = 9, nrow = 1)), 
                              c("NoReps","NoCases", "perBaseRate", "SNPRate", "predictedSNPs",
                                "predictedGens", "actualGens", "SNPsAccuracy", "GensAccuracy"))
        df = read.csv(paste0("output/simulation/simPredicts",seed,perBaseRate,noCases,NoReps,timelimPercent[1],timelimPercent[2],".csv"))
        accuracy2$NoReps = NoReps
        accuracy2$NoCases = noCases
        accuracy2$perBaseRate = perBaseRate
        accuracy2$SNPRate = perBaseRate * 12000
        accuracy2$predictedSNPs = mean(df$snpsPerGen)
        accuracy2$predictedGens = sum(df$gens)
        accuracy2$actualGens = sum(df$actualGens)
        accuracy2$SNPsAccuracy =100 - (abs(accuracy2$predictedSNPs - accuracy2$SNPRate) / accuracy2$SNPRate) * 100
        accuracy2$GensAccuracy = 100 - (abs(accuracy2$predictedGens - accuracy2$actualGens) / accuracy2$actualGens) * 100
        accuracy = rbind(accuracy, accuracy2)
        
      }
      
    }
  }
}

clockRateMethodAccuracy = read.csv("output/simulation/clockrate_method_accuracy.csv")

plot(data = accuracy, SNPsAccuracy ~ SNPRate, pch = 16, col = "blue", xlab = "Input SNPs/generation", 
     ylab = "% Accuracy (SNPs/gen)", ylim = c(70,100))
points(data = clockRateMethodAccuracy, SNPsAccuracy ~ SNPRate, pch = 16, col = "red")
legend("bottomright",                    # Add legend to plot
       legend = c("Generation method", "Clock rate method"),
       col = c("blue", "red"),
       pch = 16)

plot(data = accuracy, GensAccuracy ~ SNPRate, pch = 16 ,col = NoReps, xlab = "Input SNPs/generation", 
     ylab = "% Accuracy (Generations)")
legend("bottomright",                    # Add legend to plot
       legend = c("1 Rep", "10 Reps", "100 Reps"),
       col = c(1,10,100),
       pch = 16)

