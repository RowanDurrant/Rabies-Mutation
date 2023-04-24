library(igraph)
library(readr)
library(seqinr)
library(ape)
source("code/mutations_function.R")
source("code/generations_calculation_function.R") 

densities= read_csv("output/simulation/densitiesSimParams.csv")
densities = densities[,2:ncol(densities)]

noCases = 100
NoReps = 100
genomeLength = 12000
timelimPercent = c(0,100)

seeds = c(9,28,29,31,35)

snpRate = c(0.2,0.5,1,2,5)

for(seed in seeds){
  for(perBaseRate in snpRate/genomeLength){
    source("code/1_sim_output_prep.R")
  }
}

##MAKE TREES WITH SEQUENCE OUTPUT BEFORE PROCEEDING
#save tree csv in format "simSequences",seed,"_",noCases,"_",perBaseRate,"_strict.MCC.csv"

for(seed in seeds){
  for(perBaseRate in snpRate/genomeLength){
    source("code/2_prep_sim_trees.R")
  }
}

for(seed in seeds){
  for(NoReps in NoReps){
    for(perBaseRate in snpRate/genomeLength){
      
      source("code/3_compare_clockrate_gen_divergence_time.R")
    }
  }
}


