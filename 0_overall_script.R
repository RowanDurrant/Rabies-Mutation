library(igraph)
library(readr)
library(ape)
source("code/mutations_function.R")
source("code/generations_calculation_function.R") 

densities= read_csv("output/pemba/densities.csv")
densities = densities[,2:ncol(densities)]

NoReps = 100

start1 = Sys.time()
source("code/2a_prep_phylo_trees.R")
source("code/3_main_calculations.R")
end1 = Sys.time()
end1 - start1
