treeYearsUnpaired = read.csv(paste0("output/simulation/simsampledtips",seed,perBaseRate,noCases,timelimPercent[1],timelimPercent[2],".csv"))
treeYearsUnpaired = treeYearsUnpaired[,2:ncol(treeYearsUnpaired)]
tips = unique(treeYearsUnpaired$caseID)

allCases = read.csv(paste0("output/simulation/simFullCases",seed,perBaseRate,noCases,timelimPercent[1],timelimPercent[2],".csv"))
allCases = allCases[,2:ncol(allCases)]
allCases$from = as.character(allCases$from)
allCases$to = as.character(allCases$to)
#make network
g = graph.data.frame(allCases, directed=F)
E(g)$length = 1

actualGens = setNames(data.frame(matrix(ncol = length(tips), nrow = length(tips))), c(tips))
rownames(actualGens) = tips

for(i in tips){
  for(j in tips){
    route = get.shortest.paths(g, as.character(i), as.character(j), output="epath")$epath[[1]]
    actualGens[as.character(i),as.character(j)] = sum(E(g)$length[route])
  }
}

simGenPredicts = read.csv(paste0("output/simulation/full_bootstraps_sim_",seed,perBaseRate,noCases,NoReps,timelimPercent[1],timelimPercent[2],".csv"))

simGenPredicts$actualGens = NA
for(k in 1:nrow(simGenPredicts)){
  simGenPredicts$actualGens[k] = actualGens[as.character(strsplit(simGenPredicts$Tip1[k], "_")[[1]][1]),
                                            as.character(strsplit(simGenPredicts$Tip2[k], "_")[[1]][1])]
}

write.csv(simGenPredicts, paste0("output/simulation/simPredicts",seed,perBaseRate,noCases,NoReps,timelimPercent[1],timelimPercent[2],".csv"))


plot(data = simGenPredicts, gens~actualGens, pch = ".",
     xlab= "Actual Generations", ylab = "Median Predicted Generations")
abline(a=0, b=1, col="red")
