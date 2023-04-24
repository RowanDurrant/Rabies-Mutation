load(paste0("input/EF_simulations/seed",seed,".Rdata"))
outMatrix = as.data.frame(out$caseMat)
outMatrix = outMatrix[order(outMatrix$caseID),]
initialInfections = outMatrix$caseID[outMatrix$parentID == 0]

SI_meanlog <- 2.96
SI_sdlog <-  0.82
timelim = (timelimPercent * max(outMatrix$infD))/100

for(i in initialInfections){
  outMatrix$sequence[outMatrix$caseID == i] = paste(rep("a",12000), collapse = "")
}

allCases = outMatrix$caseID[order(outMatrix$caseID)]

for(j in allCases){
  if(is.na(outMatrix$sequence[outMatrix$caseID == j]) == T){
    outMatrix$sequence[outMatrix$caseID == j] = viralSeq(outMatrix$sequence[outMatrix$caseID == outMatrix$parentID[outMatrix$caseID ==j]],
                                                         perBaseRate)
    print(j)

  }
  if(outMatrix$parentID[outMatrix$caseID == j] == 0){
    outMatrix$timeDiff[outMatrix$caseID == j] = rlnorm(1, meanlog = SI_meanlog, sdlog = SI_sdlog)
  }
  else{
    outMatrix$timeDiff[outMatrix$caseID == j] = outMatrix$infD[outMatrix$caseID == j] - outMatrix$infD[outMatrix$caseID == outMatrix$parentID[outMatrix$caseID ==j]]
  } 
}

tips = outMatrix[outMatrix$infD > timelim[1] & outMatrix$infD < timelim[2],]
sampledtips = tips[tips$caseID %in% sample(tips$caseID, noCases),]
write.csv(sampledtips, paste0("output/simulation/simsampledtips",seed,perBaseRate,noCases,timelimPercent[1],timelimPercent[2],".csv"))
write.fasta(as.list(sampledtips$sequence), paste(sampledtips$caseID, sampledtips$infD, sep = "_"), 
            file.out = paste0("output/simulation/simSequences",seed,perBaseRate,noCases,timelimPercent[1],timelimPercent[2],".fasta"), 
            open = "w", nbchar = 60, as.string = F)

all.fasta=read.dna(paste0("output/simulation/simSequences",seed,perBaseRate,noCases,timelimPercent[1],timelimPercent[2],".fasta"), format = 'fasta')
alnDist <- dist.dna(all.fasta, model = "raw", as.matrix = TRUE, pairwise.deletion = T)
write.csv(alnDist, paste0("output/simulation/simsampledsnpdists",seed,perBaseRate,noCases,timelimPercent[1],timelimPercent[2],".csv"))

to = as.character(outMatrix$caseID)
from = as.character(outMatrix$parentID)
TimeDiff = outMatrix$timeDiff
allCases= as.data.frame(cbind(from,to,TimeDiff))
write.csv(allCases, paste0("output/simulation/simFullCases",seed,perBaseRate,noCases,timelimPercent[1],timelimPercent[2],".csv"))

