library(seqinr)
library(ape)
k = 24

perBaseRates = c(0.1/12000, 0.2/12000, 0.5/12000, 1/12000, 2/12000,0.05/(12000), 
                 0.15/(12000), 0.25/(12000), 0.75/(12000), 1.5/(12000), 3/(12000))

source("code/mutations_function.R")
set.seed(758)
#noCases = 150
percentCases = c(0.01, 0.05, 0.1, 0.2)
SI_meanlog <- 2.96
SI_sdlog <-  0.82
branches = c(71,83,124,156,166,179,191,269,271)

#for(k in seed){
  for(perBaseRate in perBaseRates){
    
    
    for(g in branches){
      load(paste0("input/EF_simulations/seed",k,".Rdata"))
      outMatrix = as.data.frame(out$caseMat)
      outMatrix = outMatrix[order(outMatrix$caseID),]
      initialInfections = outMatrix$caseID[outMatrix$parentID == 0]
      
      previousGen = g
      descendents = c(previousGen)
      
      offspring = 1 
      while(offspring > 0){
        previousGen = outMatrix$caseID[outMatrix$parentID %in% previousGen]
        descendents = append(descendents, previousGen)
        offspring = length(previousGen)
      }
      
      outMatrix = outMatrix[outMatrix$caseID %in% descendents,]
      
      timelim = c(0, max(outMatrix$infD))
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
      
      tips = outMatrix
      for(noCases in percentCases*nrow(outMatrix)){
        sampledtips = tips[tips$caseID %in% sample(tips$caseID, noCases),]
        write.csv(sampledtips, paste0("output/simulation/simsampledtips_perGen_",k,"_",perBaseRate,"_",g,"_",noCases,".csv"))
        write.fasta(as.list(sampledtips$sequence), paste(sampledtips$caseID, sampledtips$infD, sep = "_"), 
                    file.out = paste0("output/simulation/simsampledtips_perGen_",k,"_",perBaseRate,"_",g,"_",noCases,".fasta"), 
                    open = "w", nbchar = 60, as.string = F)
        
   #     all.fasta=read.dna(paste0("output/simulation/simsampledtips_perGen_",k,"_",perBaseRate,"_",noCases,".fasta"), format = 'fasta')
  #      alnDist <- dist.dna(all.fasta, model = "raw", as.matrix = TRUE, pairwise.deletion = T)
        #write.csv(alnDist, paste0("output/simulation/simsampledsnpdists_perGen_",k,"_",perBaseRate,"_",noCases,".csv"))
        
      }
     
      to = as.character(outMatrix$caseID)
      from = as.character(outMatrix$parentID)
      TimeDiff = outMatrix$timeDiff
      allCases= as.data.frame(cbind(from,to,TimeDiff))
      write.csv(allCases, paste0("output/simulation/simFullCases_perGen_",k,"_",perBaseRate,"_",g,".csv"))
      
      
    }
   
  }

#}


