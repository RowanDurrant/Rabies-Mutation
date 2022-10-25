alnDist = read.csv("output/pemba/snpdistancesall.csv")
rownames(alnDist) <- alnDist[,1]
alnDist[,1] <- NULL
colnames(alnDist) = rownames(alnDist)


clusters = read.csv("input/test_seq_assignment.csv")
treeYearsUnpaired = read.csv("output/pemba/treeYearsUnpaired.csv")
treeYearsUnpaired = treeYearsUnpaired[,2:ncol(treeYearsUnpaired)]
tips = unique(treeYearsUnpaired$Desc[nchar(treeYearsUnpaired$Desc)>5])

#make network
g = graph.data.frame(treeYearsUnpaired, directed=F)
E(g)$length = treeYearsUnpaired$Branch.Length

fullBootstraps =  setNames(data.frame(matrix(ncol = 7, nrow = 0)), 
                           c("Tip1", "Tip2", "TimeDiff","gens", "snpsPerGen", 
                             "clusterMatch", "snpDist"))

for(j in 1:length(tips)){
  for(k in 1:length(tips)){
    if(k > j){
      route = get.shortest.paths(g, tips[j], tips[k], output="epath")$epath[[1]] #path between two tips
      timeDiff = sum(E(g)$length[route]) #time diff between two tips
      
      fullBootstraps2 =  setNames(data.frame(matrix(ncol = 7, nrow = NoReps)), 
                                  c("Tip1", "Tip2", "TimeDiff","gens", "snpsPerGen", 
                                    "clusterMatch", "snpDist"))
      for(m in 1:NoReps){
        
        fullBootstraps2$Tip1[m] = tips[j]
        fullBootstraps2$Tip2[m] = tips[k]
        fullBootstraps2$TimeDiff[m] = timeDiff
        fullBootstraps2$gens[m] = gencalc(fullBootstraps2$TimeDiff[m]) #time -> gens prediction
        fullBootstraps2$snpDist[m] = alnDist[fullBootstraps2$Tip1[m], fullBootstraps2$Tip2[m]] * genomeLength
        fullBootstraps2$snpsPerGen[m] = fullBootstraps2$snpDist[m] / fullBootstraps2$gens[m]
        fullBootstraps2$clusterMatch[m] = clusters$lineage[clusters$ID == fullBootstraps2$Tip1[m]] == clusters$lineage[clusters$ID == fullBootstraps2$Tip2[m]]
        
      }
      
      fullBootstraps = rbind(fullBootstraps, fullBootstraps2)
      
      print(j)
      
    }
    
  }
  
}
write.csv(fullBootstraps, paste0("output/full_bootstraps_",NoReps,".csv"))
