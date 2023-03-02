tipDists = read.csv("output/pemba/full_bootstraps_100_2022.csv") #change if you changed no. reps
clusters = read.csv("input/Pemba_assignment.csv")
alnDist = read.csv("output/pemba/snpdistancesall.csv")
rownames(alnDist) <- alnDist[,1]
alnDist[,1] <- NULL
colnames(alnDist) = rownames(alnDist)
clusters = clusters[clusters$ID %in% rownames(alnDist),]
tipDists$TimeDiffYears = tipDists$TimeDiff / 365
tipDists$n = NA
lineages = unique(clusters$lineage)
lineageTipDists = head(tipDists, 0)

lineageMeanTime = c()
lineageMaxTime = c()
lineageMeanSNPs = c()

for(i in lineages){
  newLineage = tipDists[tipDists$Tip1 %in% clusters$ID[clusters$lineage == i] &
                          tipDists$Tip2 %in% clusters$ID[clusters$lineage == i],]
  if(nrow(newLineage) > 0){
      
  lineageMeanTime = append(lineageMeanTime, mean(newLineage$TimeDiffYears, na.rm = T))
  lineageMaxTime = append(lineageMaxTime, max(newLineage$TimeDiffYears, na.rm = T))
  lineageMeanSNPs = append(lineageMeanSNPs, mean(newLineage$snpsPerGen, na.rm = T))
    
  }

}

par(mfrow = c(1,2))
plot(lineageMeanSNPs ~ lineageMeanTime, xlab = "Mean Temporal Distance Between Tips (years)",
     ylab = "Mean SNPs/Gen Between Tips")
abline(lm(lineageMeanSNPs ~ lineageMeanTime), col = "red")
plot(lineageMeanSNPs ~ lineageMaxTime, xlab = "Max Temporal Distance Between Tips (years)",
     ylab = "Mean SNPs/Gen Between Tips")
abline(lm(lineageMeanSNPs ~ lineageMaxTime), col = "red")

a = lm(lineageMeanSNPs ~ lineageMeanTime)
b = lm(lineageMeanSNPs ~ lineageMaxTime)

summary(a)
summary(b)
