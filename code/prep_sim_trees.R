treeYears = read.csv(paste0("output/simulation/simSequences",seed,"_",noCases,"_",perBaseRate,"_strict.MCC.csv"))
#df with columns AncId, Desc1, Desc2, Branch.Length.1 & Branch.Length.2

treeYearsUnpaired = setNames(data.frame(matrix(ncol = 3, nrow = 0)), 
                             c("AncId", "Desc", "Branch.Length"))
for(i in 1:nrow(treeYears)){
  treeYearsUnpaired2 = setNames(data.frame(matrix(ncol = 3, nrow = 2)), 
                                c("AncId", "Desc", "Branch.Length"))
  #split into one tip per row
  treeYearsUnpaired2$AncId = as.character(treeYears$AncId[i])
  treeYearsUnpaired2$Desc[1] = as.character(treeYears$Desc1[i])
  treeYearsUnpaired2$Desc[2] = as.character(treeYears$Desc2[i])
  treeYearsUnpaired2$Branch.Length[1] = treeYears$Branch.Length.1[i]
  treeYearsUnpaired2$Branch.Length[2] = treeYears$Branch.Length.2[i]
  
  treeYearsUnpaired = rbind(treeYearsUnpaired, treeYearsUnpaired2)
}

#node names have spaces for some reason, get rid of those
treeYearsUnpaired$Desc = gsub(" ", "", treeYearsUnpaired$Desc, fixed = TRUE)

write.csv(treeYearsUnpaired, paste0("output/simulation/treeYearsUnpaired_",seed,"_",noCases,"_",perBaseRate*genomeLength,".csv"))
