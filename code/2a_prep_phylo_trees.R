
#alignment only- distance matrix based purely on genetic distance
all.fasta=read.dna("../Pemba/data/genetic/pemba_tz_n153.aln.fasta", format = 'fasta')
alnDist <- dist.dna(all.fasta, model = "raw", as.matrix = TRUE, pairwise.deletion = T)
write.csv(alnDist, "output/snpdistancesall.csv")

treeYears = read.csv("input/pemba.csv")
#df with columns AncId, Desc1, Desc2, Branch.Length.1 & Branch.Length.2

treeYears$Branch.Length.1 = treeYears$Branch.Length.1 * 365 #convert years to days
treeYears$Branch.Length.2 = treeYears$Branch.Length.2 * 365

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

write.csv(treeYearsUnpaired, "output/treeYearsUnpaired.csv")
